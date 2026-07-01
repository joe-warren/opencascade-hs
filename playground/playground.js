import { EditorView, basicSetup } from "https://esm.sh/codemirror@6.0.1";
import { StreamLanguage } from "https://esm.sh/@codemirror/language@6";
import { haskell } from "https://esm.sh/@codemirror/legacy-modes@6/mode/haskell";
import { oneDark } from "https://esm.sh/@codemirror/theme-one-dark@6";
import {
  ConsoleStdout, File, OpenFile, PreopenDirectory, WASI,
} from "https://esm.sh/gh/haskell-wasm/browser_wasi_shim";
import { DyLDBrowserHost, main } from "./dyld.mjs";

const statusEl = document.getElementById("status");
const setStatus = (msg) => { statusEl.textContent = msg; };

setStatus("Downloading rootfs...");
const rootfs = new PreopenDirectory("/", []);

const bsdtar_wasi = new WASI(
  ["bsdtar.wasm", "-x"], [],
  [
    new OpenFile(new File(new Uint8Array(), { readonly: true })),
    ConsoleStdout.lineBuffered((msg) => console.info(msg)),
    ConsoleStdout.lineBuffered((msg) => console.warn(msg)),
    rootfs,
  ],
  { debug: false }
);

// The initial editor program is loaded from the `program` query parameter
// (a relative or absolute URL), falling back to the bundled example.
const programUrl =
  new URLSearchParams(window.location.search).get("program") ?? "./example.hs";

// Assigned during init below; referenced by the event handlers. Kept at module
// scope (rather than inside the try) so the handlers can see them.
let dyld, runProgram, renderSolid;

try {
  const [{ instance }, rootfs_bytes, programResult] = await Promise.all([
    WebAssembly.instantiateStreaming(
      fetch("https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm"),
      { wasi_snapshot_preview1: bsdtar_wasi.wasiImport }
    ),
    fetch("./rootfs.tar.zst").then((r) => r.bytes()),
    // A failed program fetch shouldn't brick the playground, so capture the
    // error rather than rejecting the whole init; it's surfaced below.
    fetch(programUrl)
      .then(async (r) => {
        if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
        return { text: await r.text() };
      })
      .catch((e) => ({ error: e })),
  ]);

  setStatus("Extracting rootfs...");
  bsdtar_wasi.fds[0] = new OpenFile(
    new File(rootfs_bytes, { readonly: true })
  );
  bsdtar_wasi.start(instance);

  if (document.readyState === "loading") {
    await new Promise((res) =>
      document.addEventListener("DOMContentLoaded", res, { once: true })
    );
  }

  // Surface a program-load failure in the stderr console rather than silently
  // starting from a blank (or wrong) editor.
  if (programResult.error) {
    document.getElementById("stderr").value +=
      `Failed to load program from ${programUrl}: ${programResult.error.message}\n`;
  }

  window.editor = new EditorView({
    doc: programResult.text ?? "",
    parent: document.getElementById("editor"),
    extensions: [basicSetup, StreamLanguage.define(haskell), oneDark],
  });
  // Loading a program only touches the editor, so enable it before GHC is ready.
  document.getElementById("loadBtn").disabled = false;

  setStatus("Initialising GHC...");
  dyld = await main({
    rpc: new DyLDBrowserHost({
      rootfs,
      stdout: (msg) => {
        document.getElementById("stdout").value += `${msg}\n`;
      },
      stderr: (msg) => {
        document.getElementById("stderr").value += `${msg}\n`;
      },
    }),
    searchDirs: SEARCH_DIRS_JSON,
    mainSoPath: "/tmp/libplayground.so",
    args: ["libplayground.so", "+RTS", "-c", "-RTS"],
    isIserv: false,
  });

  // Grow memory to ensure enough heap space for OCCT allocations. After loading
  // all GHC modules the default memory is ~33MB, which is insufficient for
  // OCCT's Standard::Allocate; growing by 256MB fixes it.
  dyld.exportFuncs.memory.grow(4096);

  [runProgram, renderSolid] = await dyld.exportFuncs.myMain("GHC_LIBDIR", "PLAYGROUND_PKG_DBS");

  setStatus("Ready!");
  document.getElementById("runBtn").disabled = false;
} catch (e) {
  const msg = e && e.message ? e.message : String(e);
  console.error("Playground initialisation failed:", e);
  setStatus(`Initialisation failed: ${msg}`);
  const stderrEl = document.getElementById("stderr");
  if (stderrEl) {
    stderrEl.value += `Initialisation failed: ${msg}\n`;
    // A wasm CompileError here almost always means the browser can't decode the
    // module's exception-handling opcodes (this build uses the newer wasm EH).
    if (e instanceof WebAssembly.CompileError) {
      stderrEl.value +=
        `This build needs WebAssembly exception-handling support. ` +
        `In Firefox, open about:config and set ` +
        `javascript.options.wasm_exnref to true, then reload.\n`;
    }
  }
}

// --- 3D preview: show the first .glb file found in the wasm filesystem ---
// User code writes glTF binary files via Waterfall.IO.writeGLB; after each
// run we scan the in-memory rootfs and display the result in <model-viewer>.
const VIEWER_SKIP_DIRS = new Set(["root", "opencascade-hs"]);
window.__lastModel = null;
function findGlbFiles(node, prefix, depth, out) {
  if (!(node.contents instanceof Map)) return;
  for (const [name, child] of node.contents) {
    const path = `${prefix}/${name}`;
    if (child.contents instanceof Map) {
      if (depth === 0 && VIEWER_SKIP_DIRS.has(name)) continue;
      if (depth < 6) findGlbFiles(child, path, depth + 1, out);
    } else if (name.toLowerCase().endsWith(".glb") && child.data) {
      out.push([path, child.data]);
    }
  }
}
function updateViewer() {
  const viewer = document.getElementById("viewer");
  const found = [];
  try {
    findGlbFiles(rootfs.dir ?? rootfs, "", 0, found);
  } catch (e) {
    console.warn("model scan failed:", e);
  }
  if (!found.length) {
    viewer.style.display = "none";
    window.__lastModel = null;
    return;
  }
  found.sort((a, b) => a[0].length - b[0].length || a[0].localeCompare(b[0]));
  const [path, data] = found[0];
  if (viewer.dataset.url) URL.revokeObjectURL(viewer.dataset.url);
  const url = URL.createObjectURL(
    new Blob([data], { type: "model/gltf-binary" })
  );
  viewer.dataset.url = url;
  viewer.setAttribute("src", url);
  viewer.style.display = "block";
  window.__lastModel = path;
}

const solidSelect = document.getElementById("solidSelect");

// Render the Solid currently chosen in the dropdown, reusing the already-loaded
// module. Deletes any previous model first so the viewer reflects only this one.
async function showSelected() {
  const name = solidSelect.value;
  if (!name) { updateViewer(); return; }
  setStatus(`Rendering ${name}...`);
  try { (rootfs.dir ?? rootfs).contents.delete("out.glb"); } catch (_) {}
  try {
    await renderSolid(name);
    updateViewer();
    setStatus("Done!");
  } catch (e) {
    setStatus(`Error: ${e.message}`);
  }
}
solidSelect.addEventListener("change", showSelected);

// --- Load a program from a URL, reflecting it in the ?program= query string ---
const loadDialog = document.getElementById("loadDialog");
const loadUrlInput = document.getElementById("loadUrl");

function applyProgram(url, text) {
  editor.dispatch({
    changes: { from: 0, to: editor.state.doc.length, insert: text },
  });
  const u = new URL(window.location.href);
  u.searchParams.set("program", url);
  history.replaceState(null, "", u);
}

async function loadFromUrl(url) {
  setStatus(`Loading ${url}...`);
  try {
    const r = await fetch(url);
    if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
    applyProgram(url, await r.text());
    // Run it immediately if GHC is ready; otherwise the startup auto-run will
    // pick up the freshly-loaded editor content once init finishes.
    if (runProgram) await run();
    else setStatus("Loaded");
  } catch (e) {
    setStatus("Failed to load program");
    document.getElementById("stderr").value +=
      `Failed to load program from ${url}: ${e.message}\n`;
  }
}

document.getElementById("loadBtn").addEventListener("click", () => {
  loadUrlInput.value =
    new URLSearchParams(window.location.search).get("program") ?? "";
  loadDialog.showModal();
  loadUrlInput.select();
});

loadDialog.addEventListener("close", () => {
  if (loadDialog.returnValue !== "ok") return;
  const url = loadUrlInput.value.trim();
  if (url) loadFromUrl(url);
});

async function run() {
  document.getElementById("runBtn").disabled = true;
  setStatus("Compiling...");

  try {
    document.getElementById("stdout").value = "";
    document.getElementById("stderr").value = "";

    // Drop any model from a previous run so the viewer reflects only this run.
    try { (rootfs.dir ?? rootfs).contents.delete("out.glb"); } catch (_) {}

    const names = JSON.parse(
      await runProgram(
        document.getElementById("ghcArgs").value,
        editor.state.doc.toString()
      )
    );

    // Repopulate the dropdown with this run's Solid-valued bindings.
    solidSelect.innerHTML = "";
    for (const n of names) {
      const opt = document.createElement("option");
      opt.value = n;
      opt.textContent = n;
      solidSelect.appendChild(opt);
    }

    if (names.length === 0) {
      solidSelect.disabled = true;
      updateViewer();
      setStatus("No top-level values of type Solid found.");
    } else {
      solidSelect.disabled = false;
      // Default to the last-defined Solid.
      solidSelect.value = names[names.length - 1];
      await showSelected();
    }
  } catch (e) {
    setStatus(`Error: ${e.message}`);
  } finally {
    document.getElementById("runBtn").disabled = false;
  }
}

document.getElementById("runBtn").addEventListener("click", run);

// Auto-run on startup once GHC is ready and there's a program to run. `runProgram`
// is only defined if init fully succeeded, so this is skipped on init failure.
if (runProgram && editor.state.doc.length > 0) {
  await run();
}
