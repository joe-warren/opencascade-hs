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

// Full-page spinner overlay, shown while loading/compiling/rendering. Counted so
// nested busy sections (load -> run -> render) don't hide it prematurely. Starts
// at 1 because the page begins in the loading state (overlay visible by default).
const spinner = document.getElementById("spinner");
let busyCount = 1;
function setBusy(on) {
  busyCount = Math.max(0, busyCount + (on ? 1 : -1));
  spinner.classList.toggle("hidden", busyCount === 0);
}

// Keep the console hidden unless there's an error (stderr output). stdout on its
// own (e.g. the "Rendering: …" line) isn't worth showing; when there is an error
// we reveal stdout too, but only if it actually has content.
function refreshOutputs() {
  const hasErr = document.getElementById("stderr").value.trim() !== "";
  const hasOut = document.getElementById("stdout").value.trim() !== "";
  document.getElementById("outputs").style.display = hasErr ? "block" : "none";
  document.getElementById("stderrPanel").style.display = hasErr ? "block" : "none";
  document.getElementById("stdoutPanel").style.display =
    hasErr && hasOut ? "block" : "none";
}

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
let dyld, runProgram, renderSolid, exportSolid;

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
    refreshOutputs();
  }

  window.editor = new EditorView({
    doc: programResult.text ?? "",
    parent: document.getElementById("editor"),
    extensions: [basicSetup, StreamLanguage.define(haskell), oneDark],
  });
  // Loading a program only touches the editor, so enable it before GHC is ready.
  document.getElementById("loadBtn").disabled = false;
  document.getElementById("exampleMain").disabled = false;
  document.getElementById("exampleToggle").disabled = false;

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

  [runProgram, renderSolid, exportSolid] = await dyld.exportFuncs.myMain("GHC_LIBDIR", "PLAYGROUND_PKG_DBS");

  setStatus("Ready!");
  document.getElementById("runBtn").disabled = false;
  setBusy(false);
} catch (e) {
  setBusy(false);
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
    refreshOutputs();
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
  setBusy(true);
  setStatus(`Rendering ${name}...`);
  try { (rootfs.dir ?? rootfs).contents.delete("out.glb"); } catch (_) {}
  try {
    await renderSolid(name);
    updateViewer();
    setStatus("Done!");
  } catch (e) {
    setStatus(`Error: ${e.message}`);
  } finally {
    setBusy(false);
    refreshOutputs();
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
  setBusy(true);
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
  } finally {
    setBusy(false);
    refreshOutputs();
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

// --- About modal ---
document.getElementById("aboutBtn").addEventListener("click", () => {
  document.getElementById("aboutDialog").showModal();
});

// --- Examples menu: curated waterfall-cad-examples modules that work in the
// playground (each defines a top-level Solid and needs no fonts/files/SVG). ---
const EXAMPLES_BASE =
  "https://raw.githubusercontent.com/joe-warren/opencascade-hs/refs/heads/main/waterfall-cad-examples/src/";
const EXAMPLES = [
  ["CSG", "CsgExample"],
  ["Bounding boxes", "BoundingBoxExample"],
  ["Fillet", "FilletExample"],
  ["Offset", "OffsetExample"],
  ["Platonic solids", "PlatonicSolidsExample"],
  ["Prism", "PrismExample"],
  ["Revolution", "RevolutionExample"],
  ["Sweep", "SweepExample"],
  ["Take path fraction", "TakePathFractionExample"],
  ["2D booleans", "TwoDBooleansExample"],
];

// --- Split-button dropdown menus (Examples, Download format) ---
function closeAllMenus() {
  document.querySelectorAll(".split-menu").forEach((m) => (m.hidden = true));
}
// Clicking anywhere else dismisses any open menu.
document.addEventListener("click", closeAllMenus);

// Wire an element to toggle a menu (stopping the click from immediately
// closing it again via the document handler).
function wireMenuToggle(triggerEl, menuEl) {
  triggerEl.addEventListener("click", (e) => {
    e.stopPropagation();
    const willOpen = menuEl.hidden;
    closeAllMenus();
    menuEl.hidden = !willOpen;
  });
}

// Examples: both segments just open the menu (there's no default action).
const exampleMenu = document.getElementById("exampleMenu");
for (const [label, mod] of EXAMPLES) {
  const li = document.createElement("li");
  li.textContent = label;
  li.addEventListener("click", () => {
    closeAllMenus();
    loadFromUrl(`${EXAMPLES_BASE}${mod}.hs`);
  });
  exampleMenu.appendChild(li);
}
exampleMenu.addEventListener("click", (e) => e.stopPropagation());
wireMenuToggle(document.getElementById("exampleMain"), exampleMenu);
wireMenuToggle(document.getElementById("exampleToggle"), exampleMenu);

// --- Download the current solid. Only single-file formats are offered (STL,
// STEP, GLB); writeSolid on the Haskell side picks the writer by extension. ---
const downloadMain = document.getElementById("downloadMain");
const downloadMenu = document.getElementById("downloadMenu");
let downloadExt = "stl";

function setDownloadFormat(ext) {
  downloadExt = ext;
  downloadMain.textContent = `Download ${ext.toUpperCase()}`;
}

async function downloadModel() {
  const name = solidSelect.value;
  if (!name) return;
  const ext = downloadExt;
  const file = `${name}.${ext}`;
  setBusy(true);
  setStatus(`Exporting ${ext.toUpperCase()}…`);
  try {
    const dir = (rootfs.dir ?? rootfs).contents;
    try { dir.delete(file); } catch (_) {}
    await exportSolid(name, `/${file}`);
    const node = dir.get(file);
    if (!node || !node.data) throw new Error("export produced no output");
    const url = URL.createObjectURL(
      new Blob([node.data], { type: "application/octet-stream" })
    );
    const a = document.createElement("a");
    a.href = url;
    a.download = file;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
    setStatus("Done!");
  } catch (e) {
    setStatus(`Error: ${e.message}`);
  } finally {
    setBusy(false);
    refreshOutputs();
  }
}

// Main segment downloads in the current format; the menu picks a format and
// downloads immediately (and remembers it for the main segment).
downloadMain.addEventListener("click", downloadModel);
wireMenuToggle(document.getElementById("downloadToggle"), downloadMenu);
downloadMenu.addEventListener("click", (e) => e.stopPropagation());
downloadMenu.querySelectorAll("li").forEach((li) => {
  li.addEventListener("click", () => {
    closeAllMenus();
    setDownloadFormat(li.dataset.value);
    downloadModel();
  });
});

async function run() {
  document.getElementById("runBtn").disabled = true;
  setBusy(true);
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
    // Only show the picker when there's an actual choice to make.
    document.getElementById("solidControls").style.display =
      names.length > 1 ? "flex" : "none";
    // Download is possible whenever there's at least one solid.
    document.getElementById("downloadMain").disabled = names.length === 0;
    document.getElementById("downloadToggle").disabled = names.length === 0;

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
    setBusy(false);
    refreshOutputs();
  }
}

document.getElementById("runBtn").addEventListener("click", run);

// Auto-run on startup once GHC is ready and there's a program to run. `runProgram`
// is only defined if init fully succeeded, so this is skipped on init failure.
if (runProgram && editor.state.doc.length > 0) {
  await run();
}
