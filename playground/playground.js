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

const [{ instance }, rootfs_bytes, initialProgram] = await Promise.all([
  WebAssembly.instantiateStreaming(
    fetch("https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm"),
    { wasi_snapshot_preview1: bsdtar_wasi.wasiImport }
  ),
  fetch("./rootfs.tar.zst").then((r) => r.bytes()),
  fetch("./example.hs").then((r) => r.text()),
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

window.editor = new EditorView({
  doc: initialProgram,
  parent: document.getElementById("editor"),
  extensions: [basicSetup, StreamLanguage.define(haskell), oneDark],
});

setStatus("Initialising GHC...");
const dyld = await main({
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
// Grow memory to ensure enough heap space for OCCT allocations.
// After loading all GHC modules, the default memory is ~33MB which is
// insufficient for OCCT's Standard::Allocate. Growing by 256MB fixes it.
dyld.exportFuncs.memory.grow(4096);

const [runProgram, renderSolid] = await dyld.exportFuncs.myMain("GHC_LIBDIR", "PLAYGROUND_PKG_DBS");

setStatus("Ready!");
document.getElementById("runBtn").disabled = false;

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

document.getElementById("runBtn").addEventListener("click", async () => {
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
});
