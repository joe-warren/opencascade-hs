import { EditorView, basicSetup } from "https://esm.sh/codemirror@6.0.1";
import { keymap } from "https://esm.sh/@codemirror/view@6";
import { StreamLanguage } from "https://esm.sh/@codemirror/language@6";
import { haskell } from "https://esm.sh/@codemirror/legacy-modes@6/mode/haskell";
import { oneDark } from "https://esm.sh/@codemirror/theme-one-dark@6";
import { Compartment } from "https://esm.sh/@codemirror/state@6";
import {
  ConsoleStdout, File, OpenFile, PreopenDirectory, WASI,
} from "https://esm.sh/gh/haskell-wasm/browser_wasi_shim";
import { DyLDBrowserHost, main } from "./dyld.mjs";

const statusEl = document.getElementById("status");
const setStatus = (msg) => { statusEl.textContent = msg; };

// Editor colour scheme follows the OS light/dark preference. The theme lives in
// a Compartment so it can be swapped live when the preference changes.
const themeCompartment = new Compartment();
const darkQuery = window.matchMedia("(prefers-color-scheme: dark)");
const editorTheme = () => (darkQuery.matches ? oneDark : []);

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

const programUrl =
  new URLSearchParams(window.location.search).get("program") ?? "./example.hs";

// Assigned during init below; referenced by the event handlers. Kept at module
// scope so the handlers can see them.
let dyld, runProgram, renderSolid, exportSolid;

try {
  const [{ instance }, rootfs_bytes, programResult] = await Promise.all([
    WebAssembly.instantiateStreaming(
      fetch("https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm"),
      { wasi_snapshot_preview1: bsdtar_wasi.wasiImport }
    ),
    // ROOTFS_URL is substituted by build_playground.sh; defaults to
    // "./rootfs.tar.zst" but can point at another host for deployment.
    fetch("ROOTFS_URL").then((r) => r.bytes()),
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

  if (programResult.error) {
    document.getElementById("stderr").value +=
      `Failed to load program from ${programUrl}: ${programResult.error.message}\n`;
    refreshOutputs();
  }

  window.editor = new EditorView({
    doc: programResult.text ?? "",
    parent: document.getElementById("editor"),
    extensions: [
      keymap.of([
        {
          key: "Mod-Enter",
          preventDefault: true,
          run: () => {
            if (runProgram) run();
            return true;
          },
        },
      ]),
      basicSetup,
      StreamLanguage.define(haskell),
      themeCompartment.of(editorTheme()),
    ],
  });
  // Swap the editor theme live if the OS light/dark preference changes.
  darkQuery.addEventListener("change", () => {
    editor.dispatch({ effects: themeCompartment.reconfigure(editorTheme()) });
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
    // A wasm CompileError here might mean the browser can't decode the
    // module's exception-handling opcodes (this build uses the newer wasm EH).
    if (e instanceof WebAssembly.CompileError) {
      stderrEl.value +=
        `\nThis build may need WebAssembly exception-handling support.\n ` +
        `In Firefox, open about:config and set ` +
        `javascript.options.wasm_exnref to true, then reload.\n` + 
        `Also, it's a little temperamental, it may help to reload even if that is set.\n`;
    }
    refreshOutputs();
  }
}

// --- 3D preview: show the first .glb file found in the wasm filesystem ---
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

function clearViewer() {
  const viewer = document.getElementById("viewer");
  if (viewer.dataset.url) {
    URL.revokeObjectURL(viewer.dataset.url);
    delete viewer.dataset.url;
  }
  viewer.removeAttribute("src");
  viewer.style.display = "none";
  window.__lastModel = null;
}

const solidSelect = document.getElementById("solidSelect");

// name -> whether the binding is `IO Solid` (true) or plain `Solid` (false),
// populated by run(). render/export need this to build the right expression.
let solidIsIO = {};
const ioFlag = (name) => (solidIsIO[name] ? "true" : "false");

// Render the Solid currently chosen in the dropdown, reusing the already-loaded
// module. Deletes any previous model first so the viewer reflects only this one.
async function showSelected() {
  const name = solidSelect.value;
  if (!name) { updateViewer(); return; }
  setBusy(true);
  setStatus(`Rendering ${name}...`);
  try { (rootfs.dir ?? rootfs).contents.delete("out.glb"); } catch (_) {}
  try {
    await renderSolid(name, resolution(), ioFlag(name));
    updateViewer();
    setStatus("Done!");
  } catch (e) {
    setStatus(`Error: ${e.message}`);
    clearViewer();
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

document.getElementById("aboutBtn").addEventListener("click", () => {
  document.getElementById("aboutDialog").showModal();
});

// --- Settings modal ---
document.getElementById("settingsBtn").addEventListener("click", () => {
  document.getElementById("settingsDialog").showModal();
});

// --- File upload modal: write arbitrary files into the in-memory FS at their
// own name (/<name>) so user code can read them (fonts, meshes to import, ...).
// URL-loaded files are reflected in the query string (?file=<url>, repeatable)
// for deeplinking and are re-fetched on load; uploaded files are session-only. ---
const filesDialog = document.getElementById("filesDialog");
const fileUrlInput = document.getElementById("fileUrlInput");
const fileUploadInput = document.getElementById("fileUploadInput");
const fileListEl = document.getElementById("fileList");
// name -> { url } (url present when the file came from a URL, for the deeplink).
const loadedFiles = new Map();

document.getElementById("filesBtn").addEventListener("click", () => {
  filesDialog.showModal();
});

function fsWrite(name, bytes) {
  (rootfs.dir ?? rootfs).contents.set(name, new File(bytes, { readonly: true }));
}
// Reflect URL-loaded files in the address bar (?file=<url>, one per file) so the
// page is shareable. Local uploads have no URL and are omitted.
function syncFileParams() {
  const u = new URL(window.location.href);
  u.searchParams.delete("file");
  for (const v of loadedFiles.values()) {
    if (v.url) u.searchParams.append("file", v.url);
  }
  history.replaceState(null, "", u);
}
function reportFileError(where, e) {
  document.getElementById("stderr").value +=
    `Failed to load file from ${where}: ${e.message}\n`;
  refreshOutputs();
}
function renderFileList() {
  fileListEl.innerHTML = "";
  for (const name of loadedFiles.keys()) {
    const li = document.createElement("li");
    const code = document.createElement("code");
    code.textContent = `/${name}`;
    const rm = document.createElement("button");
    rm.type = "button";
    rm.textContent = "✕";
    rm.setAttribute("aria-label", `Remove ${name}`);
    rm.addEventListener("click", () => {
      try { (rootfs.dir ?? rootfs).contents.delete(name); } catch (_) {}
      loadedFiles.delete(name);
      syncFileParams();
      renderFileList();
    });
    li.append(code, rm);
    fileListEl.appendChild(li);
  }
}
function urlBasename(url) {
  try {
    const name = new URL(url, window.location.href).pathname.split("/").filter(Boolean).pop();
    return name || "download";
  } catch (_) {
    return "download";
  }
}
async function addFileFromUrl(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
  const name = urlBasename(url);
  fsWrite(name, new Uint8Array(await r.arrayBuffer()));
  loadedFiles.set(name, { url });
  syncFileParams();
  renderFileList();
}

document.getElementById("fileUrlAdd").addEventListener("click", async () => {
  const url = fileUrlInput.value.trim();
  if (!url) return;
  try {
    await addFileFromUrl(url);
    fileUrlInput.value = "";
    setStatus("File loaded");
  } catch (e) {
    reportFileError(url, e);
  }
});
fileUploadInput.addEventListener("change", async () => {
  for (const file of fileUploadInput.files) {
    fsWrite(file.name, new Uint8Array(await file.arrayBuffer()));
    loadedFiles.set(file.name, {}); // session-only, no URL to persist
  }
  fileUploadInput.value = "";
  renderFileList();
  setStatus("Files loaded");
});
// Load files named in the query string (?file=<url>, repeatable) on startup,
// for deeplinks (fire-and-forget; Reload if a program races ahead of a file).
for (const url of new URLSearchParams(window.location.search).getAll("file")) {
  addFileFromUrl(url).catch((e) => reportFileError(url, e));
}

const rotateToggle = document.getElementById("rotateToggle");
const resolutionInput = document.getElementById("resolutionInput");

// Persist settings across refreshes (localStorage; ignored if unavailable,
// e.g. private mode).
const SETTINGS_KEY = "waterfall-playground-settings";
function saveSettings() {
  try {
    localStorage.setItem(
      SETTINGS_KEY,
      JSON.stringify({
        autoRotate: rotateToggle.checked,
        resolution: resolutionInput.value,
      })
    );
  } catch (_) {}
}
// Restore before wiring handlers, so the startup render picks up the stored
// resolution and the viewer starts with the stored rotate state.
try {
  const s = JSON.parse(localStorage.getItem(SETTINGS_KEY) || "{}");
  if (typeof s.autoRotate === "boolean") rotateToggle.checked = s.autoRotate;
  if (s.resolution) resolutionInput.value = s.resolution;
} catch (_) {}

// Auto-rotate toggle for the model viewer.
function applyRotate() {
  const viewer = document.getElementById("viewer");
  if (rotateToggle.checked) viewer.setAttribute("auto-rotate", "");
  else viewer.removeAttribute("auto-rotate");
}
applyRotate();
rotateToggle.addEventListener("change", () => {
  applyRotate();
  saveSettings();
});

// Mesh resolution (deflection) passed to render/export. Falls back to a sane
// default if the field is blank or non-positive; returned as a string so it
// drops straight into the compiled Haskell expression.
function resolution() {
  const v = parseFloat(resolutionInput.value);
  return Number.isFinite(v) && v > 0 ? String(v) : "0.01";
}
// Re-render the current solid when the resolution changes.
resolutionInput.addEventListener("change", () => {
  saveSettings();
  if (runProgram && solidSelect.value) showSelected();
});

// TEMPORARY: load examples from the current dev branch rather than main, so
// unmerged example changes (e.g. the Text example) are available. Revert to
// refs/heads/main once merged.
const EXAMPLES_BASE =
  "https://raw.githubusercontent.com/joe-warren/opencascade-hs/refs/heads/wasm-build-dirty-rebased/waterfall-cad-examples/src/";
// Each example may bring files that are loaded into the FS before it runs (e.g.
// a font for the Text example). Font is on main (it's stable test-data).
const FONT_VARELA =
  "https://raw.githubusercontent.com/joe-warren/opencascade-hs/refs/heads/main/waterfall-cad-examples/test-data/fonts/varela/VarelaRound-Regular.ttf";
const EXAMPLES = [
  { label: "CSG", mod: "CsgExample", files: [] },
  { label: "Bounding Boxes", mod: "BoundingBoxExample", files: [] },
  { label: "Fillet", mod: "FilletExample", files: [] },
  { label: "Offset", mod: "OffsetExample", files: [] },
  { label: "Platonic Solids", mod: "PlatonicSolidsExample", files: [] },
  { label: "Gear", mod: "GearExample", files: [] },
  { label: "Prism", mod: "PrismExample", files: [] },
  { label: "Revolution", mod: "RevolutionExample", files: [] },
  { label: "Sweep", mod: "SweepExample", files: [] },
  { label: "Take Path Fraction", mod: "TakePathFractionExample", files: [] },
  { label: "2D Booleans", mod: "TwoDBooleansExample", files: [] },
  { label: "Text", mod: "TextExample", files: [FONT_VARELA] },
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
for (const { label, mod, files } of EXAMPLES) {
  const li = document.createElement("li");
  li.textContent = label;
  li.addEventListener("click", async () => {
    closeAllMenus();
    // Load the example's files into the FS first, so the program can read them,
    // then load + run the program.
    for (const url of files) {
      try { await addFileFromUrl(url); } catch (e) { reportFileError(url, e); }
    }
    loadFromUrl(`${EXAMPLES_BASE}${mod}.hs`);
  });
  exampleMenu.appendChild(li);
}
exampleMenu.addEventListener("click", (e) => e.stopPropagation());
wireMenuToggle(document.getElementById("exampleMain"), exampleMenu);
wireMenuToggle(document.getElementById("exampleToggle"), exampleMenu);

// Download the current solid, (STL, STEP, GLB); 
// writeSolid on the Haskell side picks the writer by extension.
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
    await exportSolid(name, `/${file}`, resolution(), ioFlag(name));
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

    // Each entry is { name, io } — io=true for `IO Solid` bindings.
    const entries = JSON.parse(
      await runProgram(
        document.getElementById("ghcArgs").value,
        editor.state.doc.toString()
      )
    );

    // Repopulate the dropdown and remember each binding's kind (pure vs IO).
    solidSelect.innerHTML = "";
    solidIsIO = {};
    for (const { name, io } of entries) {
      solidIsIO[name] = io;
      const opt = document.createElement("option");
      opt.value = name;
      opt.textContent = name;
      solidSelect.appendChild(opt);
    }
    // Only show the picker when there's an actual choice to make.
    document.getElementById("solidControls").style.display =
      entries.length > 1 ? "flex" : "none";
    // Download is possible whenever there's at least one solid.
    document.getElementById("downloadMain").disabled = entries.length === 0;
    document.getElementById("downloadToggle").disabled = entries.length === 0;

    if (entries.length === 0) {
      solidSelect.disabled = true;
      updateViewer();
      setStatus("No top-level values of type Solid found.");
    } else {
      solidSelect.disabled = false;
      // Default to the last-defined Solid.
      solidSelect.value = entries[entries.length - 1].name;
      await showSelected();
    }
  } catch (e) {
    setStatus(`Error: ${e.message}`);
    // The run failed (e.g. a compile error), so there's no current model.
    clearViewer();
    document.getElementById("solidControls").style.display = "none";
    document.getElementById("downloadMain").disabled = true;
    document.getElementById("downloadToggle").disabled = true;
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
