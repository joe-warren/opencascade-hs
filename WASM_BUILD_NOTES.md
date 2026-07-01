# WASM Build Notes

The wasm build works end-to-end but relies on several workarounds. This document tracks what's cursed and what the ideal fix would be.

## Pinned versions

OCCT comes from a fork,
[`joe-warren/OCCT-Wasi-Fork`](https://github.com/joe-warren/OCCT-Wasi-Fork),
based on the **7.9.3 release** (`V7_9_3`) — the stable series the native build
targets, rather than 8.0-dev master — with the wasi support committed on top.
**The Dockerfile clones the fork's `main` at HEAD; it is not pinned to a
commit.** Any push to the fork changes the build; pinning a commit (as is
already done for freetype and rapidjson) would close that reproducibility gap.

freetype and rapidjson are pinned to their master commits as of 2026-04-01,
ghc-wasm-meta to commit `61a4baf7` (GHC 9.14.1.20260213), and `cabal.project`
pins `index-state` to 2026-04-01. Bump these deliberately, one at a time.

The ghc-wasm-meta pin exists because the GHC 9.14.1.20260330 snapshot (from an
April 2026 bump) broke wasm exception handling in shared libraries, while
9.14.1.20260213 works. The exact failure mode was never recorded (the
unsquashed history on `wasm-build-dirty` only says "the test works with GHC
9.14.1.20260213 but crashes with 20260330").
`scripts/test_node.sh` was the canary that distinguished the two versions at
the time (it exercises OCCT exception handling via Node/dyld.mjs);
`scripts/test_exceptions.sh` and the browser suite (`scripts/test_browser.sh`)
are the fuller checks.

What is known about the regression (investigated 2026-07-01):

- The wasi-sdk tarball (`20251219T213239`, LLVM 21) is **identical** in both
  ghc-wasm-meta states, so clang/lld/libc++abi behaviour for
  `-fwasm-exceptions` did not change. The regression is inside GHC itself.
- The 9.14 flavour is built from the haskell-wasm fork's backport branch
  (https://gitlab.haskell.org/haskell-wasm/ghc, branch `ghc-9.14`). The delta
  is `3e0db874ab57...05e0ef08e100`. Nothing in it touches wasm EH codegen,
  linker flags, or dyld loader logic; the plausible indirect culprits are the
  ResolvedBCO/BCOByteArray bytecode-serialisation overhaul ("bytecode
  improvements", 2026-03-30 batch), the `tryPutMVar#` ccall lowering (JSFFI
  async completion is MVar-based), or the ghci JSFFI-export check change in
  `Tc/Gen/Foreign.hs` (all in the 2026-03-10 batch).
- No upstream GHC issue reports this; it is not known-fixed. As of 2026-07-01,
  **ghc-wasm-meta master still ships 9.14.1.20260330** as the 9.14 flavour
  (last GHC bump 2026-04-04), so unpinning alone changes nothing — moving
  forward means re-testing a newer flavour (master ships 9.15.20260331) or
  bisecting/reporting the 9.14 delta upstream.

7.9 builds a host tool (`ExpToCasExe`) as a wasm executable; since everything
is compiled `-fPIC -fwasm-exceptions` the C++ exception tag is an import the
playground's loader would normally supply, so the fork's
`adm/scripts/wasm_build.sh` links with `--allow-undefined` (the tool links but
is never run; only the static libraries are consumed).

## Workarounds in use

### Pretending to be Emscripten (`-D__EMSCRIPTEN__`)

We define `__EMSCRIPTEN__` when building OCCT so it takes its existing wasm code paths (no X11, no fontconfig, simplified signal handling, etc.). This works but means OCCT thinks it has access to Emscripten APIs, which we stub out.

**Ideal fix:** Add proper `__wasi__` platform guards to OCCT (fork or upstream). OCCT already has `__EMSCRIPTEN__` guards everywhere - adding `|| defined(__wasi__)` alongside them would be straightforward.

### Stub POSIX headers (`wasi_stubs/` in the OCCT fork)

WASI lacks many POSIX features that OCCT expects. The fork provides stub headers for: `syslog.h`, `netdb.h`, `pwd.h`, `execinfo.h`, `sys/utsname.h`, `setjmp.h`, `malloc.h`, plus `wasi_signal_ext.h` and `wasi_fcntl_ext.h`.

It also provides stub Emscripten headers: `emscripten.h`, `emscripten/html5.h`, `emscripten/key_codes.h`, `emscripten/dom_pk_codes.h`.

**Ideal fix:** proper `__wasi__` guards (in the fork, ideally upstreamed) would eliminate most of these. The Emscripten stubs would be unnecessary if we weren't pretending to be Emscripten.

### OCCT source changes (the fork; formerly `sed` patches)

The build used to `sed`-patch OCCT sources at image-build time to stub `getuid`, `umask`, `mkstemp`, `mkdtemp`, `timezone`, and signal extensions. Those changes now live as commits in `OCCT-Wasi-Fork` instead ("Add fake stub system calls to support wasi target"), which is far less fragile against upstream drift.

**Ideal fix:** Same as above - upstream WASI support in OCCT.

### C++ exception handling

OCCT is compiled with `-fwasm-exceptions` for real wasm-native exception handling. The runtime (`__cxa_throw`, `__cxa_begin_catch`, etc.) comes from the stock `libc++abi` shipped in ghc-wasm's wasi-sdk sysroot. (Earlier iterations built `libunwind` and `libc++abi` from the ghc-wasm LLVM fork with `-fwasm-exceptions -fPIC`; those build steps were removed once the sysroot copies proved sufficient.)

**Why this matters:** Without real exception handling, the `__cxa_*` stubs called `abort()`, leaving OCCT data structures corrupt. This caused `TopoDS_Iterator::updateCurrentShape()` to crash with "memory access out of bounds" when iterating over shapes built by `BRepPrimAPI_MakeBox`.

**History:** the build used to define `-DNo_Exception` (a legacy OCCT switch that turned `Standard_Failure::Raise()` into `abort()`), originally to work around header issues from the abort-stub era. OCCT 8 removed the machinery behind that define entirely - rebuilding without it produces byte-identical libraries - so it has been stripped from all build flags. Exceptions are simply on, everywhere.

### `setjmp.h` stub

WASI's `setjmp.h` requires wasm exception handling support (`-mllvm -wasm-enable-sjlj`), but enabling that produces `__cpp_exception` imports. Our stub provides a no-op `setjmp` that always returns 0 and a `longjmp` that traps.

**Risk:** Any OCCT code that uses `setjmp`/`longjmp` for error recovery will crash instead of recovering. (The corrupted-STL meshing bug was once suspected to be related; it turned out to be an unrelated upstream OCCT 8.0-dev mesher bug — see below.)

### No `extra-libraries` and no `cxx-sources` on the library package

GHC's wasm backend tries to dynamically load `extra-libraries` at compile time via Node.js/dyld.mjs, which can't handle OCCT's static archives. We moved all library references to linker flags instead.

On top of that, the wasm build of `opencascade-hs` doesn't compile `cxx-sources` either: all C++ (the wrappers, OCCT, libc++abi) must live in a single wasm module for exception handling to work (see "C++ exception handling across wasm shared libraries" below). The wrappers are compiled by `playground/build_playground.sh` into `libplayground.so`, and the package's `hs_*` FFI imports are bound to that module by dyld at load time.

**Ideal fix:** GHC wasm should support static-only `extra-libraries` without trying to load them at compile time. This may be worth raising as a ghc-wasm issue.

### OCCT linked as plain archives (was `--whole-archive`), to fit Firefox

`libplayground.so` links the ~30 OCCT static libraries plainly and lets `wasm-ld`
pull in only the archive members actually referenced by the wrappers (it resolves
OCCT's circular inter-library dependencies with a fixpoint, so no `--start-group`
is needed — and `wasm-ld` rejects that flag anyway). `--export-all` is kept so the
in-browser GHC interpreter and the dynamic loader can still resolve every
waterfall-cad / wrapper / lifecycle symbol at runtime.

This replaced an earlier `--whole-archive` that forced *all* of OCCT into the
module. That was fine in Chrome, but once main's exception-handling migration
wrapped every call in `try/catch` (a `try_table` landing pad in every function),
the compiled machine code overran **Firefox/SpiderMonkey's wasm executable-memory
ceiling** — the playground stalled at "Initialising GHC..." with `failed to
allocate executable memory for module` → `InternalError: out of memory`. Dropping
the unreferenced OCCT trimmed `libplayground.so` from ~57MB to ~52MB, just enough
to clear the limit. Both Chrome and Firefox now load and pass the suite.

**Note:** the margin is thin — the in-browser GHC API and OCCT are both large. If
future growth pushes Firefox back over the ceiling, the next lever is function-
level `--gc-sections` with an explicit export list (export the waterfall-cad /
wrapper / lifecycle symbols the interpreter needs, let unused OCCT functions be
garbage-collected) rather than archive-member selection alone.

### Stale `.cabal` files and Docker layer caching

The `.cabal` files are generated by hpack from `package.yaml`. Docker's `COPY . /opencascade-hs` layer is cached based on file hashes. If `package.yaml` is changed but the `.cabal` file isn't regenerated locally, Docker uses the cached (stale) `.cabal`. The wasm Docker image doesn't have hpack installed, and `cabal build` doesn't regenerate `.cabal` from `package.yaml` automatically without it.

**Workaround:** Edit both `package.yaml` and `.cabal` when changing build options, or run `hpack` locally before building Docker.

### OCCT deprecation warnings (`-Wno-#pragma-messages`)

Introduced while the wasm build was on OCCT `main` (8.x), which deprecates `TopTools_ListOfShape.hxx` and other convenience headers in favour of using `NCollection` types directly; the `opencascade-hs` wrappers still use the deprecated headers. The flag is still passed (in `package.yaml` `cxx-options` and in `build_playground.sh`) — harmless on 7.9.3, and only really matters if the pin ever moves back to 8.x.

**Ideal fix:** Update `opencascade-hs` C++ wrappers to use the non-deprecated OCCT APIs. This would also be needed for native builds once OCCT 8.x becomes standard in distro packages.

### Missing C++ `#include` directives

Several `opencascade-hs` C++ wrapper files were missing explicit includes (`TopTools_ListOfShape.hxx`, `TColStd_IndexedDataMapOfStringString.hxx`). These worked on native builds due to transitive includes but failed on the wasm build, whose OCCT version has a different include graph.

**Status:** Fixed in the source. Not a workaround, just bugs that the wasm build exposed.

## C++ exception handling across wasm shared libraries (SOLVED)

OCCT throws exception types like `Standard_DomainError` internally as part of
normal control flow, so catching them has to work. It now does - the browser
test suite passes 4/4, including the `volume`/boolean test. Getting there
required understanding a chain of dynamic-linking problems with weakly-defined
RTTI symbols (typeinfo/vtable/typename for header-only classes defined via
`DEFINE_STANDARD_EXCEPTION`, which have C++ vague linkage and so are emitted weak
in every translation unit):

1. **wasm-ld does not export weakly-defined symbols** - neither `--export-all` nor
   the dylib default export them, although references to them still go through
   `GOT.mem`/`GOT.func` imports (weak symbols stay preemptible, even under
   `--Bsymbolic`). `--export-if-defined=<sym>` *does* export them, when the symbol
   is not preempted (see 3). Verified unfixed in upstream LLD 22.1.0 (wasi-sdk-33).
2. **dyld.mjs applied data relocations too early** - each module ran its
   `__wasm_apply_data_relocs` at instantiation time, against GOT entries that were
   still poisoned if the defining module loads later in the same plan. The GOT got
   healed afterwards, but the already-relocated data words (typeinfo vtable/base
   pointers) kept the poison value (`0xfffeffff` and `poison+8`), crashing
   `scan_eh_tab` on catch. **Fixed** by patch 2 in `scripts/patch_dyld.js`, which
   defers `__wasm_apply_data_relocs` + `_initialize` until the whole plan is
   loaded (eager only for libc, since dyld needs `aligned_alloc` while loading).
3. **Shared-library preemption breaks typeinfo identity** - linking
   `libplayground.so` against the cabal-built `libHSopencascade-hs-*.so` makes the
   weak RTTI definitions in the playground resolve to the shared lib's symbol, so
   `--export-if-defined` on the playground link no longer fires (symbol counts as
   imported, not defined).
4. With (2) fixed, the crash moves one layer deeper: `can_catch` ->
   `__dynamic_cast` -> `type_info::operator==` does a name comparison between the
   throw-site and catch-site typeinfo copies, and reads a corrupted `__name`
   pointer. Experiments that force-exported the weak RTTI data symbols from
   `libHSopencascade-hs.so` (via a generated `--export-if-defined` response file,
   `_ZTI*`/`_ZTS*`/`_ZTV*` only) produced a resolvable GOT but the *contents* of
   the exported typeinfo in that module were still mis-relocated
   (`name_ptr=0x0`, vtable pointer pointing at another module's base). Exporting
   *all* weak symbols (including functions) regressed other tests with
   "function signature mismatch", so any fix must stick to data symbols.

**The solution: keep all C++ in a single wasm module.** Splitting the C++
between `libHSopencascade-hs.so` (which compiled the wrapper `cxx-sources`) and
`libplayground.so` (which statically links OCCT) meant duplicate RTTI, GOT
preemption, and exceptions crossing module boundaries - an endless source of
the problems above. Instead:

- On wasm32, the `opencascade-hs` package ships **no C++ at all**
  (`cxx-sources` is native-only in `package.yaml`). Its `hs_*` FFI imports are
  left undefined (`--unresolved-symbols=import-dynamic`) and bound by dyld at
  load time.
- `playground/build_playground.sh` compiles the wrapper `cpp/*.cpp` files
  itself and links them, OCCT, freetype and libc++abi into `libplayground.so`.
  All throws and catches are now module-internal, like the (working) static
  executable case.
- The weak RTTI/guard data of that module is force-exported via a generated
  `--export-if-defined` response file (data symbols only - `_ZT[ISV]*`,
  `_ZGV*`, `_ZZ*` - exporting weak *functions* broke dyld's function bindings
  with "function signature mismatch"), so the module satisfies its own GOT.
  Together with the dyld relocation-ordering fix (patch 2) this makes the
  typeinfo objects fully and consistently relocated.

The wasm-ld weak-data export behaviour is still worth an upstream report
(LLVM and/or ghc-wasm); `scripts/test_exceptions.sh` inside the Docker image is
a standalone reproduction (throws and catches `Standard_DomainError` in a wasm
shared library under Node).

### Upstream status of the `patch_dyld.js` patches (checked 2026-07-01)

Neither patch has an upstream issue or MR, and GHC master's `dyld.mjs`
(last touched 2026-04-08) still needs both. There is no stated upstream
position on C++/foreign exceptions through the wasm dynamic linker at all —
the users guide and the dyld design note simply never mention EH tags — so
this is unexplored territory, which favours filing both as first-of-their-kind
issues with MRs attached.

**Patch 1 (shared `WebAssembly.Tag` for tag imports).** Upstream's import
loop handles exactly `env`+function, `GOT.mem` and `GOT.func` globals, then
throws `cannot handle import`; the export loop has the mirror gap (a tag
*export* also throws), so an upstream fix should cover both sides. Emscripten
precedent: it defines `__cpp_exception` once, in assembly inside libc++abi
(`system/lib/libcxxabi/src/__cpp_exception.S`); the main module exports it and
`libdylink.js` hands every side module the same Tag object by identity. The
principled upstream shape is a tag symbol table keyed by import name
(`__cpp_exception`, `__c_longjmp`, ...), fulfilled from a module's tag export
when one exists and synthesised otherwise — rather than our name-blind single
`i32` tag. Likely to be accepted: small, self-contained, turns a hard crash
into working `-fwasm-exceptions` support.

**Patch 2 (defer `__wasm_apply_data_relocs`/`_initialize` to end of plan).**
Upstream still relocates and initialises each module immediately after
instantiation; GOT.mem entries for not-yet-defined symbols hold a poison
value, and a data reloc *copies* that poison into linear memory where the
later GOT healing never reaches it — exactly the typeinfo-corruption
mechanism. Emscripten does structurally what our patch does: every startup
module's `__wasm_apply_data_relocs` goes into a `__RELOC_FUNCS__` queue
drained only in `initRuntime()`, after all startup modules are instantiated
(only post-startup `dlopen` relocates eagerly, when all symbols already
resolve). Two things upstream will push back on: our libc carve-out keys on
"exports `aligned_alloc`" where they'd prefer `soname === "libc.so"` (dyld
already special-cases that name), and deferring `_initialize` needs arguing
against upstream commit `0c9032a0`, which deliberately simplified that
ordering. The bug is latent upstream today only because pure-Haskell `.so`
chains rarely carry cross-module data relocs against forward symbols; C++
vtables/typeinfo make them inevitable.

### Mangled meshes: an upstream OCCT 8.0-dev Watson mesher bug (fixed by pinning 7.9)

Boolean results rendered with mangled geometry: boolean-cut faces with
internal boundaries got fan-shaped garbage triangles. Diagnosed June 2026 on
the original 8.0-dev pin; **no longer reproduces now that the build is pinned
to OCCT 7.9.3** (its default Watson mesher meshes the test CSG correctly -
signed mesh volume 0.1097, matching native). The history is kept here because
it explains the version pin and would resurface for anyone building on 8.0.

- The boolean/BRep itself is correct on wasm (analytic `volume` matches a
  native OCCT 7.x build to 11 decimal places), and the triangulation *nodes*
  are correct too. Plain (untrimmed) faces mesh fine.
- The trimmed, REVERSED spherical face of a cut gets interior nodes that are
  generated but never inserted into the triangulation; the node-index
  compaction then maps triangle indices onto wrong/uninitialised nodes.
  Signed mesh volume of the result: 0.66 instead of the analytic 0.10.
- **This is not a wasm bug**: building the same pinned OCCT commit natively
  (gcc, Debian arm64) reproduces it bit-for-bit. It is the same family as
  OCCT issue #929 ("Internal vertices not bound in output triangulation");
  PR #940 (merged 2026-04-24) fixed one binding path but the boolean-result
  case is still broken at master `d3056ef8` (2026-06-12). Worth a new
  upstream report - `/tmp/meshdump.cpp`-style repro: mesh
  `BRepAlgoAPI_Cut(box, sphere)` at deflection 0.01 and compare per-face
  node/triangle counts.
- **Past workaround (removed):** while on 8.0-dev the playground forced OCCT's
  alternative Delabella algorithm (`CSF_MeshAlgo=delabella`) via a constructor
  in `playground/wasm_mesh_default.cpp`. That file was deleted once 7.9.3 was
  confirmed to mesh correctly with the default Watson algorithm. If the pin is
  ever moved back to 8.x, reinstating that one-file `setenv` is the fix.
- If you suspect a meshing regression, the diagnostic was a `meshdump.cpp` that
  meshes `BRepAlgoAPI_Cut(box, sphere)` at deflection 0.01 and compares per-face
  node/triangle counts; the bug is the same family as OCCT issue #929
  ("Internal vertices not bound in output triangulation").

The browser test suite asserts the signed mesh volume of an exported CSG
model against its analytic volume, which catches this whole class of bug.

## Visualization

The playground shows a 3D preview using
[`<model-viewer>`](https://modelviewer.dev): user code writes a glTF binary
file (`Waterfall.IO.writeGLB`) into the in-browser WASI filesystem, and after
each run the page scans the filesystem for `.glb` files and displays the first
one found via a Blob URL. The default example does exactly that with
`/out.glb`. The browser test suite covers this end-to-end (writes a CSG model,
asserts `model-viewer` actually parsed and loaded it).

Two cosmetic issues:
- OCCT's glTF writer warns `Unable to remove temporary glTF content file
  '/out.bin.tmp'` - file removal isn't supported by the WASI shim setup, so a
  stray `.tmp` file stays in the in-memory filesystem. Harmless.
- In headless Chromium (`--disable-gpu`), model-viewer logs
  `Cannot read properties of null (reading 'isPresenting')` from its XR probe.
  Doesn't happen in a normal browser, doesn't affect rendering.

### OCCT native visualization (OpenGL) still disabled

`USE_OPENGL=OFF` and `USE_GLES2=OFF` in the OCCT build. OCCT's own wasm
visualization (`OpenGl_GraphicDriver` + `Wasm_Window`) assumes the Emscripten
runtime: EGL/GLES2 emulation and `emscripten_webgl_*` html5 APIs, which are
no-op stubs in this wasi-sdk build. Making it work would mean implementing a
GLES-to-WebGL shim for the dyld runtime (the equivalent of Emscripten's
`library_webgl.js`) - a substantial project, tracked as possible future work.
The model-viewer approach above is the practical alternative.

### `-with-rtsopts=-N` incompatible with wasm

The examples executable had `-with-rtsopts=-N` baked in, which fails on wasm (no threading). Removed from the `.cabal` file directly - should ideally be conditional on platform.
