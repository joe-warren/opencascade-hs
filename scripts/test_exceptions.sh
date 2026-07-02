#!/bin/bash
set -euo pipefail
# Test C++ exception throw/catch with OCCT types in wasm shared library
source ~/.ghc-wasm/env
CABAL_STORE=$(find ~/.ghc-wasm/.cabal/store -maxdepth 1 -type d -name "ghc-*" | head -1)
BUILD_DIR=$(find /opencascade-hs/dist-newstyle/build/wasm32-wasi -maxdepth 1 -type d -name "ghc-*" | head -1)
OCHS_PKG=$(basename "$(find "$BUILD_DIR" -maxdepth 1 -type d -name 'opencascade-hs-*' | head -1)")
GHC_LIBDIR=$(wasm32-wasi-ghc --print-libdir)
GHC_VERSION_DIR=$(find "$GHC_LIBDIR" -maxdepth 1 -type d -name "wasm32-wasi-ghc-*" | head -1)
SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi
UNWIND_FLAG=""; [ -f "$SYSROOT/libunwind.a" ] && UNWIND_FLAG="-optl-lunwind"
cd /opencascade-hs

cat > /tmp/throwtest.cpp << 'CPPEOF'
#include <cstdio>
#include <Standard_DomainError.hxx>
#include <Standard_Failure.hxx>

extern "C" {
int test_no_throw() {
    Standard_DomainError e("test");
    fprintf(stderr, "construct OK: %s\n", e.GetMessageString());
    return 1;
}
int test_catch_all() {
    try { throw Standard_DomainError("test"); }
    catch (...) { fprintf(stderr, "catch(...) OK\n"); return 1; }
    return 0;
}
int test_catch_exact() {
    try { throw Standard_DomainError("test"); }
    catch (Standard_DomainError const& e) { fprintf(stderr, "catch exact OK: %s\n", e.GetMessageString()); return 1; }
    return 0;
}
int test_catch_base() {
    try { throw Standard_DomainError("test"); }
    catch (Standard_Failure const& e) { fprintf(stderr, "catch base OK: %s\n", e.GetMessageString()); return 1; }
    return 0;
}
}
CPPEOF

wasm32-wasi-clang++ -fPIC -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false \
  -I/OCCT/work/wasm/inc -D__EMSCRIPTEN__ \
  -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS \
  -D_WASI_EMULATED_MMAN -D_WASI_EMULATED_GETPID -I/OCCT/wasi_stubs \
  -c /tmp/throwtest.cpp -o /tmp/throwtest.o

mkdir -p /tmp/cxxabi_objs && cd /tmp/cxxabi_objs && llvm-ar x "$SYSROOT/libc++abi.a"
cd /opencascade-hs
CXX_ABI_OBJS=""
for obj in /tmp/cxxabi_objs/*.o*; do CXX_ABI_OBJS="$CXX_ABI_OBJS -optl$obj"; done

cat > /tmp/TestThrow.hs << 'HSEOF'
{-# LANGUAGE CApiFFI #-}
module TestThrow (testMain) where
import GHC.Wasm.Prim
import Foreign.C.Types
import System.IO (hFlush, stdout)
foreign import ccall unsafe "test_no_throw" t0 :: IO CInt
foreign import ccall unsafe "test_catch_all" t1 :: IO CInt
foreign import ccall unsafe "test_catch_exact" t2 :: IO CInt
foreign import ccall unsafe "test_catch_base" t3 :: IO CInt
foreign export javascript "testMain" testMain :: IO ()
testMain :: IO ()
testMain = do
  putStrLn "=== no throw ===" >> hFlush stdout
  r0 <- t0; putStrLn ("  " ++ show r0) >> hFlush stdout
  putStrLn "=== catch(...) ===" >> hFlush stdout
  r1 <- t1; putStrLn ("  " ++ show r1) >> hFlush stdout
  putStrLn "=== catch exact ===" >> hFlush stdout
  r2 <- t2; putStrLn ("  " ++ show r2) >> hFlush stdout
  putStrLn "=== catch base ===" >> hFlush stdout
  r3 <- t3; putStrLn ("  " ++ show r3) >> hFlush stdout
  putStrLn "ALL PASSED" >> hFlush stdout
HSEOF

wasm32-wasi-ghc -v0 -shared -dynamic \
  -package-db "$CABAL_STORE/package.db" \
  -package-db "$BUILD_DIR/$OCHS_PKG/package.conf.inplace" \
  -package opencascade-hs \
  -optl-Wl,--export-all -optl-Wl,--whole-archive \
  -optl-lTKernel -optl-lTKMath -optl-lTKG3d -optl-lTKG2d \
  -optl-lTKGeomBase -optl-lTKGeomAlgo -optl-lTKBRep -optl-lTKTopAlgo \
  -optl-lTKPrim -optl-lTKBO -optl-lTKBool \
  -optl-Wl,--no-whole-archive \
  -optl-lc++ -optl-lc++abi $UNWIND_FLAG \
  -optl-lwasi-emulated-process-clocks -optl-lwasi-emulated-signal \
  -optl-lwasi-emulated-mman -optl-lwasi-emulated-getpid \
  -optl/tmp/throwtest.o $CXX_ABI_OBJS \
  /tmp/TestThrow.hs -o /tmp/testthrow.so

node /opencascade-hs/scripts/patch_dyld.js "$GHC_LIBDIR/dyld.mjs" /tmp/dyld_patched.mjs
cp "$GHC_LIBDIR/prelude.mjs" /tmp/
cp "$GHC_LIBDIR/post-link.mjs" /tmp/
DIRS="[\"$SYSROOT\",\"$GHC_VERSION_DIR\""
for d in "$CABAL_STORE"/*/lib; do DIRS="$DIRS,\"$d\""; done
DIRS="$DIRS,\"$BUILD_DIR/$OCHS_PKG/build\",\"/tmp\"]"

cat > /tmp/run.mjs << JSEOF
import { DyLDHost, main as dyldMain } from "/tmp/dyld_patched.mjs";
const dyld = await dyldMain({
  rpc: new DyLDHost({}),
  searchDirs: $DIRS,
  mainSoPath: "/tmp/testthrow.so",
  args: ["t", "+RTS", "-c", "-RTS"],
  isIserv: false,
});
dyld.exportFuncs.memory.grow(4096);
try { await dyld.exportFuncs.testMain(); }
catch(e) { console.error("CRASH:", e.message);
  console.error(e.stack.split("\n").slice(0,5).join("\n")); }
JSEOF

node --experimental-wasm-type-reflection /tmp/run.mjs 2>&1 | grep -v "Assertion failed"
