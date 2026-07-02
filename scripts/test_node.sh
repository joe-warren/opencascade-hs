#!/bin/bash
set -euo pipefail
# Test OCCT exception handling in Node via dyld.mjs
# Run inside Docker: docker run --rm opencascade-wasm bash scripts/test_node.sh

source ~/.ghc-wasm/env
GHC_LIBDIR=$(wasm32-wasi-ghc --print-libdir)
GHC_VERSION_DIR=$(find "$GHC_LIBDIR" -maxdepth 1 -type d -name "wasm32-wasi-ghc-*" | head -1)
SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi
UNWIND_FLAG=""; [ -f "$SYSROOT/libunwind.a" ] && UNWIND_FLAG="-optl-lunwind"
CABAL_STORE=$(find ~/.ghc-wasm/.cabal/store -maxdepth 1 -type d -name "ghc-*" | head -1)
BUILD_DIR=$(find /opencascade-hs/dist-newstyle/build/wasm32-wasi -maxdepth 1 -type d -name "ghc-*" | head -1)
OCHS_PKG=$(basename "$(find "$BUILD_DIR" -maxdepth 1 -type d -name 'opencascade-hs-*' | head -1)")

echo "GHC: $(wasm32-wasi-ghc --version)"
echo "Clang: $(wasm32-wasi-clang --version | head -1)"

# Build test .so
cd /opencascade-hs
cat > /tmp/TestBox.hs << 'HSEOF'
{-# LANGUAGE CApiFFI #-}
module TestBox (testMain) where
import GHC.Wasm.Prim
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke)

-- Every hs_* wrapper runs its body inside try/catch (see hs_Exception.h)
-- and reports any C++ exception through the (HSExceptionType*, void**)
-- out-parameters, so each call below exercises the wasm EH machinery.
foreign import capi unsafe "hs_gp_Pnt.h hs_new_gp_Pnt" rawNewPnt :: Double -> Double -> Double -> IO (Ptr ())
foreign import capi unsafe "hs_BRepPrimAPI_MakeBox.h hs_new_BRepPrimAPI_MakeBox_fromPnts" rawNewBox :: Ptr () -> Ptr () -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr ())
foreign import capi unsafe "hs_BRepPrimAPI_MakeBox.h hs_BRepPrimAPI_MakeBox_Shell" rawShell :: Ptr () -> Ptr CInt -> Ptr (Ptr ()) -> IO (Ptr ())
foreign import capi unsafe "hs_Exception.h hs_new_runtime_error" rawNewRuntimeError :: Ptr CChar -> IO (Ptr ())
foreign import capi unsafe "hs_Exception.h hs_throw_std_exception" rawThrowStd :: Ptr () -> Ptr CInt -> Ptr (Ptr ()) -> IO ()
foreign export javascript "testMain" testMain :: IO ()

withEx :: String -> (Ptr CInt -> Ptr (Ptr ()) -> IO a) -> IO (a, CInt)
withEx label act = alloca $ \exType -> alloca $ \exPtr -> do
  poke exType 0
  poke exPtr nullPtr
  r <- act exType exPtr
  et <- peek exType
  putStrLn ("  " ++ label ++ ": exType=" ++ show et)
  pure (r, et)

testMain :: IO ()
testMain = do
  p1 <- rawNewPnt 0 0 0
  p2 <- rawNewPnt 1 1 1
  putStrLn "=== MakeBox fromPnts + Shell (expect exType=0) ==="
  (box, et0) <- withEx "fromPnts" (rawNewBox p1 p2)
  (sh, et1) <- withEx "shell" (rawShell box)
  putStrLn ("  shell ptr: " ++ show sh)
  putStrLn "=== throw std::exception, catch at wrapper (expect exType=2) ==="
  err <- withCString "test error" rawNewRuntimeError
  ((), et2) <- withEx "throwStd" (rawThrowStd err)
  if et0 == 0 && et1 == 0 && sh /= nullPtr && et2 == 2
    then putStrLn "NODE TEST PASSED"
    else putStrLn ("NODE TEST FAILED: " ++ show (et0, et1, et2, sh))
HSEOF

# On wasm the opencascade-hs package ships no C++ (see WASM_BUILD_NOTES.md);
# compile the hs_* wrappers into the test module, as build_playground.sh does.
echo "Compiling wrapper objects..."
WRAP_DIR=/tmp/test_wrapper_objs
mkdir -p "$WRAP_DIR"
WRAP_FLAGS="-fPIC --std=c++17 -O0 -Wno-deprecated -Wno-#pragma-messages -fexceptions -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false -D__EMSCRIPTEN__ -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS -D_WASI_EMULATED_MMAN -D_WASI_EMULATED_GETPID -I/opencascade-hs/opencascade-hs/cpp -I/OCCT/work/wasm/inc -I/OCCT/wasi_stubs"
find /opencascade-hs/opencascade-hs/cpp -maxdepth 1 -name '*.cpp' | \
  xargs -P"$(nproc)" -I{} sh -c "[ -f $WRAP_DIR/\$(basename {} .cpp).o ] || wasm32-wasi-clang++ $WRAP_FLAGS -c {} -o $WRAP_DIR/\$(basename {} .cpp).o"
WRAPPER_OBJS=""
for obj in "$WRAP_DIR"/*.o; do WRAPPER_OBJS="$WRAPPER_OBJS -optl$obj"; done

# Force-link libc++abi's objects for the EH entry points (__cxa_throw etc.);
# the sysroot's libc++abi.so does not export them.
mkdir -p /tmp/cxxabi_objs && (cd /tmp/cxxabi_objs && llvm-ar x "$SYSROOT/libc++abi.a")
for obj in /tmp/cxxabi_objs/*.o*; do WRAPPER_OBJS="$WRAPPER_OBJS -optl$obj"; done

echo "Building testbox.so..."
wasm32-wasi-ghc -v0 -shared -dynamic \
  -package-db "$CABAL_STORE/package.db" \
  -package-db "$BUILD_DIR/$OCHS_PKG/package.conf.inplace" \
  -package opencascade-hs \
  -optl-Wl,--export-all \
  -optl-lTKernel -optl-lTKMath -optl-lTKG3d -optl-lTKG2d \
  -optl-lTKGeomBase -optl-lTKGeomAlgo -optl-lTKBRep -optl-lTKTopAlgo \
  -optl-lTKPrim -optl-lTKBO -optl-lTKBool -optl-lTKFillet -optl-lTKOffset \
  -optl-lTKHLR -optl-lTKMesh -optl-lTKService -optl-lTKV3d -optl-lTKRWMesh \
  -optl-lTKShHealing -optl-lTKXSBase -optl-lTKStd -optl-lTKLCAF -optl-lTKCAF \
  -optl-lTKCDF -optl-lTKXCAF -optl-lTKDESTL -optl-lTKDESTEP -optl-lTKDEGLTF \
  -optl-lTKDEOBJ -optl-lTKDE \
  -optl-lfreetype \
  -optl-lc++ -optl-lc++abi $UNWIND_FLAG \
  $WRAPPER_OBJS \
  -optl-lwasi-emulated-process-clocks -optl-lwasi-emulated-signal \
  -optl-lwasi-emulated-mman -optl-lwasi-emulated-getpid \
  /tmp/TestBox.hs -o /tmp/testbox.so

# Patch dyld.mjs
cp "$GHC_LIBDIR/dyld.mjs" /tmp/dyld_patched.mjs
cp "$GHC_LIBDIR/prelude.mjs" /tmp/
cp "$GHC_LIBDIR/post-link.mjs" /tmp/
node /opencascade-hs/scripts/patch_dyld.js /tmp/dyld_patched.mjs

# Build search dirs
DIRS="[\"$SYSROOT\",\"$GHC_VERSION_DIR\""
for d in "$CABAL_STORE"/*/lib; do DIRS="$DIRS,\"$d\""; done
DIRS="$DIRS,\"$BUILD_DIR/$OCHS_PKG/build\",\"/tmp\"]"

# Write runner
cat > /tmp/run.mjs << JSEOF
import { DyLDHost, main as dyldMain } from "/tmp/dyld_patched.mjs";
const dyld = await dyldMain({
  rpc: new DyLDHost({}),
  searchDirs: $DIRS,
  mainSoPath: "/tmp/testbox.so",
  args: ["t", "+RTS", "-c", "-RTS"],
  isIserv: false,
});
try { await dyld.exportFuncs.testMain(); }
catch(e) { console.error("CRASH: " + e.message); process.exit(1); }
JSEOF

echo "Running test..."
node --experimental-wasm-type-reflection /tmp/run.mjs 2>&1 | tee /tmp/test_node_out.txt
grep -q "NODE TEST PASSED" /tmp/test_node_out.txt
