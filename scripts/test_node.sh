#!/bin/bash
set -euo pipefail
# Test OCCT exception handling in Node via dyld.mjs
# Run inside Docker: docker run --rm opencascade-wasm bash scripts/test_node.sh

source ~/.ghc-wasm/env
GHC_LIBDIR=$(wasm32-wasi-ghc --print-libdir)
GHC_VERSION_DIR=$(find "$GHC_LIBDIR" -maxdepth 1 -type d -name "wasm32-wasi-ghc-*" | head -1)
SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi
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
foreign import capi unsafe "hs_gp_Pnt.h hs_new_gp_Pnt" rawNewPnt :: Double -> Double -> Double -> IO (Ptr ())
foreign import capi unsafe "hs_BRepPrimAPI_MakeBox.h hs_BRepPrimAPI_MakeBox_fromPnts_Shell" rawShell :: Ptr () -> Ptr () -> IO (Ptr ())
foreign export javascript "testMain" testMain :: IO ()
testMain :: IO ()
testMain = do
  p1 <- rawNewPnt 0 0 0
  p2 <- rawNewPnt 1 1 1
  putStrLn "calling fromPntsShell..."
  sh <- rawShell p1 p2
  putStrLn ("shell: " ++ show sh)
HSEOF

echo "Building testbox.so..."
wasm32-wasi-ghc -v0 -shared -dynamic \
  -package-db "$CABAL_STORE/package.db" \
  -package-db "$BUILD_DIR/$OCHS_PKG/package.conf.inplace" \
  -package opencascade-hs \
  -optl-Wl,--export-all -optl-Wl,--whole-archive \
  -optl-lTKernel -optl-lTKMath -optl-lTKG3d -optl-lTKG2d \
  -optl-lTKGeomBase -optl-lTKGeomAlgo -optl-lTKBRep -optl-lTKTopAlgo \
  -optl-lTKPrim -optl-lTKBO -optl-lTKBool \
  -optl-Wl,--no-whole-archive \
  -optl-lc++ -optl-lc++abi -optl-lunwind \
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
try { await dyld.exportFuncs.testMain(); console.error("SUCCESS!"); }
catch(e) { console.error("CRASH: " + e.message); }
JSEOF

echo "Running test..."
node --experimental-wasm-type-reflection /tmp/run.mjs 2>&1
