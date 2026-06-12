#!/bin/bash
set -euxo pipefail

# Build the waterfall-cad browser playground.
# Must be run inside the Docker container after wasm32-wasi-cabal build all.
# Produces: playground/dist/ containing index.html, rootfs.tar.zst, dyld.mjs

source ~/.ghc-wasm/env

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIST_DIR="${SCRIPT_DIR}/dist"
SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi
GHC_LIBDIR="$(wasm32-wasi-ghc --print-libdir)"
GHC_VERSION_DIR="$(find "$GHC_LIBDIR" -maxdepth 1 -type d -name 'wasm32-wasi-ghc-*' | head -1)"
GHC_VERSION_DIRNAME="$(basename "$GHC_VERSION_DIR")"
CABAL_STORE=$(find ~/.ghc-wasm/.cabal/store -maxdepth 1 -type d -name "ghc-*" | head -1)
OPENCASCADE_HS_DIR=/opencascade-hs
BUILD_DIR=$(find /opencascade-hs/dist-newstyle/build/wasm32-wasi -maxdepth 1 -type d -name "ghc-*" | head -1)

# Shared package version, anchored in package-defaults.yaml
PKG_VERSION=$(sed -n 's/^_version: &version "\(.*\)"/\1/p' "$OPENCASCADE_HS_DIR/package-defaults.yaml")
LOCAL_PKGS=("opencascade-hs-$PKG_VERSION" "waterfall-cad-$PKG_VERSION" "waterfall-cad-svg-$PKG_VERSION")

# OCCT libraries to statically link into libplayground.so
OCCT_LIBS=(
  TKernel TKMath TKG3d TKG2d TKGeomBase TKGeomAlgo TKBRep TKTopAlgo
  TKPrim TKBO TKBool TKFillet TKOffset TKHLR TKMesh TKService TKV3d
  TKRWMesh TKShHealing TKXSBase TKStd TKLCAF TKCAF TKCDF TKXCAF
  TKDESTL TKDESTEP TKDEGLTF TKDEOBJ TKDE
)

echo "=== Building waterfall-cad playground ==="

rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR"

# We build the rootfs using the original absolute paths to avoid
# package relocatability issues. The rootfs tarball extracts to / .

ROOTFS=/tmp/playground-rootfs
rm -rf "$ROOTFS"
mkdir -p "$ROOTFS"

# --- 1. Build libplayground.so with OCCT statically linked ---
echo "[1/6] Building libplayground.so..."

# Extract all libc++abi objects and link them directly into libplayground.so.
# This ensures __cxa_throw etc. are IN the same module as the OCCT catch blocks,
# which is required for wasm exception handling to work across call boundaries.
# We can't use -lc++abi with --whole-archive because wasm-ld -shared treats
# __cxa_* as imports regardless. Direct .o linking forces definitions.
SYSROOT_LIB="$SYSROOT"
mkdir -p /tmp/cxxabi_objs && cd /tmp/cxxabi_objs && llvm-ar x "$SYSROOT_LIB/libc++abi.a"
cd "$OPENCASCADE_HS_DIR"
CXX_ABI_OBJS=""
for obj in /tmp/cxxabi_objs/*.obj; do
  CXX_ABI_OBJS="$CXX_ABI_OBJS -optl$obj"
done

OCCT_LINK_FLAGS="-optl-fwasm-exceptions -optl-Wl,-z,stack-size=8388608 -optl-Wl,--export-all -optl-Wl,--whole-archive"
for lib in "${OCCT_LIBS[@]}"; do
  OCCT_LINK_FLAGS="$OCCT_LINK_FLAGS -optl-l$lib"
done
OCCT_LINK_FLAGS="$OCCT_LINK_FLAGS -optl-Wl,--no-whole-archive"
OCCT_LINK_FLAGS="$OCCT_LINK_FLAGS -optl-lfreetype -optl-lc++ -optl-lunwind"
OCCT_LINK_FLAGS="$OCCT_LINK_FLAGS $CXX_ABI_OBJS"
OCCT_LINK_FLAGS="$OCCT_LINK_FLAGS -optl-lwasi-emulated-process-clocks -optl-lwasi-emulated-signal"
OCCT_LINK_FLAGS="$OCCT_LINK_FLAGS -optl-lwasi-emulated-mman -optl-lwasi-emulated-getpid"

# Also statically link the Haskell packages (opencascade-hs, waterfall-cad, waterfall-cad-svg)
# by passing their .a files directly to avoid cross-module function table issues
HS_STATIC_LIBS=""
for pkg in "${LOCAL_PKGS[@]}"; do
  HS_STATIC_LIBS="$HS_STATIC_LIBS -optl$BUILD_DIR/$pkg/build/libHS${pkg}-inplace.a"
done

PLAYGROUND_SO="/tmp/libplayground.so"
cd "$OPENCASCADE_HS_DIR"
wasm32-wasi-ghc \
  -v0 \
  -package ghc \
  -package-db "$CABAL_STORE/package.db" \
  $(find dist-newstyle/packagedb -maxdepth 1 -type d -name 'ghc-*' 2>/dev/null | head -1 | xargs -I{} echo "-package-db {}") \
  -package-db "$BUILD_DIR/${LOCAL_PKGS[0]}/package.conf.inplace" \
  -package-db "$BUILD_DIR/${LOCAL_PKGS[1]}/package.conf.inplace" \
  -package-db "$BUILD_DIR/${LOCAL_PKGS[2]}/package.conf.inplace" \
  -package opencascade-hs \
  -package waterfall-cad \
  -package waterfall-cad-svg \
  -shared -dynamic \
  -no-keep-hi-files -no-keep-o-files \
  -O2 \
  $OCCT_LINK_FLAGS \
  $HS_STATIC_LIBS \
  "$SCRIPT_DIR/Playground.hs" -o "$PLAYGROUND_SO"
rm -f "$SCRIPT_DIR"/*_stub.h

mkdir -p "$ROOTFS/tmp"
cp "$PLAYGROUND_SO" "$ROOTFS/tmp/"
echo "  $(du -h "$PLAYGROUND_SO" | cut -f1)"

# --- 2. C/C++ shared libraries ---
echo "[2/6] Copying C/C++ shared libraries..."
mkdir -p "$ROOTFS/tmp/clib"
cp "$SYSROOT"/*.so "$ROOTFS/tmp/clib/" 2>/dev/null || true

# Keep the sysroot libc++abi.so (provides operator new, __dynamic_cast, etc.)
# libplayground.so has the exception-specific __cxa_throw etc. from force-linked .o files.
# Some minor duplicate symbols (__cxa_bad_cast etc.) will cause assertion warnings - harmless.
cp "$ROOTFS/tmp/clib/libc++abi.so" "$ROOTFS/tmp/clib/liblibc++abi.so" 2>/dev/null || true
cp "$ROOTFS/tmp/clib/libc++.so" "$ROOTFS/tmp/clib/liblibc++.so" 2>/dev/null || true
echo "  $(find "$ROOTFS/tmp/clib" -name "*.so" | wc -l) .so files"

# --- 3. GHC libdir ---
echo "[3/6] Copying GHC libdir..."
mkdir -p "$ROOTFS/$GHC_LIBDIR"
cp -r "$GHC_LIBDIR"/* "$ROOTFS/$GHC_LIBDIR/"

# Trim: remove static archives, profiling, docs
find "$ROOTFS/$GHC_LIBDIR" "(" \
  -name "*.hi" \
  -o -name "*.a" \
  -o -name "*.p_hi" \
  -o -name "libHS*_p.a" \
  -o -name "*.p_dyn_hi" \
  -o -name "libHS*_p*.so" \
  -o -name "libHSrts*_debug*.so" \
  ")" -delete 2>/dev/null || true
rm -rf \
  "$ROOTFS/$GHC_LIBDIR/doc" \
  "$ROOTFS/$GHC_LIBDIR/html" \
  "$ROOTFS/$GHC_LIBDIR/latex" \
  "$ROOTFS/$GHC_LIBDIR"/*.txt \
  "$ROOTFS/$GHC_LIBDIR"/*.mjs \
  "$ROOTFS/$GHC_LIBDIR"/*.js

# Unregister Cabal/Cabal-syntax (too big, not needed for playground)
wasm32-wasi-ghc-pkg --no-user-package-db \
  --global-package-db="$ROOTFS/$GHC_LIBDIR/package.conf.d" \
  unregister Cabal Cabal-syntax 2>/dev/null || true
wasm32-wasi-ghc-pkg --no-user-package-db \
  --global-package-db="$ROOTFS/$GHC_LIBDIR/package.conf.d" \
  recache

echo "  $(du -sh "$ROOTFS/$GHC_LIBDIR" | cut -f1)"

# --- 4. Cabal store packages (waterfall-cad deps) ---
echo "[4/6] Copying cabal store packages..."
mkdir -p "$ROOTFS/$CABAL_STORE"

# Copy the package database
cp -r "$CABAL_STORE/package.db" "$ROOTFS/$CABAL_STORE/"

# Copy each package's lib directory (.so + .dyn_hi files, preserving subdirs)
for pkg_dir in "$CABAL_STORE"/*/lib; do
  pkg_name="$(basename "$(dirname "$pkg_dir")")"
  dest="$ROOTFS/$CABAL_STORE/$pkg_name/lib"
  mkdir -p "$dest"
  (cd "$pkg_dir" && find . \( -name "*.so" -o -name "*.dyn_hi" \) -print0 | \
    while IFS= read -r -d '' f; do
      mkdir -p "$dest/$(dirname "$f")"
      cp "$f" "$dest/$f"
    done)
done

# Recache the copied package DB
wasm32-wasi-ghc-pkg --no-user-package-db \
  --global-package-db="$ROOTFS/$CABAL_STORE/package.db" \
  recache

echo "  $(du -sh "$ROOTFS/$CABAL_STORE" | cut -f1)"

# --- 5. Local packages (opencascade-hs, waterfall-cad, waterfall-cad-svg) ---
echo "[5/6] Copying local packages..."

for pkg in "${LOCAL_PKGS[@]}"; do
  src="$BUILD_DIR/$pkg/build"
  dest="$ROOTFS/$BUILD_DIR/$pkg/build"
  mkdir -p "$dest"
  # Copy .so and .dyn_hi files preserving directory structure
  (cd "$src" && find . \( -name "*.so" -o -name "*.dyn_hi" \) -print0 | \
    while IFS= read -r -d '' f; do
      mkdir -p "$dest/$(dirname "$f")"
      cp "$f" "$dest/$f"
    done)

  # Copy the package config
  conf_dir="$BUILD_DIR/$pkg/package.conf.inplace"
  if [ -d "$conf_dir" ]; then
    dest_conf="$ROOTFS/$conf_dir"
    mkdir -p "$dest_conf"
    cp "$conf_dir"/*.conf "$dest_conf/" 2>/dev/null || true
    wasm32-wasi-ghc-pkg --no-user-package-db \
      --global-package-db="$dest_conf" recache 2>/dev/null || true
  fi
done

echo "  $(du -sh "$ROOTFS/$BUILD_DIR" | cut -f1)"

# --- 6. Create rootfs tarball ---
echo "[6/6] Creating rootfs.tar.zst..."
cd "$ROOTFS"
tar -cf "$DIST_DIR/rootfs.tar.zst" --zstd .
echo "  $(du -h "$DIST_DIR/rootfs.tar.zst" | cut -f1)"

# Copy dyld.mjs and index.html, patch dyld.mjs for wasm exception tag support
cp "$GHC_LIBDIR/dyld.mjs" "$GHC_LIBDIR/prelude.mjs" "$GHC_LIBDIR/post-link.mjs" "$DIST_DIR/"

# Patch dyld.mjs for wasm exception tags and duplicate symbol handling
node "$SCRIPT_DIR/../scripts/patch_dyld.js" "$DIST_DIR/dyld.mjs"
cp "$SCRIPT_DIR/index.html" "$DIST_DIR/"

# Fix paths in index.html
sed -i "s|HSLIB_SEARCH_DIR|$GHC_VERSION_DIR|g" "$DIST_DIR/index.html"
sed -i "s|GHC_LIBDIR|$GHC_LIBDIR|g" "$DIST_DIR/index.html"

# Build the package DB paths string (space-separated)
PKG_DBS="$CABAL_STORE/package.db"
for pkg in "${LOCAL_PKGS[@]}"; do
  PKG_DBS="$PKG_DBS $BUILD_DIR/$pkg/package.conf.inplace"
done
sed -i "s|PLAYGROUND_PKG_DBS|$PKG_DBS|g" "$DIST_DIR/index.html"

# Build search dirs list for dyld.mjs
# It needs to find .so files for: C stdlib, GHC packages, cabal store packages, local packages
SEARCH_DIRS="[\"\/tmp\/clib\", \"$GHC_VERSION_DIR\""
# Add each cabal store package's lib dir
for pkg_dir in "$CABAL_STORE"/*/lib; do
  pkg_name="$(basename "$(dirname "$pkg_dir")")"
  SEARCH_DIRS="$SEARCH_DIRS, \"$CABAL_STORE/$pkg_name/lib\""
done
# Add local package build dirs
for pkg in "${LOCAL_PKGS[@]}"; do
  SEARCH_DIRS="$SEARCH_DIRS, \"$BUILD_DIR/$pkg/build\""
done
SEARCH_DIRS="$SEARCH_DIRS]"
sed -i "s|SEARCH_DIRS_JSON|$SEARCH_DIRS|g" "$DIST_DIR/index.html"

# Cleanup
rm -rf "$ROOTFS"

echo ""
echo "=== Playground build complete ==="
ls -lh "$DIST_DIR/"
echo ""
echo "To test: cd $DIST_DIR && python3 -m http.server 8080"
