FROM debian:testing-slim
# Dockerized wasm build of opencascade-hs using wasi-sdk and ghc-wasm.
# Not intended as a reusable build environment - more to document the build
# process in a reproducible way.

SHELL ["/bin/bash", "-c"]

# install system dependencies
RUN apt-get update && \
    apt-get -y install git python3 xz-utils cmake curl jq zip zstd ca-certificates ninja-build \
    chromium npm && \
    mkdir -p /opt/puppeteer && cd /opt/puppeteer && \
    PUPPETEER_SKIP_DOWNLOAD=true npm install puppeteer
ENV PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium
ENV NODE_PATH=/opt/puppeteer/node_modules

# clone source dependencies, pinned to known-good commits.
# OCCT is pinned to master 2026-06-12 (8.0.0): earlier 8.0-dev commits have a
# BRepMesh node-binding bug (issue #929); the default Watson mesher is still
# broken for boolean-result faces there, so the playground selects Delabella
# (see playground/wasm_mesh_default.cpp). freetype/rapidjson masters as of
# 2026-04-01.
ENV OCCT_COMMIT=d3056ef80c9668f395da40f5fd7be186cae4501f \
    FREETYPE_COMMIT=07d8d50a63a45a7446b2fc44732baecc685f3e4c \
    RAPIDJSON_COMMIT=24b5e7a8b27f42fa16b96fc70aade9106cf7102f
RUN git clone --filter=tree:0 https://github.com/Open-Cascade-SAS/OCCT.git && \
    git -C OCCT checkout "$OCCT_COMMIT" && \
    git clone --filter=tree:0 https://github.com/freetype/freetype.git && \
    git -C freetype checkout "$FREETYPE_COMMIT" && \
    git clone --filter=tree:0 https://github.com/Tencent/rapidjson.git && \
    git -C rapidjson checkout "$RAPIDJSON_COMMIT"

# install ghc-wasm (provides wasi-sdk, wasmtime, ghc cross-compiler, cabal)
# Pin to a specific commit to avoid GHC version drift breaking wasm EH.
# The wasm exception handling fix works with GHC 9.14.1.20260213 but regressed
# with 20260330. We pin to a commit that gives us the working version.
ENV GHC_WASM_META_COMMIT=61a4baf7
RUN cd /tmp && \
    curl -f -L --retry 5 "https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/archive/${GHC_WASM_META_COMMIT}/ghc-wasm-meta.tar.gz" | tar xz --strip-components=1 && \
    ./setup.sh

# copy WASI stubs and OCCT build scripts (needed by freetype and OCCT builds)
WORKDIR /
COPY scripts/occt/wasi_stubs OCCT/wasi_stubs
COPY scripts/occt/wasm_build.sh OCCT/adm/scripts/wasm_build.sh
COPY scripts/occt/wasm_custom.sh OCCT/adm/scripts/wasm_custom.sh
COPY scripts/occt/wasm_patch.sh OCCT/adm/scripts/wasm_patch.sh

# build freetype for wasm
RUN source ~/.ghc-wasm/env && \
    mkdir -p /freetype/build && cd /freetype/build && \
    cmake -DCMAKE_TOOLCHAIN_FILE=~/.ghc-wasm/wasi-sdk/share/cmake/wasi-sdk.cmake \
      -DCMAKE_BUILD_TYPE=Release \
      -DBUILD_SHARED_LIBS=OFF \
      -DCMAKE_C_FLAGS="-fPIC -D__EMSCRIPTEN__ -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS -D_WASI_EMULATED_MMAN -D_WASI_EMULATED_GETPID -I/OCCT/wasi_stubs" \
      -DCMAKE_INSTALL_PREFIX=/freetype/install \
      .. && \
    make -j4 && make install && \
    cp /freetype/install/lib/*.a ~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi/

# build libunwind + libc++abi with wasm exception support
# These replace the exception-less versions in the sysroot to enable
# C++ throw/catch in OCCT code running inside wasm shared libraries
RUN source ~/.ghc-wasm/env && \
    SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot && \
    git clone --depth 1 --filter=blob:none --sparse \
      https://gitlab.haskell.org/haskell-wasm/llvm-project.git /tmp/llvm && \
    cd /tmp/llvm && git sparse-checkout set libunwind libcxx libcxxabi cmake runtimes

RUN source ~/.ghc-wasm/env && \
    SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot && \
    BASE="-fPIC -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false -fdeclspec --sysroot=$SYSROOT -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_MMAN -Wno-c23-extensions" && \
    cmake -G "Unix Makefiles" -B /tmp/build-unwind \
      -DCMAKE_C_COMPILER=wasm32-wasi-clang -DCMAKE_CXX_COMPILER=wasm32-wasi-clang++ \
      -DCMAKE_SYSTEM_NAME=WASI -DCMAKE_C_COMPILER_WORKS=ON -DCMAKE_CXX_COMPILER_WORKS=ON \
      -DCMAKE_C_FLAGS="$BASE" -DCMAKE_CXX_FLAGS="$BASE" \
      -DLIBUNWIND_ENABLE_SHARED=OFF -DLIBUNWIND_ENABLE_STATIC=ON \
      -DLIBUNWIND_ENABLE_THREADS=OFF -DLIBUNWIND_USE_COMPILER_RT=ON \
      -S /tmp/llvm/libunwind && \
    make -C /tmp/build-unwind -j4 && \
    cp /tmp/build-unwind/lib/libunwind.a "$SYSROOT/lib/wasm32-wasi/" && \
    ls -lh "$SYSROOT/lib/wasm32-wasi/libunwind.a"

# Patch cxa_personality.cpp to use __memory_base for DW_EH_PE_datarel on wasm.
# Without this, scan_eh_tab uses base=0 for typeinfo lookup, causing OOB when
# catching derived exception types in PIC shared libraries.
RUN python3 /OCCT/wasi_stubs/cxa_personality_patch.py /tmp/llvm/libcxxabi/src/cxa_personality.cpp

RUN source ~/.ghc-wasm/env && \
    SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot && \
    CXX_INC1="$SYSROOT/include/wasm32-wasi/c++/v1" && \
    CXX_INC2="$SYSROOT/include/c++/v1" && \
    BASE="-fPIC -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false -fdeclspec --sysroot=$SYSROOT -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_MMAN -Wno-c23-extensions" && \
    cmake -G "Unix Makefiles" -B /tmp/build-cxxabi \
      -DCMAKE_C_COMPILER=wasm32-wasi-clang -DCMAKE_CXX_COMPILER=wasm32-wasi-clang++ \
      -DCMAKE_SYSTEM_NAME=WASI -DCMAKE_C_COMPILER_WORKS=ON -DCMAKE_CXX_COMPILER_WORKS=ON \
      -DCMAKE_C_FLAGS="$BASE -I/tmp/llvm/libunwind/include" \
      -DCMAKE_CXX_FLAGS="$BASE -I/tmp/llvm/libunwind/include -isystem $CXX_INC1 -isystem $CXX_INC2" \
      -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_STATIC=ON \
      -DLIBCXXABI_ENABLE_THREADS=OFF -DLIBCXXABI_ENABLE_EXCEPTIONS=ON \
      -DLIBCXXABI_USE_COMPILER_RT=ON -DLIBCXXABI_USE_LLVM_UNWINDER=OFF \
      -DLIBCXXABI_LIBCXX_INCLUDES="$CXX_INC1" \
      -S /tmp/llvm/libcxxabi && \
    make -C /tmp/build-cxxabi -j4 && \
    cp /tmp/build-cxxabi/lib/libc++abi.a "$SYSROOT/lib/wasm32-wasi/" && \
    rm -rf /tmp/llvm /tmp/build-unwind /tmp/build-cxxabi

# patch OCCT source for WASI compatibility (stub headers + source patches)
RUN chmod +x ./OCCT/adm/scripts/wasm_patch.sh && ./OCCT/adm/scripts/wasm_patch.sh /OCCT

# build OCCT for wasm (with -fwasm-exceptions for real C++ exception handling)
RUN chmod +x ./OCCT/adm/scripts/wasm_build.sh && \
    ./OCCT/adm/scripts/wasm_build.sh || (cat /OCCT/work/build-wasm.log && false)

# install OCCT libs into wasi-sdk sysroot for linking
RUN source ~/.ghc-wasm/env && \
    SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi && \
    cp /OCCT/work/wasm/lib/*.a "$SYSROOT/"

# copy in opencascade-hs source and build
COPY . /opencascade-hs
WORKDIR /opencascade-hs
# Build library packages only (not waterfall-cad-examples executable, which
# can't link with -fwasm-exceptions due to __cpp_exception tag limitation)
RUN source ~/.ghc-wasm/env && \
    wasm32-wasi-cabal build opencascade-hs waterfall-cad waterfall-cad-svg

# build the browser playground
RUN chmod +x playground/build_playground.sh && playground/build_playground.sh
