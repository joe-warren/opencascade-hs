FROM debian:testing-slim AS toolchain
# Dockerized wasm build of opencascade-hs
# Only recommended for building the wasm playground

SHELL ["/bin/bash", "-c"]

# install system dependencies
RUN apt-get update && \
    apt-get -y install git python3 xz-utils cmake curl jq zip zstd ca-certificates ninja-build \
    chromium npm && \
    mkdir -p /opt/puppeteer && cd /opt/puppeteer && \
    PUPPETEER_SKIP_DOWNLOAD=true npm install puppeteer
ENV PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium
ENV NODE_PATH=/opt/puppeteer/node_modules

WORKDIR /

# clone source dependencies, pinned to known-good refs.
ENV FREETYPE_COMMIT=07d8d50a63a45a7446b2fc44732baecc685f3e4c \
    RAPIDJSON_COMMIT=24b5e7a8b27f42fa16b96fc70aade9106cf7102f
RUN git clone --filter=tree:0 https://github.com/joe-warren/OCCT-Wasi-Fork.git OCCT && \
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

WORKDIR /

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

# build OCCT for wasm (with -fwasm-exceptions for real C++ exception handling)
RUN chmod +x ./OCCT/adm/scripts/wasm_build.sh && \
    ./OCCT/adm/scripts/wasm_build.sh || (cat /OCCT/work/build-wasm.log && false)

# install OCCT libs into wasi-sdk sysroot for linking
RUN source ~/.ghc-wasm/env && \
    SYSROOT=~/.ghc-wasm/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi && \
    cp /OCCT/work/wasm/lib/*.a "$SYSROOT/"

FROM toolchain AS playground

# copy in opencascade-hs source and build
COPY . /opencascade-hs
WORKDIR /opencascade-hs
# Build library packages only (not waterfall-cad-examples executable, which
# can't link with -fwasm-exceptions due to __cpp_exception tag limitation)
RUN source ~/.ghc-wasm/env && \
    wasm32-wasi-cabal build opencascade-hs waterfall-cad waterfall-cad-svg

# build the browser playground
RUN chmod +x playground/build_playground.sh && playground/build_playground.sh
