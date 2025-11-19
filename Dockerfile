FROM debian:testing-slim
## This is a dockerized build of my (currently failing) attempt to get opencascade-hs to build to wasm
# docker isn't a tremendously sensible way to automate this, but I barely know nix
# the purpose of this is more to document "what I'm doing" so I can share errors in a repeatable way
# as opposed to being the basis of a reusable build environment

# use bash as the shell, we're going to be using `source` a lot
SHELL ["/bin/bash", "-c"]

# install system dependencies
RUN apt-get update
RUN apt-get -y install git python3 xz-utils cmake curl jq zip zstd

# setup ssh key (not used)
RUN mkdir ~/.ssh
RUN ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts

# clone source dependencies
RUN git clone --depth 1 https://github.com/Open-Cascade-SAS/OCCT.git

RUN git clone --depth 1 https://github.com/freetype/freetype.git

RUN git clone --depth 1 https://github.com/Tencent/rapidjson.git

RUN git clone --depth 1 https://github.com/emscripten-core/emsdk.git

# install and activate emsdk
WORKDIR /emsdk
RUN ./emsdk install latest
RUN ./emsdk activate latest
RUN source ./emsdk_env.sh

# overwrite opencascade build scripts
WORKDIR /
COPY scripts/occt/wasm_build.sh OCCT/adm/scripts/wasm_build.sh
COPY scripts/occt/wasm_custom.sh OCCT/adm/scripts/wasm_custom.sh

# run opencascade wasm build
RUN chmod +x ./OCCT/adm/scripts/wasm_build.sh
RUN ./OCCT/adm/scripts/wasm_build.sh

# install ghc-wasm
RUN curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | sh

# copy in opencascade-hs source 
RUN mkdir opencascade-hs
COPY . /opencascade-hs

# attempt to build opencascade-hs
WORKDIR /opencascade-hs
RUN source ~/.ghc-wasm/env && wasm32-wasi-cabal build all