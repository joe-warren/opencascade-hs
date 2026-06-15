#!/bin/bash

# Auxiliary script for semi-automated building of OCCT for WASM platform.
# wasm_custom.sh should be configured with paths to CMake, 3rd-parties and Emscripten SDK.
# FreeType should be specified as mandatory dependency.

export aScriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
export aSrcRoot="${aScriptDir}/../.."
export aBuildRoot=work

# Limit parallelism to avoid OOM in Docker - wasm compilation is memory-heavy
export aNbJobs=${OCCT_BUILD_JOBS:-4}

export toCMake=1
export toClean=0
export toMake=1
export toInstall=1

export BUILD_ModelingData=ON
export BUILD_ModelingAlgorithms=ON
export BUILD_Visualization=ON
export BUILD_ApplicationFramework=ON
export BUILD_DataExchange=ON

if [ -f "${aScriptDir}/wasm_custom.sh" ] ; then
  . "${aScriptDir}/wasm_custom.sh"
fi
source ~/.ghc-wasm/env

export aToolchain=~/.ghc-wasm/wasi-sdk/share/cmake/wasi-sdk.cmake

# WASI emulation flags needed for OCCT's POSIX dependencies
# Define __EMSCRIPTEN__ to trigger OCCT's existing wasm code paths (no X11, no fontconfig, etc.)
# The wasi_stubs directory provides any Emscripten-specific headers that OCCT may reference.
export aWasiCFlags="-fPIC -D__EMSCRIPTEN__ -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS -D_WASI_EMULATED_MMAN -D_WASI_EMULATED_GETPID -I${aSrcRoot}/wasi_stubs"
# Exe link flags: OCCT 7.9 builds host tools (e.g. ExpToCasExe) as wasm executables.
# Everything is compiled -fPIC + -fwasm-exceptions, which makes C++ exceptions
# *import* the __cpp_exception tag (the playground's dynamic loader supplies it at
# runtime; a standalone executable has nothing to). -lc++abi/-lunwind resolve the
# _Unwind_* / cxa symbols, and --allow-undefined lets the unsatisfiable tag become
# a harmless import so the tool links. Only the static libraries are consumed
# downstream -- these executables are built but never run.
export aWasiLinkFlags="-Wl,--allow-undefined -lc++abi -lunwind -lwasi-emulated-signal -lwasi-emulated-process-clocks -lwasi-emulated-mman -lwasi-emulated-getpid"

export aGitBranch=`git symbolic-ref --short HEAD`

echo "Compilation OCCT branch : $aGitBranch"

export aPlatformAndCompiler=wasm

export aWorkDir="${aSrcRoot}/${aBuildRoot}/${aPlatformAndCompiler}-make"
if [ ! -d "${aWorkDir}" ]; then
  mkdir -p "${aWorkDir}"
fi

export aDestDir="${aSrcRoot}/${aBuildRoot}/${aPlatformAndCompiler}"
if [ ! -d "${aDestDir}" ]; then
  mkdir -p "${aDestDir}"
fi

export aLogFile="${aSrcRoot}/${aBuildRoot}/build-${aPlatformAndCompiler}.log"
if [ -f "${aLogFile}" ]; then
  rm "${aLogFile}"
fi

echo Start building OCCT for ${aPlatformAndCompiler}
echo Start building OCCT for ${aPlatformAndCompiler}>> "${aLogFile}"

pushd "${aWorkDir}"
pwd
echo toCMake=${toCMake}
if [ "${toCMake}" = "1" ]; then

echo "Configuring OCCT for WASM..."
echo "Configuring OCCT for WASM with wasi-sdk..."
echo "WASI C/CXX flags: ${aWasiCFlags}"
echo "WASI link flags: ${aWasiLinkFlags}"

cmake -G "Unix Makefiles" -DCMAKE_TOOLCHAIN_FILE:FILEPATH="${aToolchain}" \
-DCMAKE_BUILD_TYPE:STRING="Release" \
-DBUILD_LIBRARY_TYPE:STRING="Static" \
-DCMAKE_C_FLAGS:STRING="${aWasiCFlags}" \
-DCMAKE_CXX_FLAGS:STRING="${aWasiCFlags}" \
-DCMAKE_EXE_LINKER_FLAGS:STRING="${aWasiLinkFlags}" \
-DCMAKE_STATIC_LINKER_FLAGS:STRING="" \
-DINSTALL_DIR:PATH="${aDestDir}" \
-DINSTALL_DIR_INCLUDE:STRING="inc" \
-DINSTALL_DIR_RESOURCE:STRING="src" \
-D3RDPARTY_FREETYPE_DIR:PATH="$aFreeType" \
-D3RDPARTY_FREETYPE_INCLUDE_DIR_freetype2:FILEPATH="$aFreeType/include" \
-D3RDPARTY_FREETYPE_INCLUDE_DIR_ft2build:FILEPATH="$aFreeType/include" \
-DUSE_RAPIDJSON:BOOL="ON" \
-D3RDPARTY_RAPIDJSON_DIR:PATH="$aRapidJson" \
-D3RDPARTY_RAPIDJSON_INCLUDE_DIR:PATH="$aRapidJson/include" \
-DUSE_DRACO:BOOL="OFF" \
-D3RDPARTY_DRACO_DIR:PATH="$aDraco" \
-D3RDPARTY_DRACO_INCLUDE_DIR:FILEPATH="$aDraco/include" \
-D3RDPARTY_DRACO_LIBRARY_DIR:PATH="$aDraco/lib" \
-DBUILD_MODULE_FoundationClasses:BOOL="ON" \
-DBUILD_MODULE_ModelingData:BOOL="${BUILD_ModelingData}" \
-DBUILD_MODULE_ModelingAlgorithms:BOOL="${BUILD_ModelingAlgorithms}" \
-DBUILD_MODULE_Visualization:BOOL="${BUILD_Visualization}" \
-DBUILD_MODULE_ApplicationFramework:BOOL="${BUILD_ApplicationFramework}" \
-DBUILD_MODULE_DataExchange:BOOL="${BUILD_DataExchange}" \
-DBUILD_MODULE_Draw:BOOL="OFF" \
-DBUILD_DOC_Overview:BOOL="OFF" \
-DUSE_XLIB:BOOL="OFF" \
-DUSE_GLES2:BOOL="OFF" \
-DUSE_OPENGL:BOOL="OFF" \
-DBUILD_RELEASE_DISABLE_EXCEPTIONS="ON" \
"${aSrcRoot}"

  if [ $? -ne 0 ]; then
    echo "Problem during configuration"
    popd
    exit 1
  fi

fi

if [ "${toClean}" = "1" ]; then
  make clean
fi

if [ "${toMake}" = "1" ]; then
  echo Building...
  make -j ${aNbJobs} 2>> "${aLogFile}"
  if [ $? -ne 0 ]; then
    echo "Problem during make operation"
    popd
    exit 1
  fi
  echo "${aLogFile}"
fi

if [ "${toInstall}" = "1" ]; then
  echo Installing into ${aDestDir}
  make install 2>> "${aLogFile}"
fi

popd
