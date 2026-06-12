#!/bin/bash
# Patch OCCT source for WASI compatibility.
# WASI lacks many POSIX features that OCCT expects on non-Windows platforms.
# This script copies stub headers and patches source files to work around those gaps.

OCCT_DIR="${1:-/OCCT}"
STUB_DIR="${OCCT_DIR}/wasi_stubs"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Copy stub headers into OCCT tree
echo "Copying WASI stub headers..."
cp -r "${SCRIPT_DIR}/wasi_stubs" "${OCCT_DIR}/"

# --- Source patches ---

echo "Patching OCCT source files for WASI..."

# Patch OSD_signal.cxx - add signal extension include (MUST come before all other includes)
SIGNAL_FILE="${OCCT_DIR}/src/FoundationClasses/TKernel/OSD/OSD_signal.cxx"
if [ -f "${SIGNAL_FILE}" ] && ! grep -q "wasi_signal_ext" "${SIGNAL_FILE}"; then
  sed -i '1i #ifdef __wasi__\n#include "wasi_signal_ext.h"\n#endif' "${SIGNAL_FILE}"
  echo "  Patched OSD_signal.cxx"
fi

# Patch OSD_File.cxx - add fcntl extension include and mkstemp stub
FILE_FILE="${OCCT_DIR}/src/FoundationClasses/TKernel/OSD/OSD_File.cxx"
if [ -f "${FILE_FILE}" ] && ! grep -q "wasi_fcntl_ext" "${FILE_FILE}"; then
  sed -i '1i #ifdef __wasi__\n#include "wasi_fcntl_ext.h"\nstatic inline int mkstemp(char* tpl) { (void)tpl; return -1; }\n#endif' "${FILE_FILE}"
  echo "  Patched OSD_File.cxx"
fi

# Patch OSD_Directory.cxx - add umask and mkdtemp stubs
DIR_FILE="${OCCT_DIR}/src/FoundationClasses/TKernel/OSD/OSD_Directory.cxx"
if [ -f "${DIR_FILE}" ] && ! grep -q "__wasi__" "${DIR_FILE}"; then
  sed -i '1i #ifdef __wasi__\n#include <sys/types.h>\nstatic inline mode_t umask(mode_t mask) { (void)mask; return 0; }\nstatic inline char* mkdtemp(char* tpl) { (void)tpl; return 0; }\n#endif' "${DIR_FILE}"
  echo "  Patched OSD_Directory.cxx"
fi

# Patch OSD_Path.cxx - force include our utsname stub
PATH_FILE="${OCCT_DIR}/src/FoundationClasses/TKernel/OSD/OSD_Path.cxx"
if [ -f "${PATH_FILE}" ] && ! grep -q "_WASI_STUB_UTSNAME" "${PATH_FILE}"; then
  sed -i '1i #ifdef __wasi__\n#include "sys/utsname.h"\n#endif' "${PATH_FILE}"
  echo "  Patched OSD_Path.cxx"
fi

# Patch OSD_Process.cxx - getuid() is not available in WASI (Emscripten provides it, WASI doesn't)
PROC_FILE="${OCCT_DIR}/src/FoundationClasses/TKernel/OSD/OSD_Process.cxx"
if [ -f "${PROC_FILE}" ] && ! grep -q "__wasi__" "${PROC_FILE}"; then
  sed -i '1i #ifdef __wasi__\n#include <sys/types.h>\nstatic inline uid_t getuid(void) { return 0; }\n#endif' "${PROC_FILE}"
  echo "  Patched OSD_Process.cxx"
fi

# Patch STEPConstruct_AP203Context.cxx - timezone is not available in WASI
STEP_FILE="${OCCT_DIR}/src/DataExchange/TKDESTEP/STEPConstruct/STEPConstruct_AP203Context.cxx"
if [ -f "${STEP_FILE}" ] && ! grep -q "__wasi__" "${STEP_FILE}"; then
  sed -i '1i #ifdef __wasi__\nstatic long timezone = 0;\n#endif' "${STEP_FILE}"
  echo "  Patched STEPConstruct_AP203Context.cxx"
fi

echo "WASI patching complete. Stub headers in: ${STUB_DIR}"
