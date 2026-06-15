#!/bin/bash
# Patch OCCT source for WASI compatibility.
# WASI lacks many POSIX features that OCCT expects on non-Windows platforms.
# This script copies stub headers and patches source files to work around those gaps.
#
# Source files are located by name (not by hardcoded path) so this works across
# OCCT source layouts: the flat src/<Package>/ of the 7.x series and the
# src/<Module>/<Toolkit>/<Package>/ tree introduced in 8.0. A target that can't
# be found is a hard error rather than a silent skip (a silent skip would let the
# OCCT build fail much later with a confusing missing-symbol error).
set -euo pipefail

OCCT_DIR="${1:-/OCCT}"
STUB_DIR="${OCCT_DIR}/wasi_stubs"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Ensure stub headers are in the OCCT tree at ${OCCT_DIR}/wasi_stubs.
# In the Docker build they're already placed there by a COPY; when running this
# script standalone from the repo they sit next to it and get copied in.
echo "Ensuring WASI stub headers are in place..."
if [ -d "${SCRIPT_DIR}/wasi_stubs" ] && [ "${SCRIPT_DIR}/wasi_stubs" != "${STUB_DIR}" ]; then
  cp -r "${SCRIPT_DIR}/wasi_stubs" "${OCCT_DIR}/"
fi
if [ ! -d "${STUB_DIR}" ]; then
  echo "ERROR: WASI stub headers not found at ${STUB_DIR}" >&2
  exit 1
fi

# patch_src <filename> <text> : find <filename> under the OCCT source tree and
# prepend <text> (guarded by #ifdef __wasi__). Idempotent on the __wasi__ marker.
patch_src() {
  local name="$1" text="$2" file
  file="$(find "${OCCT_DIR}/src" -name "$name" -type f | head -1)"
  if [ -z "$file" ]; then
    echo "ERROR: could not find $name under ${OCCT_DIR}/src" >&2
    exit 1
  fi
  if ! grep -q "__wasi__" "$file"; then
    sed -i "1i $text" "$file"
    echo "  Patched ${file#"${OCCT_DIR}/"}"
  fi
}

echo "Patching OCCT source files for WASI..."

# OSD_signal.cxx - signal extension include (must precede all other includes)
patch_src OSD_signal.cxx '#ifdef __wasi__\n#include "wasi_signal_ext.h"\n#endif'

# OSD_File.cxx - fcntl extension include and mkstemp stub
patch_src OSD_File.cxx '#ifdef __wasi__\n#include "wasi_fcntl_ext.h"\nstatic inline int mkstemp(char* tpl) { (void)tpl; return -1; }\n#endif'

# OSD_Directory.cxx - umask and mkdtemp stubs
patch_src OSD_Directory.cxx '#ifdef __wasi__\n#include <sys/types.h>\nstatic inline mode_t umask(mode_t mask) { (void)mask; return 0; }\nstatic inline char* mkdtemp(char* tpl) { (void)tpl; return 0; }\n#endif'

# OSD_Path.cxx - force include our utsname stub
patch_src OSD_Path.cxx '#ifdef __wasi__\n#include "sys/utsname.h"\n#endif'

# OSD_Process.cxx - getuid() is not available in WASI (Emscripten provides it, WASI doesn't)
patch_src OSD_Process.cxx '#ifdef __wasi__\n#include <sys/types.h>\nstatic inline uid_t getuid(void) { return 0; }\n#endif'

# STEPConstruct_AP203Context.cxx - timezone is not available in WASI
patch_src STEPConstruct_AP203Context.cxx '#ifdef __wasi__\nstatic long timezone = 0;\n#endif'

echo "WASI patching complete. Stub headers in: ${STUB_DIR}"
