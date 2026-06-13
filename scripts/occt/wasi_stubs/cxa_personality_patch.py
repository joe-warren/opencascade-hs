#!/usr/bin/env python3
"""Patch cxa_personality.cpp for wasm PIC shared libraries:
1. Use __memory_base for DW_EH_PE_datarel base
2. Add debug output to scan_eh_tab
"""
import sys

src = sys.argv[1]
with open(src) as f:
    content = f.read()

# Patch 1: Use __memory_base for datarel base
old1 = """#if defined(_AIX)
    uintptr_t base = _Unwind_GetDataRelBase(context);
#else
    uintptr_t base = 0;
#endif"""

new1 = """#if defined(_AIX)
    uintptr_t base = _Unwind_GetDataRelBase(context);
#elif defined(__wasm__)
    extern uintptr_t __memory_base;
    uintptr_t base = __memory_base;
#else
    uintptr_t base = 0;
#endif"""

# Patch 2: Add debug to get_shim_type_info (where typeinfo is decoded)
old2 = """    return (const __shim_type_info*)readEncodedPointer(&classInfo,
                                                       ttypeEncoding, base);
}"""

new2 = """    uintptr_t raw = readEncodedPointer(&classInfo, ttypeEncoding, base);
#ifdef __wasm__
    {
        extern uintptr_t __memory_base;
        // Debug: inspect the typeinfo data at the resolved address
        if (raw != 0) {
            const uint32_t* data = (const uint32_t*)raw;
            fprintf(stderr, "typeinfo at 0x%x: vtable_ptr=0x%x name_ptr=0x%x base_ptr=0x%x __memory_base=0x%x\\n",
                    (unsigned)raw, data[0], data[1], data[2], (unsigned)__memory_base);
        }
    }
#endif
    return (const __shim_type_info*)raw;
}"""

# Patch 3: Add include for fprintf
old3 = """#include "cxa_exception.h"
"""

new3 = """#include "cxa_exception.h"
#include <cstdio>
"""

patched = False
if old1 in content:
    content = content.replace(old1, new1)
    patched = True

if old2 in content:
    content = content.replace(old2, new2)
    patched = True

if old3 in content and '<cstdio>' not in content:
    content = content.replace(old3, new3)
    patched = True

if patched:
    with open(src, 'w') as f:
        f.write(content)
    print("Patched cxa_personality.cpp with __memory_base + debug output")
else:
    print("WARNING: some patches didn't apply")
