/* Stub setjmp.h for WASI without wasm exception handling.
 * With No_Exception defined, OCCT doesn't use setjmp/longjmp,
 * but Standard_ErrorHandler.hxx still includes this header.
 */
#ifndef _WASI_STUB_SETJMP_H
#define _WASI_STUB_SETJMP_H

typedef long jmp_buf[8];

static inline int setjmp(jmp_buf env) { (void)env; return 0; }
static inline void longjmp(jmp_buf env, int val) { (void)env; (void)val; __builtin_trap(); }

#endif
