/* Stub emscripten.h for building OCCT with wasi-sdk.
 * We define __EMSCRIPTEN__ to trigger OCCT's wasm code paths,
 * but we're not actually using Emscripten - just its preprocessor guards.
 * This header provides no-op stubs for any Emscripten APIs OCCT may reference.
 */
#ifndef _WASI_STUB_EMSCRIPTEN_H
#define _WASI_STUB_EMSCRIPTEN_H

#define EMSCRIPTEN_KEEPALIVE

#define EM_ASM(...)
#define EM_ASM_INT(...) 0
#define EM_ASM_DOUBLE(...) 0.0

/* EM_JS(ret_type, name, params, body) - defines a C function that would call JS.
 * We stub it to declare a function returning a default value. */
#define EM_JS(ret, name, params, ...) \
  static inline ret name params { return (ret)0; }

/* Emscripten logging flags */
#define EM_LOG_C_STACK        1
#define EM_LOG_DEMANGLE       4
#define EM_LOG_NO_PATHS       8
#define EM_LOG_FUNC_PARAMS    16

static inline void emscripten_set_main_loop(void (*func)(void), int fps, int simulate_infinite_loop) {
  (void)func; (void)fps; (void)simulate_infinite_loop;
}
static inline void emscripten_cancel_main_loop(void) {}

static inline int emscripten_get_callstack(int flags, char* out, int maxbytes) {
  (void)flags; if (out && maxbytes > 0) out[0] = 0; return 0;
}

static inline char* emscripten_get_preloaded_image_data(const char* path, int* w, int* h) {
  (void)path; if (w) *w = 0; if (h) *h = 0; return 0;
}

#endif
