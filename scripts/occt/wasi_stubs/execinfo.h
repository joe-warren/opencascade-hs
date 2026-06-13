#ifndef _WASI_STUB_EXECINFO_H
#define _WASI_STUB_EXECINFO_H
static inline int backtrace(void **buffer, int size) { (void)buffer; (void)size; return 0; }
static inline char** backtrace_symbols(void *const *buffer, int size) { (void)buffer; (void)size; return 0; }
static inline void backtrace_symbols_fd(void *const *buffer, int size, int fd) { (void)buffer; (void)size; (void)fd; }
#endif
