/* Minimal C++ exception runtime for wasi-sdk.
 *
 * With -fexceptions, clang generates setjmp/longjmp-based exception handling.
 * The __cxa_* functions manage exception objects. This implementation provides
 * just enough to make throw/catch work for OCCT's internal error handling
 * (e.g. BRep_Tool::Parameter throwing Standard_NoSuchObject).
 *
 * Thread-local exception state is stored in a simple global (wasm is
 * single-threaded in our use case).
 */
#include <stdlib.h>
#include <string.h>

/* Exception object header - must match the ABI layout that clang expects */
struct __cxa_exception {
    void *exceptionType;
    void (*exceptionDestructor)(void *);
    /* The thrown object follows immediately after this header */
};

/* Global exception state (single-threaded) */
static struct {
    struct __cxa_exception *current;
    int uncaught_count;
} __cxa_eh_state = {0, 0};

void *__cxa_allocate_exception(unsigned long size) {
    /* Allocate header + thrown object */
    struct __cxa_exception *ex = (struct __cxa_exception *)malloc(
        sizeof(struct __cxa_exception) + size);
    if (!ex) abort();
    memset(ex, 0, sizeof(struct __cxa_exception) + size);
    /* Return pointer to the thrown object (after the header) */
    return (void *)(ex + 1);
}

void __cxa_free_exception(void *thrown_object) {
    if (thrown_object) {
        struct __cxa_exception *ex =
            ((struct __cxa_exception *)thrown_object) - 1;
        free(ex);
    }
}

void __cxa_throw(void *thrown_exception, void *tinfo, void (*dest)(void *)) {
    struct __cxa_exception *ex =
        ((struct __cxa_exception *)thrown_exception) - 1;
    ex->exceptionType = tinfo;
    ex->exceptionDestructor = dest;
    __cxa_eh_state.current = ex;
    __cxa_eh_state.uncaught_count++;

    /* In the setjmp/longjmp EH model, __cxa_throw is never actually called
     * directly - the compiler generates invoke/landingpad pairs that use
     * setjmp/longjmp. But if we get here somehow, abort. */
    abort();
}

void *__cxa_begin_catch(void *exn) {
    /* exn is the pointer to the thrown object (after the header) */
    __cxa_eh_state.uncaught_count--;
    __cxa_eh_state.current = ((struct __cxa_exception *)exn) - 1;
    return exn;
}

void __cxa_end_catch(void) {
    if (__cxa_eh_state.current) {
        struct __cxa_exception *ex = __cxa_eh_state.current;
        if (ex->exceptionDestructor) {
            ex->exceptionDestructor((void *)(ex + 1));
        }
        free(ex);
        __cxa_eh_state.current = 0;
    }
}

void __cxa_rethrow(void) {
    abort();
}

void *__cxa_get_exception_ptr(void *exn) {
    return exn;
}
