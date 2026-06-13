#ifndef _WASI_STUB_MALLOC_H
#define _WASI_STUB_MALLOC_H

/* Include the real malloc functions */
#include <stdlib.h>

/* mallinfo is not available in WASI - provide a stub */
struct mallinfo {
  int arena;
  int ordblks;
  int smblks;
  int hblks;
  int hblkhd;
  int usmblks;
  int fsmblks;
  int uordblks;
  int fordblks;
  int keepcost;
};

static inline struct mallinfo mallinfo(void) {
  struct mallinfo mi = {0};
  return mi;
}

#endif
