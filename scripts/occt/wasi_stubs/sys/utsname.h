#ifndef _WASI_STUB_UTSNAME_H
#define _WASI_STUB_UTSNAME_H
struct utsname {
  char sysname[256];
  char nodename[256];
  char release[256];
  char version[256];
  char machine[256];
};
static inline int uname(struct utsname *buf) {
  if (buf) {
    buf->sysname[0] = 0;
    buf->nodename[0] = 0;
    buf->release[0] = 0;
    buf->version[0] = 0;
    buf->machine[0] = 0;
  }
  return 0;
}
#endif
