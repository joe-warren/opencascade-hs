#ifndef _WASI_STUB_PWD_H
#define _WASI_STUB_PWD_H
#include <sys/types.h>
struct passwd { char *pw_name; char *pw_passwd; uid_t pw_uid; gid_t pw_gid; char *pw_dir; char *pw_shell; };
static inline struct passwd* getpwuid(uid_t uid) { (void)uid; return 0; }
static inline uid_t getuid(void) { return 0; }
#endif
