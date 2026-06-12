#ifndef _WASI_STUB_NETDB_H
#define _WASI_STUB_NETDB_H
struct hostent { char *h_name; char **h_aliases; int h_addrtype; int h_length; char **h_addr_list; };
static inline struct hostent* gethostbyname(const char* name) { (void)name; return 0; }
static inline int gethostname(char* name, int len) { if (len > 0) name[0] = 0; return -1; }
#endif
