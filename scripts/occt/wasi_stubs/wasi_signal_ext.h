#ifndef _WASI_SIGNAL_EXT_H
#define _WASI_SIGNAL_EXT_H

#include <signal.h>
#include <string.h>

/* Define sigset_t if not already provided */
#ifndef __DEFINED_sigset_t
typedef unsigned long __wasi_sigset_t;
#define sigset_t __wasi_sigset_t
#define __DEFINED_sigset_t
#endif

#ifndef SA_RESTART
#define SA_RESTART 0
#endif
#ifndef SA_SIGINFO
#define SA_SIGINFO 0
#endif
#ifndef SIG_UNBLOCK
#define SIG_UNBLOCK 1
#endif
#ifndef SIG_BLOCK
#define SIG_BLOCK 0
#endif
#ifndef SIG_SETMASK
#define SIG_SETMASK 2
#endif

/* siginfo_t stub */
typedef struct {
  int si_signo;
  int si_code;
  int si_errno;
  void *si_addr;
} siginfo_t;

struct sigaction {
  union {
    void (*sa_handler)(int);
    void (*sa_sigaction)(int, siginfo_t*, void*);
  };
  sigset_t sa_mask;
  int sa_flags;
};

static inline int sigaction(int sig, const struct sigaction *act, struct sigaction *oldact) {
  (void)sig; (void)act; (void)oldact;
  return 0;
}
static inline int sigemptyset(sigset_t *set) { if (set) *set = 0; return 0; }
static inline int sigfillset(sigset_t *set) { if (set) *set = ~0UL; return 0; }
static inline int sigaddset(sigset_t *set, int signo) { (void)set; (void)signo; return 0; }
static inline int sigdelset(sigset_t *set, int signo) { (void)set; (void)signo; return 0; }
static inline int sigprocmask(int how, const sigset_t *set, sigset_t *oldset) {
  (void)how; (void)set; (void)oldset; return 0;
}

#endif
