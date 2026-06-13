#ifndef _WASI_STUB_SYSLOG_H
#define _WASI_STUB_SYSLOG_H
#define LOG_EMERG   0
#define LOG_ALERT   1
#define LOG_CRIT    2
#define LOG_ERR     3
#define LOG_WARNING 4
#define LOG_NOTICE  5
#define LOG_INFO    6
#define LOG_DEBUG   7
#define LOG_USER    (1<<3)
#define LOG_PID     0x01
#define LOG_NDELAY  0x08
static inline void openlog(const char *ident, int option, int facility) { (void)ident; (void)option; (void)facility; }
static inline void syslog(int priority, const char *format, ...) { (void)priority; (void)format; }
static inline void closelog(void) {}
#endif
