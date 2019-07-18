#ifndef READSTAT_WINDOWS_HELPER_H
#define READSTAT_WINDOWS_HELPER_H

#if defined _WIN32
#include <stdint.h>
//following are missing from windows and are substituted by MingW for readstat lib.
typedef int64_t _off64_t;
typedef long ssize_t;
#endif

#endif // READSTAT_WINDOWS_HELPER_H
