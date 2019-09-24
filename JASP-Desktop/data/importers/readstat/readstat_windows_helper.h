#ifndef READSTAT_WINDOWS_HELPER_H
#define READSTAT_WINDOWS_HELPER_H

#ifdef WIN32
extern "C"
{
#include <stdint.h>
//following are missing from windows and are substituted by MingW for readstat lib.
typedef long long _off64_t;
typedef __int64 ssize_t;
}
#endif

#endif // READSTAT_WINDOWS_HELPER_H
