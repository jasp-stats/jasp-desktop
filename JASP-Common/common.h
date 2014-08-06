#ifndef COMMON_H
#define COMMON_H

#if __GNUC__ == 4 && __GNUC_MINOR__ == 6
#define OVERRIDE
#else
#define OVERRIDE override
#endif

#if _WIN64 || __amd64__
#define ARCH_64
#else
#define ARCH_32
#endif

typedef unsigned int uint;

#endif // COMMON_H
