#ifndef COMMON_H
#define COMMON_H

#if __GNUC__ == 4 && __GNUC_MINOR__ == 6
#define OVERRIDE
#else
#define OVERRIDE override
#endif

#endif // COMMON_H
