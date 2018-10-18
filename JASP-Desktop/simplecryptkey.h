#ifndef SIMPLECRYPTKEY //Can be set from an environment variable as well, the value here is only for testing!

#ifdef ENVIRONMENT_CRYPTKEY
#define SIMPLECRYPTKEY (ENVIRONMENT_CRYPTKEY)
#else
#define SIMPLECRYPTKEY (0x0c2ad4a4acb9f023)
#endif

#endif

