#ifndef JASPSHAREDMEM_GLOBAL_H
#define JASPSHAREDMEM_GLOBAL_H


#include <QtCore/qglobal.h>

#if defined(JASPSHAREDMEM_LIBRARY)
#  define JASPSHAREDMEMSHARED_EXPORT Q_DECL_EXPORT
#else
#  define JASPSHAREDMEMSHARED_EXPORT Q_DECL_IMPORT
#endif

#ifdef __WIN32__
#define STDCALL __stdcall
#else
#define STDCALL
#endif

#endif // JASPSHAREDMEM_GLOBAL_H
