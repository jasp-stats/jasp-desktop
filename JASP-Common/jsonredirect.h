#ifndef JSONREDIRECT_H
#define JSONREDIRECT_H

#ifdef JASP_LIBJSON_STATIC
#include "lib_json/json.h"
#else
#include <jsoncpp/json/json.h>
#endif

#endif // JSONREDIRECT_H
