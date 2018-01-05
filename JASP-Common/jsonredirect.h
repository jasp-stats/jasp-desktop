#ifndef JSONREDIRECT_H
#define JSONREDIRECT_H

#ifdef JASP_NOT_LINUX
#include "lib_json/json.h"
#else
#include <jsoncpp/json/json.h>
#endif

#endif // JSONREDIRECT_H
