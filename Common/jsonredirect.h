#ifndef JSONREDIRECT_H
#define JSONREDIRECT_H


//This was a terrible attempt to automatically get the proper json location. CMake should obviate the need for this file entirely.
//Sorry for the merge conflict Amir ;)

#ifdef JASP_LIBJSON_STATIC
#include "lib_json/json.h"
#else
#ifdef LIBJSON_DIR_UP
#include <json/json.h>
#else
#ifdef LIBJSON_DIR_CPP
#include <jsoncpp/json/json.h>
#else
#include <libjson/json/json.h>
#endif
#endif
#endif

#endif // JSONREDIRECT_H
