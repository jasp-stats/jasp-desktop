#ifndef JSONUTILITIES_H
#define JSONUTILITIES_H

#include "jsonredirect.h"
#include <string>
#include <set>


class JsonUtilities
{
public:
	JsonUtilities() {}

	static std::set<std::string> convertEasyFilterJSONToSet(std::string jsonStr);
};

#endif // JSONUTILITIES_H
