#ifndef BASE64_H
#define BASE64_H

#include <string>

#include "base64/cencode.h"

class Base64
{
public:
	static std::string encode(std::string &prefix, std::string &in);

};

#endif // BASE64_H
