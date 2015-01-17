#ifndef BASE64_H
#define BASE64_H

#include <string>

class Base64
{
public:
	static std::string encode(const std::string &prefix, const std::string &in, const char *encoding = NULL);
	static std::string decode(const std::string &prefix, const std::string &in, const char *encoding = NULL);

	static const char *FileNameEncoding;
	static const char *RVarEncoding;

};

#endif // BASE64_H
