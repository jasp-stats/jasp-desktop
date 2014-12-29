#ifndef UTILS_H
#define UTILS_H

#include <string>

class Utils
{
public:
	static std::wstring s2ws(const std::string &s);
	static std::string ws2s(const std::wstring &s);
	static void setEnv(const std::string &env, const std::string &value);
};

#endif // UTILS_H
