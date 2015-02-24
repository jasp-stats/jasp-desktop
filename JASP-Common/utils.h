#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>

class Utils
{
public:

	static void setEnv(const std::string &env, const std::string &value);

#ifdef __WIN32__
	static std::wstring s2ws(const std::string &s);
	static std::string ws2s(const std::wstring &s);
#endif

	static long currentMillis();
	static long currentSeconds();
	static long getFileModificationTime(const std::string &filename);
	static void touch(const std::string &filename);

	static void remove(std::vector<std::string> &target, const std::vector<std::string> &toRemove);
};

#endif // UTILS_H
