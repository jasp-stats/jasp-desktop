#include "utils.h"

#ifdef __WIN32__
#include "windows.h"
#endif

using namespace std;

#ifdef __WIN32__

wstring Utils::s2ws(const string &s)
{
	int len;
	int slength = (int)s.length() + 1;
	len = MultiByteToWideChar(CP_ACP, 0, s.c_str(), slength, 0, 0);
	wchar_t* buf = new wchar_t[len];
	MultiByteToWideChar(CP_ACP, 0, s.c_str(), slength, buf, len);
	wstring r(buf);
	delete[] buf;
	return r;
}

string Utils::ws2s(const wstring &s)
{
	char defaultChar = ' ';

	int len;
	int slength = (int)s.length() + 1;
	len = WideCharToMultiByte(CP_ACP, 0, s.c_str(), slength, 0, 0, &defaultChar, NULL);
	char* buf = new char[len];
	WideCharToMultiByte(CP_ACP, 0, s.c_str(), slength, buf, len, &defaultChar, NULL);
	string r(buf);
	delete[] buf;
	return r;
}

void Utils::setEnv(const string &env, const string &value)
{
#ifdef __WIN32__

	wstring wenv = Utils::s2ws(env);
	wstring wvalue = Utils::s2ws(value);

	SetEnvironmentVariableW(wenv.c_str(), wvalue.c_str());

#else
	::setenv(env.c_str(), value.c_str(), value.size());
#endif
}

#endif
