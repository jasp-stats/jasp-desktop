#include "utils.h"

#ifdef __WIN32__
#include "windows.h"
#else
#include <sys/stat.h>
#include <utime.h>
#endif

#include <boost/date_time/posix_time/posix_time.hpp>

using namespace std;
using namespace boost::posix_time;

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

#endif

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

long Utils::currentMillis()
{
	ptime epoch(boost::gregorian::date(1970,1,1));
	ptime t = microsec_clock::local_time();
	time_duration elapsed = t - epoch;

	return elapsed.total_milliseconds();
}

long Utils::currentSeconds()
{
	time_t now;
	time(&now);

	return now;
}

long Utils::getFileModificationTime(const std::string &filename)
{
#ifdef __WIN32__

	wstring wfilename = Utils::s2ws(filename);
	HANDLE file = CreateFile(wfilename.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file == INVALID_HANDLE_VALUE)
		return -1;

	FILETIME modTime;

	bool success = GetFileTime(file, NULL, NULL, &modTime);
	CloseHandle(file);

	if (success)
	{
		ptime pt = from_ftime<ptime>(modTime);
		ptime epoch(boost::gregorian::date(1970,1,1));

		return (pt - epoch).total_seconds();
	}
	else
	{
		return -1;
	}
#elif __APPLE__

	struct stat attrib;
	stat(filename.c_str(), &attrib);
	time_t modificationTime = attrib.st_mtimespec.tv_sec;

	return modificationTime;

#else
	TODO
#endif
}

void Utils::touch(const string &filename)
{
#ifdef __WIN32__

	wstring wfilename = Utils::s2ws(filename);
	HANDLE file = CreateFile(wfilename.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file == INVALID_HANDLE_VALUE)
		return;

	FILETIME ft;
	SYSTEMTIME st;

	GetSystemTime(&st);
	SystemTimeToFileTime(&st, &ft);
	SetFileTime(file, NULL, NULL, &ft);

	CloseHandle(file);

#else
	struct utimbuf newTime;

	time_t newTimeT;
	time(&newTimeT);

	newTime.actime = newTimeT;
	newTime.modtime = newTimeT;

	utime(filename.c_str(), &newTime);
#endif
}

