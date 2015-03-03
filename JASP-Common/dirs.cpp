
#include "dirs.h"

#include <iostream>
#include <sstream>

#ifdef __WIN32__
#include <windows.h>
#include <shlwapi.h>
#include <shlobj.h>
#elif defined(__APPLE__)
#include <libproc.h>
#include <unistd.h>
#include <pwd.h>
#else
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#endif


#include <boost/filesystem.hpp>
#include <boost/system/error_code.hpp>

#include "process.h"
#include "utils.h"
#include "version.h"

using namespace std;
using namespace boost;

string Dirs::appDataDir()
{
	static string p = "";

	if (p != "")
		return p;

	string dir;

#ifdef __WIN32__
	TCHAR buffer[MAX_PATH];

	HRESULT ret = SHGetFolderPath(NULL, CSIDL_APPDATA, NULL, 0, buffer);

	if ( ! SUCCEEDED(ret))
	{
		stringstream ss;
		ss << "App Data directory could not be retrieved (" << ret << ")";
		throw Exception(ss.str());
	}

	dir = Utils::ws2s(buffer);
	dir += "/JASP/" + string(APP_VERSION);

#else

	dir = string(getpwuid(getuid())->pw_dir);
	dir += "/.JASP/" + string(APP_VERSION);

#endif

	if ( ! filesystem::exists(dir))
	{
		system::error_code ec;

		filesystem::create_directories(dir, ec);

		if (ec)
		{
			stringstream ss;
			ss << "App Data directory could not be created: " << dir;
			throw Exception(ss.str());
		}
	}

	p = filesystem::path(dir).generic_string();

	return p;
}

string Dirs::tempDir()
{
	static string p = "";

	if (p != "")
		return p;

	string dir;

#ifdef __WIN32__
	TCHAR buffer[MAX_PATH];
	if ( ! SUCCEEDED(SHGetFolderPath(NULL, CSIDL_LOCAL_APPDATA, NULL, 0, buffer)))
		throw Exception("App Data directory could not be retrieved");

	dir = Utils::ws2s(buffer);
	dir += "/JASP/temp";

#else

	dir = string(getpwuid(getuid())->pw_dir);
	dir += "/.JASP/temp";

#endif

	if ( ! filesystem::exists(dir))
	{
		system::error_code ec;
		filesystem::create_directories(dir, ec);

		if (ec)
		{
			stringstream ss;
			ss << "Temp Data directory could not be created (" << ec << ") : " << dir;
			throw Exception(ss.str());
		}
	}

	p = filesystem::path(dir).generic_string();

	return p;
}

string Dirs::exeDir()
{
	static string p = "";
	if (p != "")
		return p;

#ifdef __WIN32__
	HMODULE hModule = GetModuleHandleW(NULL);
	WCHAR path[MAX_PATH];

	int ret = GetModuleFileNameW(hModule, path, MAX_PATH);

	if (ret == 0)
	{
		stringstream ss;
		ss << "Executable directory could not be retrieved (" << ret << ")";
		throw Exception(ss.str());
	}

	wstring s(path);
	string r = Utils::ws2s(path);

	char *pathbuf = new char[MAX_PATH];
	r.copy(pathbuf, MAX_PATH);

	int last = 0;

	for (int i = 0; i < r.length(); i++)
	{
		if (pathbuf[i] == '\\')
		{
			pathbuf[i] = '/';
			last = i;
		}
	}

	r = string(pathbuf, last);

	delete[] pathbuf;

	p = r;

	return r;

#elif defined(__APPLE__)

	unsigned long pid = Process::currentPID();

	char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
	int ret = proc_pidpath (pid, pathbuf, sizeof(pathbuf));

	if (ret <= 0)
	{
		throw Exception("Executable directory could not be retrieved");
	}
	else
	{
		int last = strlen(pathbuf);

		for (int i = last - 1; i > 0; i--)
		{
			if (pathbuf[i] == '/')
			{
				pathbuf[i] = '\0';
				break;
			}
		}

		p = string(pathbuf);

		return p;
	}
#else

	char buf[65];
	char linkname[64]; /* /proc/<pid>/exe */
	pid_t pid;
	int ret;

	pid = getpid();

	if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
		throw Exception("Executable directory could not be retrieved");

	ret = readlink(linkname, buf, sizeof(buf));

	if (ret == -1)
		throw Exception("Executable directory could not be retrieved");

	if (ret >= sizeof(buf))
		throw Exception("Executable directory could not be retrieved: insufficient buffer size");

	/* Ensure proper NUL termination */
	buf[ret] = 0;

	return string(buf);

#endif

}

string Dirs::rHomeDir()
{
	string dir = exeDir();

#ifdef __WIN32__
	dir += "/R";
#elif __APPLE__
	dir += "/../Frameworks/R.framework/Versions/3.1/Resources";
#else
	dir += "/R";
#endif

	return dir;
}

