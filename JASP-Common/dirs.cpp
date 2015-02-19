
#include "dirs.h"

#include <iostream>

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

#include "process.h"
#include "utils.h"
#include "version.h"

using namespace std;
using namespace boost::filesystem;

string Dirs::appDataDir()
{
	static string p = "";

	if (p != "")
		return p;

	string dir;

#ifdef __WIN32__
	TCHAR buffer[MAX_PATH];
	if ( ! SUCCEEDED(SHGetFolderPath(NULL, CSIDL_APPDATA, NULL, 0, buffer)))
	{
		std::cerr << "App Data dir could not be retrieved\n";
		std::cerr.flush();

		throw std::exception();
	}

	dir = Utils::ws2s(buffer);
	dir += "/JASP/" + string(APP_VERSION);

#else

	dir = string(getpwuid(getuid())->pw_dir);
	dir += "/.JASP/" + string(APP_VERSION);

#endif

	if ( ! exists(dir))
	{
		if (create_directories(dir) == false)
		{
			std::cerr << dir << " could not be created\n";
			std::cerr.flush();

			throw std::exception();
		}
	}

	p = path(dir).generic_string();

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
	{
		std::cerr << "Local App Data dir could not be retrieved\n";
		std::cerr.flush();

		throw std::exception();
	}

	dir = Utils::ws2s(buffer);
	dir += "/JASP/temp";

#else

	dir = string(getpwuid(getuid())->pw_dir);
	dir += "/.JASP/temp";

#endif

	if ( ! exists(dir))
	{
		if (create_directories(dir) == false)
		{
			std::cerr << dir << " could not be created\n";
			std::cerr.flush();

			throw std::exception();
		}
	}

	p = path(dir).generic_string();

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
	GetModuleFileNameW(hModule, path, MAX_PATH);

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
		cerr << "Could not retrieve exe location\n";
		cerr.flush();

		throw exception();
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

	/* Get our PID and build the name of the link in /proc */
	pid = getpid();

	if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
		{
		/* This should only happen on large word systems. I'm not sure
		   what the proper response is here.
		   Since it really is an assert-like condition, aborting the
		   program seems to be in order. */
		abort();
		}


	/* Now read the symbolic link */
	ret = readlink(linkname, buf, sizeof(buf));

	/* In case of an error, leave the handling up to the caller */
	if (ret == -1)
		return NULL;

	/* Report insufficient buffer size */
	if (ret >= sizeof(buf))
	{
		errno = ERANGE;
		return NULL;
	}

	/* Ensure proper NUL termination */
	buf[ret] = 0;

	std::cout << buf << "\n";
	std::cout.flush();

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

