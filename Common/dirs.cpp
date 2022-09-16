//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "dirs.h"


#include <sstream>

#ifdef _WIN32
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

#include "processinfo.h"
#include "utils.h"
#include "appinfo.h"
#include <iostream>

using namespace std;
using namespace boost;

string Dirs::tempDir()
{
	static string p = "";

	if (p != "")
		return p;

	string dir;
	boost::filesystem::path pa;

#ifdef _WIN32
	wchar_t buffer[MAX_PATH];
	if ( ! SUCCEEDED(SHGetFolderPathW(NULL, CSIDL_LOCAL_APPDATA, NULL, 0, buffer)))
		throw Exception("App Data directory could not be retrieved");

	dir = Utils::wstringToString(buffer);

	dir += "/JASP/temp";

	pa = (dir);

#else

	dir = string(getpwuid(getuid())->pw_dir);
	dir += "/.JASP/temp";

	pa = dir;

#endif

	if (!boost::filesystem::exists(pa))
	{
		system::error_code ec;
		boost::filesystem::create_directories(pa, ec);

		if (ec)
		{
			stringstream ss;
			ss << "Temp Data directory could not be created (" << ec << ") : " << dir;
			throw Exception(ss.str());
		}
	}

	p = boost::filesystem::path(dir).generic_string();

	return p;
}

#ifdef BOOST_INTERPROCESS_SHARED_DIR_FUNC
namespace boost {
namespace interprocess {
namespace ipcdetail {
void get_shared_dir(std::string &shared_dir)
{
	shared_dir = Dirs::tempDir();
}
}}}
#endif

string Dirs::exeDir()
{
	static string p = "";
	if (p != "")
		return p;

#ifdef _WIN32
	HMODULE hModule = GetModuleHandleW(NULL);
	CHAR path[MAX_PATH];

	int ret = GetModuleFileNameA(hModule, path, MAX_PATH);

	if (ret == 0)
	{
		stringstream ss;
		ss << "Executable directory could not be retrieved (" << ret << ")";
		throw Exception(ss.str());
	}

	string r = (path);

	char pathbuf[MAX_PATH];
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

	p = r;

	return r;

#elif defined(__APPLE__)

	unsigned long pid = ProcessInfo::currentPID();

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

	char buf[512];
	char linkname[512]; /* /proc/<pid>/exe */
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

	buf[ret] = '\0';

	//std::cout << "looking for exeDir in buff: '" << buf << "'\n" << std::flush;

	for (int i = ret-1; i > 0; i--)
	{
		if (buf[i] == '/')
		{
			buf[i] = '\0'; // add null terminator
			break;
		}
	}

	std::string exe = string(buf);

	//std::cout << "exeDir found: '" << exe << "'\n" << std::flush;

	return exe;

#endif

}

string Dirs::resourcesDir()
{
	static string dir;
	
	if(dir == "")
	{
		dir = exeDir();

#ifdef __APPLE__
		dir += "/..";
#elif __linux__
		dir += "/..";
#endif

		dir += "/Resources/";
	}

	return dir; 
}
