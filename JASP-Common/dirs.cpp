//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include <iostream>
#include <sstream>

#ifdef __WIN32__
#include <windows.h>
#include <shlwapi.h>
#include <shlobj.h>
#include <knownfolders.h>
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
#include <boost/nowide/convert.hpp>

#include "processinfo.h"
#include "utils.h"
#include "appinfo.h"

using namespace std;
using namespace boost;

#ifdef __WIN32__
const GUID FOLDERID_RoamingAppData = { 0x3eb685db, 0x65f9, 0x4cf6,{ 0xa0, 0x3a, 0xe3, 0xef, 0x65, 0x72, 0x9f, 0x3d } };
#endif


JaspFileTypes::FilePath Dirs::appDataDir()
{
	static filesystem::path p = "";

	if (p != "")
		return p;

#ifdef __WIN32__
	{
		PWSTR buffer;

        HRESULT ret = SHGetKnownFolderPath(FOLDERID_RoamingAppData, KF_FLAG_DEFAULT, NULL, &buffer);
		if ( ! SUCCEEDED(ret))
		{
			stringstream ss;
			ss << "App Data directory could not be retrieved (" << ret << ")";
			throw Exception(ss.str());
		}

		p = buffer;
		CoTaskMemFree(buffer);
	}
#else
	p = getpwuid(getuid())->pw_dir;
#endif

	p /= AppInfo::name;
	p /= AppInfo::getShortDesc();

	if ( ! filesystem::exists(p))
	{
		system::error_code ec;

		filesystem::create_directories(p, ec);

		if (ec)
		{
			stringstream ss;
            ss << "App Data directory could not be created: " << p.string();
			throw Exception(ss.str());
		}
	}

#ifndef QT_NO_DEBUG
	cout << "Dirs::appDataDir() - returning " << p.string() << endl;
#endif

	return p;
}


JaspFileTypes::FilePath Dirs::tempDir()
{
	static filesystem::path p = "";

	if (p != "")
		return p;

#ifdef __WIN32__
	TCHAR buffer[MAX_PATH + 2];
    if ( ::GetTempPath(MAX_PATH, buffer) == 0)
		throw Exception("App Data directory could not be retrieved");

	p = buffer;
#else
	{
		const char * tmp = getenv("TMPDIR");
		if (tmp == 0)
			tmp = "/tmp";
		p = tmp;
	}

#endif
	p /= AppInfo::name;
	if ( ! filesystem::exists(p))
	{
		system::error_code ec;
		filesystem::create_directories(p, ec);

		if (ec)
		{
			stringstream ss;
            ss << "Temp Data directory could not be created (" << ec << ") : " << p.string();
			throw Exception(ss.str());
		}
	}

#ifndef QT_NO_DEBUG
    cout << "Dirs::tempDir() - returning " << p.string() << endl;
#endif

	return p;
}

#ifdef BOOST_INTERPROCESS_SHARED_DIR_FUNC
namespace boost {
namespace interprocess {
namespace ipcdetail {
void get_shared_dir(std::string &shared_dir)
{
	shared_dir = Dirs::tempDir().native();
}
}}}
#endif

JaspFileTypes::FilePath Dirs::exeDir()
{
	static filesystem::path p;

	if (p.has_filename())
		return p.parent_path();

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

	p = path;

#elif defined(__APPLE__)

	unsigned long pid = ProcessInfo::currentPID();

	char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
	int ret = proc_pidpath (pid, pathbuf, sizeof(pathbuf));

	if (ret <= 0)
	{
		throw Exception("Executable directory could not be retrieved");
	}

	p = pathbuff;

#else

    char buf[512];
    char linkname[512]; /* /proc/<pid>/exe */
	pid_t pid;
	ssize_t ret;

	pid = getpid();

	if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
		throw Exception("Executable directory could not be retrieved");

	ret = readlink(linkname, buf, sizeof(buf));

	if (ret == -1)
		throw Exception("Executable directory could not be retrieved");

	if (ret >= sizeof(buf))
		throw Exception("Executable directory could not be retrieved: insufficient buffer size");

	p = buf;
#endif

	return p.parent_path();

}

JaspFileTypes::FilePath Dirs::rHomeDir()
{
	filesystem::path dir = exeDir();

#ifdef __WIN32__
	dir /= "R";
#elif __APPLE__
	dir /= "../Frameworks/R.framework/Versions/" + CURRENT_R_VERSION + "/Resources";
#else
	dir /= "R";
#endif

	return dir;
}


JaspFileTypes::FilePath Dirs::libraryDir()
{
	filesystem::path dir = exeDir();

#ifdef __WIN32__
	dir /= "Resources/Library";
#elif __APPLE__
	dir /= "../Resources/Library";
#else
	dir /= "Resources/Library";
#endif

#ifndef QT_NO_DEBUG
	cout << "Dirs::libraryDir() returning : " << dir.native() << endl;
#endif
    return dir;
}
