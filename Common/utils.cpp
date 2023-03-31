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


#include "utils.h"

#ifdef _WIN32
#include <windows.h>
#include <fileapi.h>
#include <winternl.h>
#else
#include <sys/stat.h>
#include <utime.h>
#endif


#include "utilenums.h"
#include <codecvt>
#include <regex>
#include <iomanip>
#include <chrono>

using namespace std;

std::string Utils::doubleToString(double dbl, int precision)
{
	std::stringstream conv; //Use this instead of std::to_string to make sure there are no trailing zeroes (and to get full precision)
	conv << std::setprecision(precision);
	conv << dbl;
	return conv.str();
}

Utils::FileType Utils::getTypeFromFileName(const std::string &path)
{

	size_t lastPoint = path.find_last_of('.');
	FileType filetype =  lastPoint == string::npos ?  FileType::empty : FileType::unknown;

	if (lastPoint != string::npos && (lastPoint + 1) < path.length())
	{
		std::string suffix = path.substr(lastPoint + 1);
		for (int i = 0; i < int(FileType::empty); i++)
		{
			FileType it = static_cast<FileType>(i);
			if (suffix == FileTypeBaseToString(it))
			{
				filetype = it;
				break;
			}
		}
	}


	return filetype;
}

long Utils::currentMillis()
{
	return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
}

long Utils::currentSeconds()
{
	return std::chrono::duration_cast<std::chrono::seconds>(std::chrono::system_clock::now().time_since_epoch()).count();
}

long Utils::getFileModificationTime(const std::string &filename)
{
#ifdef _WIN32

	HANDLE file = CreateFileA(filename.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (file == INVALID_HANDLE_VALUE)
		return -1;

	FILETIME modTime;

	bool success = GetFileTime(file, NULL, NULL, &modTime);
	CloseHandle(file);

	if (success)
	{
		LARGE_INTEGER li;
		ULONG         seconds;
		li.QuadPart = modTime.dwHighDateTime;
		li.QuadPart = (li.QuadPart << 32) | modTime.dwLowDateTime;
		RtlTimeToSecondsSince1970(&li, &seconds);

		return seconds;
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
	struct stat attrib;
	stat(filename.c_str(), &attrib);
	time_t modificationTime = attrib.st_mtim.tv_sec;

	return modificationTime;
#endif
}

long Utils::getFileSize(const string &filename)
{
	std::error_code ec;
	std::filesystem::path path;

	path = filename;


	uintmax_t fileSize = std::filesystem::file_size(path, ec);

	if (!ec)
		return fileSize;
	return -1;
}

void Utils::touch(const string &filename)
{
#ifdef _WIN32

	HANDLE file = CreateFileA(filename.c_str(), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

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

bool Utils::renameOverwrite(const string &oldName, const string &newName)
{
	std::filesystem::path o = osPath(oldName);
	std::filesystem::path n = osPath(newName);
	std::error_code ec;

#ifdef _WIN32
	std::error_code er;
	if (std::filesystem::exists(n, er))
	{
		std::filesystem::file_status s = std::filesystem::status(n);
		
		bool readOnly = (s.permissions() & std::filesystem::perms::owner_write) == std::filesystem::perms::none;
		if (readOnly)
			std::filesystem::permissions(n, std::filesystem::perms::owner_write);
	}
#endif

	std::filesystem::rename(o, n, ec);

	return !ec;
}

bool Utils::removeFile(const string &path)
{
	std::filesystem::path p = osPath(path);
	std::error_code ec;

	std::filesystem::remove(p, ec);

	return !ec;
}

std::filesystem::path Utils::osPath(const string &path)
{
	return std::filesystem::path(path);
}

string Utils::osPath(const std::filesystem::path &path)
{
	return path.generic_string();
}

void Utils::remove(vector<string> &target, const vector<string> &toRemove)
{
	for (const string &remove : toRemove)
		target.erase(std::remove_if(target.begin(), target.end(), [&remove](const string& str){return (str == remove);}), target.end());
}

void Utils::sleep(int ms)
{

#ifdef _WIN32
	Sleep(DWORD(ms));
#else
	struct timespec ts = { ms / 1000, (ms % 1000) * 1000 * 1000 };
	nanosleep(&ts, NULL);
#endif
}


bool Utils::isEqual(const double a, const double b)
{
	if (isnan(a) || isnan(b)) return false;

	return (fabs(a - b) <= ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * std::numeric_limits<double>::epsilon()));
}
bool Utils::isEqual(const float a, const float b)
{
	if (isnan(a) || isnan(b)) return false;

	return fabs(a - b) <= ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * std::numeric_limits<float>::epsilon());
}

#ifdef _WIN32
std::wstring Utils::getShortPathWin(const std::wstring & longPath) 
{
	//See: https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getshortpathnamew
	long     length = 0;
	TCHAR*   buffer = NULL;

// First obtain the size needed by passing NULL and 0.

	length = GetShortPathName(longPath.c_str(), NULL, 0);
	if (length == 0) 
		return longPath;

// Dynamically allocate the correct size 
// (terminating null char was included in length)

	buffer = new TCHAR[length];

// Now simply call again using same long path.

	length = GetShortPathName(longPath.c_str(), buffer, length);
	if (length == 0)
		return longPath;

	std::wstring shortPath(buffer, length);
	
	delete [] buffer;
	
	return shortPath;
}

string Utils::wstringToString(const std::wstring & wstr)
{
	std::string str;

	//get size of buffer we need
	int requiredSize = WideCharToMultiByte(CP_UTF8, 0, wstr.data(), -1, NULL, 0, NULL, NULL );
	str.resize(requiredSize);

	//convert it
	WideCharToMultiByte(CP_UTF8, 0, wstr.data(), -1, str.data(), str.size(), NULL, NULL );
	str.resize(requiredSize-1);//drop /nul

	return str;

}
#endif
