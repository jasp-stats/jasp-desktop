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
#include "windows.h"
#else
#include <sys/stat.h>
#include <utime.h>
#endif

#ifndef IGNORE_BOOST
#include <boost/date_time/posix_time/posix_time.hpp>

#include <boost/filesystem.hpp>
#include <boost/nowide/convert.hpp>
#include <boost/algorithm/string/predicate.hpp>
#endif

#include "utilenums.h"
#include <codecvt>
#include <regex>

using namespace std;
using namespace boost::posix_time;
using namespace boost;

Utils::FileType Utils::getTypeFromFileName(const std::string &path)
{

	FileType filetype =  FileType::unknown;

	for (int i = 0; i < int(FileType::empty); i++)
	{
		FileType it = static_cast<FileType>(i);

		if (algorithm::iends_with(path, "." + FileTypeBaseToString(it)))
		{
			filetype = it;
			break;
		}
	}

	if (filetype == FileType::unknown && !algorithm::find_last(path, "."))
		filetype =  FileType::empty;


	return filetype;
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
#ifdef _WIN32

	wstring wfilename = nowide::widen(filename);
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
    struct stat attrib;
    stat(filename.c_str(), &attrib);
    time_t modificationTime = attrib.st_mtim.tv_sec;

    return modificationTime;
#endif
}

long Utils::getFileSize(const string &filename)
{
	system::error_code ec;
	filesystem::path path;

#ifdef _WIN32

	path = boost::nowide::widen(filename);

#else

	path = filename;

#endif

	uintmax_t fileSize = filesystem::file_size(path, ec);

	if (!ec)
		return fileSize;
	return -1;
}

void Utils::touch(const string &filename)
{
#ifdef _WIN32

	wstring wfilename = nowide::widen(filename);
	HANDLE file = CreateFile(wfilename.c_str(), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

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
	filesystem::path o = osPath(oldName);
	filesystem::path n = osPath(newName);
	system::error_code ec;

#ifdef _WIN32
	system::error_code er;
	if (filesystem::exists(n, er)) {
		filesystem::file_status s = filesystem::status(n);
		bool readOnly = (s.permissions() & filesystem::owner_write) == 0;
		if (readOnly)
			filesystem::permissions(n, filesystem::owner_write);
	}
#endif

	boost::filesystem::rename(o, n, ec);

	return !ec;
}

bool Utils::removeFile(const string &path)
{
	filesystem::path p = osPath(path);
	system::error_code ec;

	boost::filesystem::remove(p, ec);

	return !ec;
}

filesystem::path Utils::osPath(const string &path)
{
#ifdef _WIN32
	return filesystem::path(nowide::widen(path));
#else
	return filesystem::path(path);
#endif
}

string Utils::osPath(const filesystem::path &path)
{
#ifdef _WIN32
	return nowide::narrow(path.generic_wstring());
#else
	return path.generic_string();
#endif
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

const string			Utils::emptyValue					= "";
const vector<string>	Utils::_defaultEmptyValues			= {"NaN", "nan", ".", "NA"};
vector<double>			Utils::_currentDoubleEmptyValues	= {};
vector<string>			Utils::_currentEmptyValues			= Utils::_defaultEmptyValues;

void Utils::setEmptyValues(const vector<string> &emptyvalues)
{
	_currentEmptyValues = emptyvalues;
	processEmptyValues();
}

void Utils::processEmptyValues()
{
	_currentDoubleEmptyValues.clear();

	for (vector<string>::const_iterator it = _currentEmptyValues.begin(); it != _currentEmptyValues.end(); ++it)
	{
		double doubleValue;
		if (Utils::getDoubleValue(*it, doubleValue))
			_currentDoubleEmptyValues.push_back(doubleValue);
	}
}

bool Utils::getIntValue(const string &value, int &intValue)
{
	try
	{
		intValue = boost::lexical_cast<int>(value);
		return true;
	}
	catch (...)	{}

	return false;
}

bool Utils::getIntValue(const double &value, int &intValue)
{
	try
	{
		double intPart;

		if (modf(value, &intPart) == 0.0)
		{
			if (intPart <=  INT_MAX && intPart >= INT_MIN)
			{
				intValue = int(intPart);
				return true;
			}
		}
	}
	catch (...) {}

	return false;
}

bool Utils::getDoubleValue(const string &value, double &doubleValue)
{
	try
	{
		doubleValue = boost::lexical_cast<double>(value);
		return true;
	}
	catch (...) {}

	return false;
}


std::string Utils::_deEuropeaniseForImport(const std::string &value)
{
	int dots	= 0,
		commas	= 0;

	for (char k : value)
		if		(k == '.')	dots++;
		else if	(k == ',')	commas++;

	if (commas > 0)
	{
		std::string uneurope = value;

		if (dots > 0)
		{
			size_t	i = 0,
					j = 0;

			for (;i < value.size(); i++)
				if (value[i] != '.')
				{
					uneurope[j] = value[i];
					j++;
				}

			uneurope.resize(j);
		}

		for (size_t i = 0; i < uneurope.length(); i++)
			if (uneurope[i] == ',')
			{
				uneurope[i] = '.';
				break;
			}

		return uneurope;
	}

	return value;
}

bool Utils::isEmptyValue(const std::string& val)
{
	if (val.empty()) return true;

	return std::find(_currentEmptyValues.begin(), _currentEmptyValues.end(), val) != _currentEmptyValues.end();
}

bool Utils::isEmptyValue(const double &val)
{
	if (std::isnan(val)) return true;

	return std::find(_currentDoubleEmptyValues.begin(), _currentDoubleEmptyValues.end(), val) != _currentDoubleEmptyValues.end();
}

bool Utils::convertValueToIntForImport(const std::string &strValue, int &intValue)
{
	if (!isEmptyValue(strValue))
	{
		if (!Utils::getIntValue(strValue, intValue))
			return false;
	}
	else
		intValue = INT_MIN;

	return true;
}

bool Utils::convertValueToDoubleForImport(const std::string &strValue, double &doubleValue)
{
	std::string v = _deEuropeaniseForImport(strValue);

	if (!isEmptyValue(v))
	{
		if (!Utils::getDoubleValue(v, doubleValue))
			return false;
	}
	else
		doubleValue = NAN;

	return true;
}

std::string Utils::doubleToString(double dbl)
{
	std::stringstream conv; //Use this instead of std::to_string to make sure there are no trailing zeroes (and to get full precision)
	conv << dbl;
	return conv.str();
}

// hex should be 4 hexadecimals characters
std::string Utils::_convertEscapedUnicodeToUTF8(const std::string& hex)
{
	std::string result = hex;
	std::istringstream iss(hex);

	uint32_t bytes;
	std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;

	// Read the 4 hexadecimals as bytes, and convert these bytes into UTF8.
	if (iss >> std::hex >> bytes) result = conv.to_bytes(char32_t(bytes));

	return result;
}

// Replace all <U+FFFF> in str by their UT8 characters.
void Utils::convertEscapedUnicodeToUTF8(std::string& inputStr)
{
	static const std::regex unicodeExpression ("<U\\+([0-9a-fA-F]{4})>");

	std::smatch match;
	auto begin	= inputStr.cbegin();

	while (std::regex_search(begin, inputStr.cend(), match, unicodeExpression))
	{
		std::string utf8 = _convertEscapedUnicodeToUTF8(match[1].str()); // match 1 is the first group of the regexp: that is the 4 hexadecimals.
		auto pos = match.position(0); // position of the whole sequence in str.
		inputStr.replace(begin + pos, begin + pos + 8, utf8);
		// str iterators cannot be trusted after replace. They must be recalculated from str.
		begin = inputStr.begin() + pos;
	}
}
