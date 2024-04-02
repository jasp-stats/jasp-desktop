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

#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>
#include <limits>
#include <filesystem>
#include <set>
#include <map>
#include "timers.h"

enum class FileTypeBase;

typedef std::vector<double>						doublevec;
typedef std::vector<bool>						boolvec;
typedef std::vector<int>						intvec;
typedef std::vector<size_t>						sizetvec;
typedef std::set<int>							intset;
typedef std::set<size_t>						sizetset;
typedef std::set<double>						doubleset;
typedef std::map<int,			std::string>	intstrmap;
typedef std::map<std::string,	std::string>	strstrmap;
typedef std::map<std::string,	int>			strintmap;
typedef std::map<int, int>						intintmap;
typedef std::set<std::string>					stringset;
typedef std::vector<std::string>				stringvec;
typedef std::vector<stringvec>					stringvecvec;

/// One of the utility classes
/// This is for the sort of functions that you might want to use in either Desktop or Engine. Should not be used in R-Interface or jaspResults.
class Utils
{
public:
	typedef FileTypeBase					FileType;
	typedef std::vector<Utils::FileType>	FileTypeVector;

	static Utils::FileType			getTypeFromFileName(	const std::string &path);

	static const std::string &		currentDateTime();
	static long						currentMillis();
	static long						currentSeconds();
	static long						getFileModificationTime(const std::string &filename);
	static long						getFileSize(			const std::string &filename);
	static void						touch(					const std::string &filename);
	static bool						renameOverwrite(		const std::string &oldName, const std::string &newName);
	static bool						removeFile(				const std::string &path);

	static std::filesystem::path	osPath(const std::string &path);
	static std::string				osPath(const std::filesystem::path &path);

	static void						remove(stringvec &target, const stringvec &toRemove);
	static void						sleep(int ms);

	static bool						isEqual(const float a,	const float b);
	static bool						isEqual(const double a, const double b);

#ifdef _WIN32
	static std::wstring	getShortPathWin(const std::wstring & path);
	static std::string  wstringToString(const std::wstring & wstr);
	static std::wstring stringToWString(const std::string & str);
#endif
	

};

#endif // UTILS_H
