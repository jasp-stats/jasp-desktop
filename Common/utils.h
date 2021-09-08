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
#include <boost/filesystem.hpp>
#include "timers.h"

enum class FileTypeBase;

class Utils
{
public:
	friend class PreferencesModel;

	typedef FileTypeBase					FileType;
	typedef std::vector<Utils::FileType>	FileTypeVector;

	static Utils::FileType getTypeFromFileName(	const std::string &path);

	static long currentMillis();
	static long currentSeconds();
	static long getFileModificationTime(const std::string &filename);
	static long getFileSize(			const std::string &filename);
	static void touch(					const std::string &filename);
	static bool renameOverwrite(		const std::string &oldName, const std::string &newName);
	static bool removeFile(				const std::string &path);

	static boost::filesystem::path osPath(const std::string &path);
    static std::string osPath(const boost::filesystem::path &path);

	static void remove(std::vector<std::string> &target, const std::vector<std::string> &toRemove);
	static void sleep(int ms);

	static const std::string emptyValue;
	static const std::vector<std::string>	& getEmptyValues()			{ return _currentEmptyValues;		}
	static const std::vector<std::string>	& getDefaultEmptyValues()	{ return _defaultEmptyValues;		}
	static const std::vector<double>		& getDoubleEmptyValues()	{ return _currentDoubleEmptyValues;	}
	static void setEmptyValues(const std::vector<std::string>& emptyvalues);
	static void processEmptyValues();

	static bool getIntValue(const std::string& value, int& intValue);
	static bool getIntValue(const double& value, int& intValue);
	static bool getDoubleValue(const std::string& value, double& doubleValue);

	static bool isEmptyValue(const std::string& val);
	static bool isEmptyValue(const double& val);

	static bool			convertValueToIntForImport(		const std::string &strValue, int &intValue);
	static bool			convertValueToDoubleForImport(	const std::string &strValue, double &doubleValue);
	static std::string	doubleToString(double dbl, int precision = 10);
	static void			convertEscapedUnicodeToUTF8(	std::string &inputStr);

	static bool isEqual(const float a, const float b);
	static bool isEqual(const double a, const double b);

#ifdef _WIN32
	static std::wstring	getShortPathWin(const std::wstring & path);
#endif
	
private:
	static std::string _deEuropeaniseForImport(			const std::string &value);
	static std::string _convertEscapedUnicodeToUTF8(	std::string hex);

	static std::vector<std::string>			_currentEmptyValues;
	static const std::vector<std::string>	_defaultEmptyValues;
	static std::vector<double>				_currentDoubleEmptyValues;
};

#endif // UTILS_H
