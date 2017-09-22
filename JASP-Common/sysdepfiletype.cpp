/*
	Copyright (C) Copyright (C) 2013-2017 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 12-08-2017
	Original file name was sysdepfiletype.h
*/



#include "sysdepfiletype.h"


using namespace JaspFiles;
using namespace boost;

/*********************************************************************
 *
 * Class _jaspPath
 *
 *********************************************************************/

/**
 * @brief operator /= Appends a directory level to the right.
 * @param rhs The name of the directory to add.
 * @return *this;
 */
_jaspPath & _jaspPath::operator /= (const std::string &rhs)
{
	return *this /= rhs.c_str();
}

_jaspPath & _jaspPath::operator /= (const char *rhs)
{
	_s.push_back(DIR_SEP);
	_s.append(rhs);

	return *this;
}

/**
 * @brief string Get path as UTF-16 string.
 * @return
 */
std::wstring _jaspPath::wstring() const
{
	return nowide::widen(_s);
}

/**
 * @brief string Get path as UTF-16 string.
 * @return
 */
QString _jaspPath::qstring() const
{
	QString result = QString::fromUtf8(_s.data(), _s.size());
	if (QT_DIR_SEP != DIR_SEP)
		result.replace(QChar(DIR_SEP), QChar(QT_DIR_SEP));
	return result;
}

/**
 * @brief toPath Returns a path
 * @return path.
 */
filesystem::path  _jaspPath::path() const
{
#ifdef _WIN32
	return filesystem::path(this->wstring());
#else
	return filesystem::path(this->string());
#endif
}

/**
 * @brief filename Returns the filename portion.
 * @return The file-name.
 */
std::string _jaspPath::filename() const
{
	// find the last dir sep.
	std::string::size_type lastDir = _s.rfind(DIR_SEP);
	// if found and not last in string.
	if ((lastDir != std::string::npos) && (lastDir < (_s.size() -1)))
		return _s.substr(lastDir + 1);
	else
		return string();
}

/**
 * @brief extension Get the extension.
 * @return The extension.
 */
std::string _jaspPath::extension() const
{
	std::string fn = filename();
	std::string::size_type lastDot  = fn.rfind(DOT);
	if ((lastDot != std::string::npos) && (lastDot < _s.size()))
		return fn.substr(lastDot+1);
	else
		return string();
}

/**
 * @brief has_extension Returns true if there is DOT anything in filename.
 * @return
 */
bool _jaspPath::has_extension() const
{
	std::string fn = filename();
	std::string::size_type lastDot  = fn.rfind(DOT);
	return ((lastDot != std::string::npos) && (lastDot < (_s.size() - 1)));
}

#ifdef _WIN32
/**
 * @brief parent_path Returns the enclosing directory.
 * @return The directory "one up".
 *
 * Only used in Windows test builds
 */
_jaspPath _jaspPath::parent_path() const
{
	size_t posLastSep = _s.rfind( DIR_SEP );
	// Not found? Just root? return the while thing?
	if ((posLastSep == std::string::npos) || (posLastSep == 0))
		return _s;
	// last char is /? Chop it and try again.
	else if (posLastSep == (_s.size() - 1))
	{
		_jaspPath result = _s.substr(0, _s.size() - 1);
		return result.parent_path();
	}
	else
		return _s.substr(0, posLastSep);
}
#endif

/**
 * @brief startsWith compares first chars in path
 * @param search Comparison.
 * @return `true` if the first chars in path match `search` exactly.
 */
bool _jaspPath::startsWith(const std::string &search) const
{
	std::string first = _s.substr(0, search.size());
	return first.compare(search);
}


/**
 * @brief toQStr Convert to QString
 * @return The file name in OS native format as QString
 */
std::string _jaspPath::cvt(const QString & from)
{
	QString pat(from);
	if (QT_DIR_SEP != DIR_SEP)
		pat.replace(QChar(QT_DIR_SEP), QChar(DIR_SEP));
	return pat.toStdString();
}

