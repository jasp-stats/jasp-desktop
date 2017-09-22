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


	File created by patrick, on 17-08-2017
	Original file name was
*/

#ifndef DESKTOPUTILS_H
#define DESKTOPUTILS_H

#include <QStringList>

#include <vector>
#include <string>


#include "../JASP-Common/sysdepfiletype.h"

/**
 * @brief toQStr Convert UTF-8 string to QString
 * @param str String to convert
 * @return QString Converted value.
 *
 * NB DO NOT USE to convert filenames: On Windows 8 bit filenames ARE NOT utf-8!
 */
QString toQStr(const std::string &str);

/**
 * @brief toQStr Convert UTF-8 string to QString
 * @param str String to convert
 * @return QString Converted value.
 *
 * NB DO NOT USE to convert filenames: On Windows 8 bit filenames ARE NOT utf-8!
 */
QString toQStr(const char *str);

/**
 * @brief toStr Convert QString to UTF-8 string
 * @param str To convert.
 * @return UTF-8 represenation of str.
 *
 * NB DO NOT USE to convert filenames: On Windows 8 bit filenames ARE NOT utf-8!
 */
std::string toStr(const QString &str);

QStringList toQStringList(const std::vector<std::string> &from);

#endif // DESKTOPUTILS_H

