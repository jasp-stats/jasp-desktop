//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "desktoputils.h"

#include <boost/foreach.hpp>

using namespace std;

/**
 * @brief toQStr Convert UTF-8 string to QString
 * @param str String to convert
 * @return QString Converted value.
 *
 * NB DO NOT USE to convert filenames: On Windows 8 bit filenames ARE NOT utf-8!
 */
QString toQStr(const std::string &str)
{
#ifdef QT_NO_DBEUG
    return QString::fromStdString(str);
#else
    QString result = QString::fromStdString(str);
    return result;
#endif
}

/**
 * @brief toQStr Convert UTF-8 string to QString
 * @param str String to convert
 * @return QString Converted value.
 *
 * NB DO NOT USE to convert filenames: On Windows 8 bit filenames ARE NOT utf-8!
 */
QString toQStr(const char *str)
{
    return QString::fromUtf8(str);
}


/**
 * @brief toStr Convert QString to UTF-8 string
 * @param str To convert.
 * @return UTF-8 represenation of str.
 *
 * NB DO NOT USE to convert filenames: On Windows 8 bit filenames ARE NOT utf-8!
 */
string toStr(const QString &str)
{
	return str.toStdString();
}


/**
 * @brief toPath Converts a QString file path to a path object.
 * @param pa The QString path to convert.
 * @return A path object wi the same constent as the param pa.
 */
/*
JaspFiles::Path toPath(const QString &pa)
{
#ifdef _WIN32
    QString path(pa);
    path.replace(QChar('/'), QChar('\\'), Qt::CaseInsensitive);
  // Spoof the pa variable.
  #define pa path
#endif
    // Even upder Windows we use URF-8 filenames.
#ifdef QT_NO_DEBUG
    return JaspFiles::Path((const char *)pa.toUtf8());
#else
    JaspFiles::Path pat((const char *)pa.toUtf8());
    return pat;
#endif
#ifdef _WIN32
  #undef pa
#endif
} */


QStringList toQStringList(const vector<string> &from)
{
    (void)from;

    QStringList result;

    BOOST_FOREACH(const std::string &str, from)
    {
        (void)from;
        result.append(QString::fromStdString(str));
    }

    return result;
}
