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
 * @brief toQStr Convert JaspFileTypes::FilePath (aka JaspFileTypes::FilePath) to QString
 * @param path The path to convert.
 * @return The file name in OS native format as QString
 */
QString toQStr(const JaspFileTypes::FilePath &pth)
{
#ifdef __WIN32__
    wstring w = pth.wstring();
    QString result = QString::fromStdWString(w);
    return result;
#else
    return QString::fromStdString(pth.string());
#endif
}


/**
 * @brief toPath Converts a QString file path to a path object.
 * @param pa The QString path to convert.
 * @return A path object wi the same constent as the param pa.
 */
JaspFileTypes::FilePath toPath(const QString &pa)
{
#ifdef _WIN32
    // Windows is odd QT uses \ as dir sep also on Windows.
    QString path(pa);
    path.replace('/', '\\');
    return JaspFileTypes::FilePath(path.toStdWString());
#else
    return JaspFileTypes::FilePath(pa.toStdString());
#endif
}


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

