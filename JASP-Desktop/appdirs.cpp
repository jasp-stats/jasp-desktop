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

#include "appdirs.h"

#include <QApplication>
#include <QDir>
#include <QDebug>

#ifdef __WIN32__
#include <windows.h>
#include "qutils.h"
#include "utils.h"
#endif

#include "dirs.h"

using namespace std;

const QString AppDirs::examples()
{
#ifdef __APPLE__
    static QString dir = QApplication::applicationDirPath() + "/../Resources/Data Sets";
#else
    static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Resources/Data Sets";
#endif

	return dir;
}

const QString AppDirs::help()
{
#ifdef __APPLE__
	static QString dir = QApplication::applicationDirPath() + "/../Resources/Help";
#else
	static QString dir = QApplication::applicationDirPath() + QDir::separator() + "Resources/Help";
#endif

	return dir;
}

const QString AppDirs::analysisDefaultsDir()
{
	QString path = QString::fromStdString(Dirs::appDataDir()) + "/AnalysisDefaults";
	QDir dir(path);
	dir.mkpath(".");

	return path;
}
