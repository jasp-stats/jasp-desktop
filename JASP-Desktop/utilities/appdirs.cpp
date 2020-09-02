//
// Copyright (C) 2013-2018 University of Amsterdam
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


#ifdef _WIN32
#include <windows.h>
#endif
#include "utilities/qutils.h"
#include "utils.h"

#include "dirs.h"
#include <QStandardPaths>

using namespace std;

QString AppDirs::examples()
{
    static QString dir = tq(Dirs::resourcesDir()) + tq("Data Sets");
	return dir;
}

QString AppDirs::help()
{
	static QString dir = tq(Dirs::resourcesDir()) + tq("Help");

	return dir;
}

QString AppDirs::analysisDefaultsDir()
{
	QString path = QString::fromStdString(Dirs::appDataDir()) + "/AnalysisDefaults";
	QDir dir(path);
	dir.mkpath(".");

	return path;
}

QString AppDirs::userRLibrary()
{
	QString path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
	path += "/libraryR/";

	return path;
}

QString AppDirs::modulesDir()
{
	QString path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
	path += "/Modules/";

	return path;
}

QString AppDirs::documents()
{
	return QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation);
}

QString AppDirs::logDir()
{
	QString path = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
	path += "/Logs/";

	QDir log(path);

	if(!log.exists())
		log.mkpath(".");

	return path;
}

QString AppDirs::appData()
{
	return QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);
}
