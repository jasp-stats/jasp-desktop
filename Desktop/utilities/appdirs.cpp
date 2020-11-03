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
#include "appinfo.h"

#include "log.h"

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
	QString path = appData() + "/AnalysisDefaults";
	QDir dir(path);
	dir.mkpath(".");

	return path;
}

QString AppDirs::userRLibrary()
{
	QString path = appData();
	path += "/libraryR/";

	return path;
}

QString AppDirs::userModulesDir()
{
	QString path = appData();
	path += "/Modules/";

	return path;
}

QString AppDirs::bundledModulesDir()
{
	static QString folder =
#ifdef __APPLE__
	 (QDir(programDir().absoluteFilePath("../Resources/Modules")).exists() ? programDir().absoluteFilePath("../Resources/Modules") : programDir().absoluteFilePath("Modules")) + '/';
#elif _WIN32
	 programDir().absoluteFilePath("Modules") + '/';
#elif FLATPAK_USED
	"/app/bin/Modules/";
#else  //Normal linux build
	programDir().absoluteFilePath("Modules") + '/';
#endif
	
	return folder ;
}

QString AppDirs::processPath(const QString & path)
{
#ifdef _WIN32
	return QString::fromStdWString(Utils::getShortPathWin(path.toStdWString()));
#else
	return path;
#endif
}


QString AppDirs::documents()
{
	return processPath(QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation));
}

QString AppDirs::logDir()
{
	QString path = appData();
	path += "/Logs/";

	QDir log(path);

	if(!log.exists())
		log.mkpath(".");

	return path;
}

QString AppDirs::appData()
{
	return processPath(QStandardPaths::writableLocation(QStandardPaths::AppDataLocation));
}

QString AppDirs::rHome()
{
	
#ifdef _WIN32
	QString rHomePath = programDir().absoluteFilePath("R");
#elif defined(__APPLE__)
    QString rHomePath = programDir().absoluteFilePath("../Frameworks/R.framework/Versions/" + QString::fromStdString(AppInfo::getRVersion()) + "/Resources");
#else //linux

#ifndef R_HOME
	QString rHomePath = programDir().absoluteFilePath("R/lib/libR.so");
	if (QFileInfo(rHomePath).exists() == false)
#ifdef FLATPAK_USED
		rHomePath = "/app/lib64/R/"; //Tools/flatpak/org.jaspstats.JASP.json also sets R_HOME to /app/lib64 for 32bits...
#else
		rHomePath = "/usr/lib/R/";
#endif
#else
	QString rHomePath = QDir::isRelativePath(R_HOME) ? programDir().absoluteFilePath(R_HOME) : R_HOME;
#endif
#endif
	
	return rHomePath;
}

QDir AppDirs::programDir()
{
	return QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();	
}
