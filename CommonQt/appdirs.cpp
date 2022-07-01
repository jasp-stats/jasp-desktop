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
#include "qutils.h"
#include "utils.h"

#include "dirs.h"
#include <QStandardPaths>
#include "appinfo.h"

#include "log.h"

using namespace std;

QString AppDirs::examples()
{
    static QString dir = QDir(tq(Dirs::resourcesDir()) + tq("Data Sets")).canonicalPath();
	return dir;
}

QString AppDirs::help()
{
	static QString dir = QDir(tq(Dirs::resourcesDir()) + tq("Help")).canonicalPath();

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
	 programDir().absoluteFilePath("../Modules/");
#elif _WIN32
	 programDir().absoluteFilePath("Modules") + '/';
#elif FLATPAK_USED
	"/app/bin/../Modules/";
#else  //Normal linux build
	programDir().absoluteFilePath("../Modules") + '/';
#endif
	// @Joris, I think these guys should be one level up,
	// they are not binaries, so, they should not be in 
	// the binary folder in my opinion.
	
	return folder;
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

/**
 * @brief 		This returns the path to R home directory, where `bin/`, `lib/`, `library/`, etc.
 *          	are located.
 * 
 * @details 	On macOS, R lives inside the Frameworks folder, both on build and inside the
 *           	the App Bundle, that's a level up from JASP, and JASPEngine. On Windows, R is in the same
 *            	level as JASP binaries, and on Linux, R might lives in different location, that's why we
 *             	have a bit of a logic there to figure out where it is. 
 * 
 * @note       	The Linux logic is most likely not necessary since rHomeDir is being set at config
 *              time by CMake and that should always point to the right place no matter what. I will
 *              revisit this as soon as we have a working Flatpak build.
 *              
 * @return 		Path to R home directory
 */
QString AppDirs::rHome()
{

	QString rHomePath;

#ifdef _WIN32
	rHomePath = programDir().absoluteFilePath("R");
#endif

#if defined(__APPLE__)
	rHomePath = programDir().absoluteFilePath("../Frameworks/R.framework/Versions/" + QString::fromStdString(AppInfo::getRDirName()) + "/Resources");
#endif
    
#ifdef linux

if (AppDirs::rHomeDir().isEmpty())
{
	rHomePath = programDir().absoluteFilePath("R/lib/libR.so");
	if (QFileInfo(rHomePath).exists() == false)
#ifdef FLATPAK_USED
		rHomePath = "/app/lib64/R/"; //Tools/flatpak/org.jaspstats.JASP.json also sets R_HOME to /app/lib64 for 32bits...
#else
		rHomePath = "/usr/lib/R/";
#endif
} else {
	Log::log() << "AppDirs::rHomeDir() is set " << AppDirs::rHomeDir() << std::endl;
	rHomePath = QDir::isRelativePath(AppDirs::rHomeDir()) ? programDir().absoluteFilePath(AppDirs::rHomeDir()) : AppDirs::rHomeDir();
}
#endif
	
	return rHomePath;
}

QDir AppDirs::programDir()
{
	return QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();	
}

//After getting an error on giving "consent" to renv to do stuff I checked the page https://rstudio.github.io/renv/reference/paths.html
//I think it would be good to make sure the root-renv folder is also within the appdata of JASP and not in their own, because then we would be partially clobbering users own renv stuff
QString AppDirs::renvRootLocation()
{
	const char * renvRootName = "renv";
	
	QDir(appData()).mkpath(renvRootName); //create it if missing
	
	return appData() + "/" + renvRootName;
}

QString AppDirs::renvCacheLocations()
{
	const char * renvCacheName = "cache";
	
	QDir(renvRootLocation()).mkpath(renvCacheName); //create it if missing
	
	QString dynamicCache = renvRootLocation() + "/" + renvCacheName,
			staticCache  = processPath(QDir(bundledModulesDir()).absoluteFilePath("renv-cache"));
	
	const QChar separator =
#ifdef WIN32
							';';
#else
							':';
#endif
	
    return dynamicCache + separator + staticCache;
	
}
