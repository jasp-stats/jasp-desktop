//
// Copyright (C) 2013-2026 University of Amsterdam
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

#include <QDir>
#include <QFile>
#include <QTextStream>
#include <QMutex>
#include <QMutexLocker>
#include <json/json.h>
#include <fstream>


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

QString AppDirs::userModulesLibDir()
{
    return AppDirs::userModulesDir() + "/module_libs/";
}

QString AppDirs::bundledModulesDir()
{
	static QString folder;
#ifdef _WIN32
	//Everything is read straight from the install tree: module_libs/<module>/ holds real directory
	//copies for the module package and its JASP-module deps, plain R deps load from binary_pkgs
	//micro-libraries (moduleExtraLibPaths). The appData "junction farm" that used to mirror this
	//dir for MSI/MSIX is gone (jasp-issues #4586) and every consumer only ever reads.
	folder = programDir().absoluteFilePath("Modules") + '/';
#elif __APPLE__
	 folder = programDir().absoluteFilePath("../Modules/");
#elif FLATPAK_USED
	folder = "/app/bin/../Modules/";
#else  //Normal linux build
	folder = programDir().absoluteFilePath("../Modules") + '/';
#endif

	return folder;
}

QString AppDirs::bundledModulesLibDir()
{
	return AppDirs::bundledModulesDir() + "/module_libs/";
}

#ifdef _WIN32
//Parses the module's own manifest (manifests/<name>_manifest.json, two dirs up from its library
//dir) and resolves every "hash => pkg_version" entry to the micro-library dir binary_pkgs/<hash>/.
//The package name is read from the directory itself (ground truth) rather than the mapping string.
//Old-layout (flat) hash dirs are skipped: with DESCRIPTION at their root only legacy links can
//load them, and moduleExtraLibPaths/modulePkgMap must then simply yield nothing for them.
static QList<QPair<QString, QString>> moduleMicroLibraries(const QString & moduleRLibrary, const QString & moduleName)
{
	QList<QPair<QString, QString>> out;

	QDir	moduleLibDir(moduleRLibrary);
	if (!(moduleLibDir.cdUp() && moduleLibDir.cdUp()))		//.../module_libs -> <root>
		return out;

	//jaspModuleBundleManager writes the manifest as manifests/<name>_manifest.json, where <name>
	//is the same full module name as the module_libs dir (jasp-prefixed).
	QDir	manifestsDir(moduleLibDir.absoluteFilePath("manifests"));
	QString	manifestPath = manifestsDir.absoluteFilePath(moduleName + "_manifest.json");
	if (!QFileInfo::exists(manifestPath))	//defensive: in case a name ever arrives without its jasp prefix
		manifestPath = manifestsDir.absoluteFilePath("jasp" + moduleName + "_manifest.json");

	std::ifstream	in(manifestPath.toStdString());
	Json::Value		root;

	if (!(in.good() && Json::Reader().parse(in, root)))
		return out;

	const Json::Value & mapping = root.get("mapping", Json::arrayValue);
	if (!mapping.isArray())
		return out;

	for (const Json::Value & entry : mapping)
	{
		//"<hash> => <pkgname>_<version>": we only need the hash, the package name lives inside the dir.
		QStringList parts = QString::fromStdString(entry.asString()).split(" => ");
		if (parts.size() != 2 || parts[0].isEmpty())
			continue;

		//binary_pkgs is a sibling of module_libs/manifests under the module root; for bundled modules
		//on Windows this resolves directly into the (read-only) install tree.
		QString hashDir = QDir::cleanPath(moduleLibDir.absoluteFilePath("binary_pkgs/" + parts[0]));

		if (!QDir(hashDir).exists() || QFile::exists(hashDir + "/DESCRIPTION"))
			continue;

		QStringList subDirs = QDir(hashDir).entryList(QDir::Dirs | QDir::NoDotAndDotDot);
		if (subDirs.size() != 1)			//a micro-library holds exactly one package
			continue;

		QPair<QString, QString> pair(subDirs.first(), hashDir);
		if (!out.contains(pair))
			out.append(pair);
	}

	return out;
}
#endif

QStringList AppDirs::moduleExtraLibPaths(const QString & moduleRLibrary, const QString & moduleName)
{
#ifdef _WIN32
	//Modules live under a shared root: <root>/module_libs/<module>/, <root>/manifests/jasp<module>.json
	//and <root>/binary_pkgs/<hash>. The manifest (written by jaspModuleBundleManager, the same routine at
	//build time and at user-install time) contains a "mapping" of "<hash> => <pkgname>_<version>" strings
	//listing every package a module needs — including the module package itself. With the old extraction
	//layout a hash dir IS the package root and only the junction farm can give it its name back; with the
	//newer nested extraction (binary_pkgs/<hash>/<pkgname>) every hash dir is a micro-library that can be
	//passed to .libPaths() directly. We detect which is which per hash dir (DESCRIPTION at its root = old)
	//so old installs simply yield nothing and keep loading through their legacy links as they always have.
	//No caching (anymore): with rModuleCall no longer re-sending .libPaths() per analysis call,
	//this runs only when a module-load request is built — once per (re)install/load — so it always
	//reflects the tree as it is right now and can never serve a previous layout's stale entries.
	QStringList libPaths;

	for (const auto & p : moduleMicroLibraries(moduleRLibrary, moduleName))
		if (!libPaths.contains(p.second))
			libPaths.append(p.second);

	if (!libPaths.isEmpty())
		Log::log() << "AppDirs::moduleExtraLibPaths() found " << libPaths.size() << " direct libpath(s) for module '" << moduleName.toStdString() << "' from its manifest" << std::endl;

	return libPaths;
#else
	//Linux/macOS have proper symlinks: the manager links module_libs/<mod>/<pkg> directly into
	//binary_pkgs and those links ship fine in the packages, so no extra libpaths are needed there.
	Q_UNUSED(moduleRLibrary);
	Q_UNUSED(moduleName);
	return QStringList();
#endif
}

QList<QPair<QString, QString>> AppDirs::modulePkgMap(const QString & moduleRLibrary, const QString & moduleName)
{
#ifdef _WIN32
	//Ordered pkg -> lib-dir pairs mirroring the .libPaths() order of DynamicModule::getLibPathsToUse():
	//the module_libs entry first (real copies of the module package and its JASP-module deps), then the
	//binary_pkgs micro-libraries from the manifest, then R's own library for anything only it provides.
	//The engine turns this into options(JASP.find.package.map) and installs a find.package fast-path
	//that consults the map before sweeping all libpaths (see engine.cpp and windows-binary-pkgs-libpaths.md).
	//No caching (anymore): runs only when a module-load request is built — see moduleExtraLibPaths.
	QList<QPair<QString, QString>> map;
	auto addPkg = [&map](const QString & pkg, const QString & dir)
	{
		for (const auto & e : map)
			if (e.first == pkg)
				return;			//first hit in .libPaths() order wins, exactly like stock find.package
		map.append(QPair<QString, QString>(pkg, dir));
	};

	for (const QString & pkg : QDir(moduleRLibrary).entryList(QDir::Dirs | QDir::NoDotAndDotDot))
		addPkg(pkg, QDir::cleanPath(moduleRLibrary));

	for (const auto & p : moduleMicroLibraries(moduleRLibrary, moduleName))
		addPkg(p.first, p.second);

	const QString rLib = QDir(AppDirs::rHome()).absoluteFilePath("library");
	//The base/recommended packages hardcoded in stock find.package() already return without any
	//libpaths sweep, so mapping them gains nothing — and intercepting them (base especially)
	//could only diverge from stock's early-return. Skip exactly the set stock hardcodes.
	static const QStringList	stockFastPkgs = { "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils" };
	for (const QString & pkg : QDir(rLib).entryList(QDir::Dirs | QDir::NoDotAndDotDot))
		if (!stockFastPkgs.contains(pkg))
			addPkg(pkg, rLib);

	if (!map.isEmpty())
		Log::log() << "AppDirs::modulePkgMap() built a map of " << map.size() << " package(s) for module '" << moduleName.toStdString() << "'" << std::endl;

	return map;
#else
	Q_UNUSED(moduleRLibrary);
	Q_UNUSED(moduleName);
	return QList<QPair<QString, QString>>();
#endif
}

QString AppDirs::processPath(const QString & path)
{
	return path;
}


QString AppDirs::documents()
{
	return processPath(QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation));
}

QString AppDirs::sandboxedDocuments()
{
	const QString name = "JASP_Sandbox";
    QDir res(AppDirs::documents());
	res.mkdir(name);
	res.cd(name);
	return res.absolutePath();
}

QString AppDirs::clipboardDir()
{
	QString path;
#ifdef _WIN32
	path = sandboxedDocuments();
	path += "/Clipboard/";
#else
	path = tq(Dirs::tempDir()) + "/clipboard/";
#endif

	QDir clipboard(path);

	if(!clipboard.exists())
		clipboard.mkpath(".");

	return path;
}

void AppDirs::purgeClipboard()
{
	QDir clipboard(clipboardDir());
	clipboard.removeRecursively();
}

QString AppDirs::logDir()	
{
    QString path = sandboxedDocuments();
	path += "/Logs/";

	QDir log(path);

	if(!log.exists())
		log.mkpath(".");

	return path;
}

QString AppDirs::autoSaveDir()
{
	QString path = appData();
	path += "/AutoSaves/";

	QDir autoSave(path);

	if(!autoSave.exists())
		autoSave.mkpath(".");

	return path;
}

QString AppDirs::appData(bool roaming)
{
	if(roaming)
		return processPath(QStandardPaths::writableLocation(QStandardPaths::AppDataLocation));
	else
		return processPath(QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation));
}

QString AppDirs::RtmpDir() {
	QString tmp = appData(false) + "/R_TMP_DIR/";
	QDir tmpDir(tmp);

	if(!tmpDir.exists())
		tmpDir.mkpath(".");
	
	return tmpDir.absolutePath();
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
	QDir path = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	if(QCoreApplication::applicationName() == "JASP" || QCoreApplication::applicationName() == "JASPDesktop")
		return path;
	
	return path.absoluteFilePath("../Desktop/"); //The testapplications arent in Desktop/ and I dont want to pollute the folder, so instead this
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
	
	QString dynamicCache = renvRootLocation() + "/" + renvCacheName;
	#ifdef FLATPAK_USED
		QString staticCache = "/app/lib64/renv-cache/";
	#else
	QString staticCache = processPath(programDir().absoluteFilePath("Modules/renv-cache"));
	if(!QFile(staticCache).exists()) {
		Log::log() << "Looks like this is a local build lets try to set the right static renv-cache path" << std::endl;
		staticCache = processPath(programDir().absoluteFilePath("../Modules/renv-cache"));
	}

	#endif
	
	const QChar separator =
#ifdef WIN32
							';';
#else
							':';
#endif
	
    return dynamicCache + separator + staticCache;
	
}

#ifdef __APPLE__
QString AppDirs::devModulePatchDir()
{
	QString path = appData();
	path += "/_DevModulePatchDir/";

	return path;
}
#endif
