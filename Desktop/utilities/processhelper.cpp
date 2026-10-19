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
#include "processhelper.h"
#include "utilities/appdirs.h"
#include "qutils.h"
#include "log.h"
#include "dirs.h"

QProcessEnvironment ProcessHelper::getProcessEnvironmentForJaspEngine()
{
	QDir				programDir	= AppDirs::programDir();
	QString				engineExe	= programDir.absoluteFilePath("JASPEngine");
	QProcessEnvironment env			= QProcessEnvironment::systemEnvironment();
	
	env.insert("R_REMOTES_NO_ERRORS_FROM_WARNINGS", "true"); //Otherwise installing dependencies for modules can crap out on ridiculous warnings
	env.insert("RENV_PATHS_ROOT",					AppDirs::renvRootLocation());
	env.insert("RENV_PATHS_CACHE",					AppDirs::renvCacheLocations());
	env.insert("RENV_CONFIG_INSTALL_VERBOSE",		"TRUE"); // force renv package installation to be verbose (see https://rstudio.github.io/renv/reference/config.html#renv-config-install-verbose)

	
	//Seems a bit weird but we need to tell this to jaspBase so it can tell renv to run it again because that will be running in a subprocess. 
	//Which also means we have the following process -> subprocess structure while installing a dynamic module:
	// jasp -> JASPEngine with R-embedded -> Separate R -> separate instances of JASPEngine...
	env.insert("JASPENGINE_LOCATION",				engineExe);

	QString TZDIR		= AppDirs::rHome() + "/share/zoneinfo";
	QString rHomePath	= AppDirs::rHome();
	QDir	rHome		= rHomePath ;

	QString custom_R_library = "";
#ifdef JASP_DEBUG
	// allow an environment variables to specify the location of packages
	if (env.contains("JASP_R_Library"))
		custom_R_library = ":" + env.value("JASP_R_Library");
#endif
		env.insert("JASP_TMP_DIR", QString(Dirs::tempDir().c_str()));
#ifdef _WIN32
		//set R_TMP_DIR to appdata dir for win appcontainers if anyone ever change tmp behaviour it will fallback to this
		env.insert("TMPDIR", AppDirs::RtmpDir());
#if defined(ARCH_32)
#define ARCH_SUBPATH "i386"
#else
#define ARCH_SUBPATH "x64"
#endif

	ProcessHelper::fixPATHForWindows(env);

	QString _R_HOME		= rHome.absolutePath(),
			JAGS_HOME	= programDir.absoluteFilePath("R/opt/jags/");

	Log::log() << "R_HOME set to " << _R_HOME << std::endl;

	env.insert("R_HOME",			_R_HOME);
	env.insert("JAGS_HOME",			JAGS_HOME);
	
#undef ARCH_SUBPATH


	env.insert("R_LIBS",			programDir.absoluteFilePath("Modules/Tools/R_cpp_includes_library") + ";" + _R_HOME + "/library");

	env.insert("R_ENVIRON",			"something-which-doesn't-exist");
	env.insert("R_PROFILE",			"something-which-doesn't-exist");
	env.insert("R_PROFILE_USER",	"something-which-doesn't-exist");
	env.insert("R_ENVIRON_USER",	"something-which-doesn't-exist");
	env.insert("TZDIR",				TZDIR);

	//Lets set LC_ALL to utf8 before the process starts.
	//env.insert("LC_ALL", ".UTF8");			  
	//Actually we already set ACP to utf8 and that is enough!
	//Also setting this breaks the check for the output of R CMD config CC we use to avoid problems on mac...

#elif __APPLE__
	env.insert("PATH",				rHome.absoluteFilePath("bin") + ":" + env.value("PATH"));
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("RHOME",				rHome.absolutePath()); //For Rscript
	env.insert("JASP_R_HOME",		rHome.absolutePath()); //Used by the modified R script in jasp-required-files/Framework/etc/bin to make sure we use the actual R of JASP! (https://github.com/jasp-stats/INTERNAL-jasp/issues/452)
	env.insert("R_LIBS",			programDir.absoluteFilePath("../Modules/Tools/R_cpp_includes_library") + ":" + rHome.absoluteFilePath("library") + ":" + programDir.absoluteFilePath("R/library") + custom_R_library);
	env.insert("JAGS_HOME",			rHome.absolutePath() + "/opt/jags/lib/JAGS/");
	// env.insert("JAGS_LIBDIR",		rHome.absolutePath() + "/opt/jags/lib/");

	//env.insert("R_ENVIRON",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE_USER",	"something-which-doesnt-exist");
	//env.insert("R_ENVIRON_USER",	"something-which-doesnt-exist");

	env.insert("LC_CTYPE",			"UTF-8"); //This isn't really a locale but seems necessary to get proper output from gettext on mac
	env.insert("TZDIR",				TZDIR);
#elif FLATPAK_USED
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			"/app/Modules/Tools/R_cpp_includes_library:/app/lib64/R/library" + custom_R_library);
	env.insert("LD_LIBRARY_PATH",	"/app/Modules/Tools/R_cpp_includes_library/RInside/lib/:/app/lib64/R/lib/");
#else  // linux
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			programDir.absoluteFilePath("../Modules/Tools/R_cpp_includes_library") + ":" + programDir.absoluteFilePath("R/library") + custom_R_library);
#endif

	env.insert("R_LIBS_SITE",		"");
	env.insert("R_LIBS_USER",		AppDirs::userRLibrary().toStdString().c_str());

#ifdef LINUX_LOCAL_BUILD
	// Sorry Joris, I still had to do this because I couldn't get your method to work!
	env.insert("R_LIBS_USER", (AppDirs::programDir().absolutePath().toStdString() + "/../R/library").c_str());
#endif

	Log::log() <<	"PATH:            "	<< env.value("PATH")			<< "\n" <<
					"R_HOME:          "	<< env.value("R_HOME")			<< "\n" <<
					"R_LIBS:          "	<< env.value("R_LIBS")			<< "\n" <<
					"R_LIBS_USER:     "	<< env.value("R_LIBS_USER")		<< "\n" <<
					"LD_LIBRARY_PATH: "	<< env.value("LD_LIBRARY_PATH") << "\n" <<
					std::endl;

	return(env);	
}

#ifdef _WIN32 
///Overwrites the PATH with a simple clean one
void ProcessHelper::fixPATHForWindows(QProcessEnvironment & env)
{
	const QString R_ARCH =
#ifdef _WIN64
		"x64";
#else
		"i386";
#endif

	QStringList pathEntries = {
		AppDirs::programDir().absolutePath(),
		QDir(AppDirs::rHome()).absoluteFilePath("bin"),
		QDir(AppDirs::rHome()).absoluteFilePath("bin/" + R_ARCH),
		QDir(AppDirs::rHome()).absoluteFilePath("R/library/Rcpp/libs/"  + R_ARCH),
		QDir(AppDirs::rHome()).absoluteFilePath("R/library/RInside/libs/"  + R_ARCH),
		env.value("PATH")
	};

	env.insert("PATH", pathEntries.join((";")));

	Log::log() << "Windows PATH was changed to: '" << env.value("PATH", "???") << "'" << std::endl;
}
#endif 
