#include "processhelper.h"
#include "appdirs.h"
#include "tempfiles.h"
#include "utilities/qutils.h"
#include "utils.h"
#include "log.h"

QProcessEnvironment ProcessHelper::getProcessEnvironmentForJaspEngine()
{
	QDir				programDir	= AppDirs::programDir();
	QString				engineExe	= programDir.absoluteFilePath("JASPEngine");
	QProcessEnvironment env			= QProcessEnvironment::systemEnvironment();
	
	env.insert("R_REMOTES_NO_ERRORS_FROM_WARNINGS", "true"); //Otherwise installing dependencies for modules can crap out on ridiculous warnings
	env.insert("RENV_PATHS_ROOT",					AppDirs::renvRootLocation());
	env.insert("RENV_PATHS_CACHE",					AppDirs::renvCacheLocations());

	
	//Seems a bit weird but we need to tell this to jaspBase so it can tell renv to run it again because that will be running in a subprocess. 
	//Which also means we have the following process -> subprocess structure while installing a dynamic module:
	// jasp -> JASPEngine with R-embedded -> Separate R -> separate instances of JASPEngine...
	env.insert("JASPENGINE_LOCATION",				engineExe); 
	
	QString TZDIR		= AppDirs::rHome() + "/share/zoneinfo";
	QString rHomePath	= AppDirs::rHome();
	QDir	rHome		( rHomePath );

	QString custom_R_library = "";
#ifdef JASP_DEBUG
	// allow an environment variables to specify the location of packages
	if (env.contains("JASP_R_Library"))
		custom_R_library = ":" + env.value("JASP_R_Library");
#endif
#ifdef _WIN32
#if defined(ARCH_32)
#define ARCH_SUBPATH "i386"
#else
#define ARCH_SUBPATH "x64"
#endif
	
			TZDIR		= shortenWinPaths(TZDIR);
	QString PATH		= shortenWinPaths(programDir.absoluteFilePath("R/library/RInside/libs/" ARCH_SUBPATH)) + ";" + shortenWinPaths(programDir.absoluteFilePath("R/library/Rcpp/libs/" ARCH_SUBPATH)) + ";" + shortenWinPaths(programDir.absoluteFilePath("R/bin/" ARCH_SUBPATH)) + ";" + shortenWinPaths(env.value("PATH")),
			R_HOME		= shortenWinPaths(rHome.absolutePath()),
			JAGS_HOME	= shortenWinPaths(programDir.absoluteFilePath("R/opt/jags/"));
			// JAGS_LIBDIR	= shortenWinPaths(programDir.absoluteFilePath("R/opt/jags/lib/"));
	
	Log::log() << "R_HOME set to " << R_HOME << std::endl;

	env.insert("PATH",				PATH);
	env.insert("R_HOME",			R_HOME);
	env.insert("JAGS_HOME",			JAGS_HOME);
	// env.insert("JAGS_LIBDIR",		JAGS_LIBDIR);
	
#undef ARCH_SUBPATH

	env.insert("R_LIBS",			 R_HOME + "/library");

	env.insert("R_ENVIRON",			"something-which-doesn't-exist");
	env.insert("R_PROFILE",			"something-which-doesn't-exist");
	env.insert("R_PROFILE_USER",	"something-which-doesn't-exist");
	env.insert("R_ENVIRON_USER",	"something-which-doesn't-exist");

    //Lets set LC_ALL to utf8 before the process starts.
    env.insert("LC_ALL", ".UTF8");			  

#elif __APPLE__

	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("JASP_R_HOME",		rHome.absolutePath()); //Used by the modified R script in jasp-required-files/Framework/etc/bin to make sure we use the actual R of JASP! (https://github.com/jasp-stats/INTERNAL-jasp/issues/452)
	env.insert("R_LIBS",			rHome.absoluteFilePath("library") + ":" + programDir.absoluteFilePath("R/library"));
	env.insert("JAGS_HOME",			rHome.absolutePath() + "/opt/jags/lib/JAGS/");
	// env.insert("JAGS_LIBDIR",		rHome.absolutePath() + "/opt/jags/lib/");

	//env.insert("R_ENVIRON",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE",			"something-which-doesnt-exist");
	//env.insert("R_PROFILE_USER",	"something-which-doesnt-exist");
	//env.insert("R_ENVIRON_USER",	"something-which-doesnt-exist");

	env.insert("LC_CTYPE",			"UTF-8"); //This isn't really a locale but seems necessary to get proper output from gettext on mac

#else  // linux
	env.insert("LD_LIBRARY_PATH",	rHome.absoluteFilePath("lib") + ":" + rHome.absoluteFilePath("library/RInside/lib") + ":" + rHome.absoluteFilePath("library/Rcpp/lib") + ":" + rHome.absoluteFilePath("site-library/RInside/lib") + ":" + rHome.absoluteFilePath("site-library/Rcpp/lib") + ":/app/lib/:/app/lib64/");
	env.insert("R_HOME",			rHome.absolutePath());
	env.insert("R_LIBS",			programDir.absoluteFilePath("R/library") + custom_R_library + ":" + rHome.absoluteFilePath("library") + ":" + rHome.absoluteFilePath("site-library"));
#endif

	env.insert("TZDIR",				TZDIR);
	env.insert("R_LIBS_SITE",		"");
	env.insert("R_LIBS_USER",		AppDirs::userRLibrary().toStdString().c_str());

#ifdef LINUX_LOCAL_BUILD
	// Sorry Joris, I still had to do this because I couldn't get your method to work!
	env.insert("R_LIBS_USER", (AppDirs::programDir().absolutePath().toStdString() + "/../R/library").c_str());
#endif

	return(env);	
}
