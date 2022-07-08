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


#include <QDir>

#include "utilities/application.h"
#include <QQuickWindow>
#include "utilities/settings.h"
#include <QtWebEngineQuick/qtwebenginequickglobal.h>
#include <boost/filesystem.hpp>
#include <codecvt>
#include "appinfo.h"
#include <iostream>
#include "timers.h"
#include <QMessageBox>
#include "utilities/plotschemehandler.h"

const std::string	jaspExtension		= ".jasp",
					unitTestArg			= "--unitTest",
					saveArg				= "--save",
					timeOutArg			= "--timeOut=",
					junctionArg			= "--junctions",
					removeJunctionsArg	= "--removeJunctions";

#ifdef _WIN32

#include "utilities/processhelper.h"
// This function simply sets the proper environment of jaspengine, and starts it in junction-fixing mode or remove-junction mode.
// The junction-fixining mode is called after the installer runs to fix the junctions in Modules that actually point to renv-cache instead of nowhere
// The remove-junction mode is called before the uninstaller runs to remove all these junstions: doing this prevent the uninstaller to run for ages.
bool runJaspEngineJunctionFixer(int argc, char *argv[], bool removeJunctions = false, bool exitAfterwards = true)
{
	QApplication	*	app		= exitAfterwards ? new QApplication(argc, argv) : nullptr;
	QProcessEnvironment env		= ProcessHelper::getProcessEnvironmentForJaspEngine(false, false);
	QString				workDir = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absolutePath();
	
	QProcess engine;
	
	engine.setProcessChannelMode(QProcess::ForwardedChannels);
	engine.setProcessEnvironment(env);
	engine.setWorkingDirectory(workDir);
	engine.setProgram("JASPEngine.exe");
	if (removeJunctions)	engine.setArguments({"--removeJunctions", workDir});
	else					engine.setArguments({"--recreateJunctions", workDir});
	engine.start();
	
	if(!engine.waitForStarted())		{	std::cerr << "JASPEngine failed to start for junctions!" << std::endl;						exit(2); }
	//Something like 10 minutes tops should be more than enough for the junctions to be replaced and otherwise it probably crashed?
	if(!engine.waitForFinished(600000)) {	std::cerr << "JASPEngine started but timed out before finishing junctions!" << std::endl;	exit(3); }

	bool worked = engine.exitCode() == 0;
	
	std::cout << (removeJunctions ? "Removing" : "Replacing") << " junctions with JASPEngine seems to have " << (worked ? "worked." : "failed.")  << std::endl;

	
	if(exitAfterwards)
		exit(engine.exitCode());
	
	return worked;
}

#endif


#ifdef _WIN32
#define QSTRING_FILE_ARG QString::fromLocal8Bit
#else
#define QSTRING_FILE_ARG QString::fromStdString
#endif

void parseArguments(int argc, char *argv[], std::string & filePath, bool & unitTest, bool & dirTest, int & timeOut, bool & save, bool & logToFile, bool & hideJASP, bool & safeGraphics, bool & LC_CTYPE_C, bool & LC_CTYPE_system)
{
	filePath		= "";
	unitTest		= false;
	dirTest			= false;
	save			= false;
	logToFile		= false;
	hideJASP		= false;
	safeGraphics	= false;
	LC_CTYPE_C		= false;
	LC_CTYPE_system	= false;
	timeOut			= 10;

	bool letsExplainSomeThings = false;

	std::vector<std::string> args(argv + 1, argv + argc); // make the arguments a little less annoying to work with

	for(int arg = 0; arg < args.size(); arg++)
	{
		if(args[arg] == saveArg)								save					= true;
		else if(args[arg] == "--help" || args[arg] == "-h")		letsExplainSomeThings	= true;
		else if(args[arg] == "--logToFile")						logToFile				= true;
		else if(args[arg] == "--hide")							hideJASP				= true;
		else if(args[arg] == "--safeGraphics")					safeGraphics			= true;
		else if(args[arg] == "--setLC_CTYPE_C")					LC_CTYPE_C				= true;
		else if(args[arg] == "--setLC_CTYPE_system")			LC_CTYPE_system			= true;
#ifdef _WIN32
		else if(args[arg] == junctionArg)						runJaspEngineJunctionFixer(argc, argv, false); //Run the junctionfixer, it will exit the application btw!
		else if(args[arg] == removeJunctionsArg)				runJaspEngineJunctionFixer(argc, argv, true);  //Remove the junctions
#endif
		else if(args[arg] == "--unitTestRecursive")
		{
			if(arg >= args.size() - 1)
				letsExplainSomeThings = true;
			else
			{
				QDir folder(QSTRING_FILE_ARG(args[arg + 1].c_str()));

				if(!folder.exists())
				{
					std::cerr << "Folder for unitTestRecursive " << folder.absolutePath().toStdString() << " does not exist!" << std::endl;
					exit(1);
				}

				dirTest = true;
				filePath = args[arg + 1];
			}
		}
		else if(args[arg] == unitTestArg)
		{
			if(arg >= args.size() - 1)
				letsExplainSomeThings = true;
			else
			{
				filePath = args[arg + 1];

				QFileInfo testMe(QSTRING_FILE_ARG(filePath.c_str()));

				if(!testMe.exists())
				{
					std::cerr << "File for unitTest " << testMe.absoluteFilePath().toStdString() << " does not exist!" << std::endl;
					letsExplainSomeThings = true;
				}

				bool	isJaspFile	= filePath.size() >= jaspExtension.size()  &&  filePath.substr(filePath.size() - jaspExtension.size()) == jaspExtension;

				if(!isJaspFile && !letsExplainSomeThings)
				{
					std::cerr << "File for unitTest " << filePath << " is not a JASP file!" << std::endl;
					letsExplainSomeThings = true;
				}

				unitTest = true;
			}
		}
		else if(args[arg].size() > timeOutArg.size() && args[arg].substr(0, timeOutArg.size()) == timeOutArg)
		{
			std::string time			= timeOutArg.substr(timeOutArg.size());
			size_t		convertedChars	= 0;
			int			convertedTime	= 0;
			try								{ convertedTime = std::stoi(time, &convertedChars); }
			catch(std::invalid_argument &)	{}
			catch(std::out_of_range &)		{}

			if(convertedChars > 0)
				timeOut = convertedTime;
		}
		else
		{
			const std::string	remoteDebuggingPort = "--remote-debugging-port=",
								qmlJsDebug			= "-qmljsdebugger",
								dashing				= "--";

			auto startsWith = [&](const std::string checkThis)
			{
				return args[arg].size() > checkThis.size() && args[arg].substr(0, checkThis.size()) == checkThis;
			};

			if(args[arg] == "-platform")
				arg++; // because it is always followed by the actual platform one wants to use (minimal for example)
			else if(!(startsWith(remoteDebuggingPort) || startsWith(qmlJsDebug))) //Just making sure it isnt something else that should be allowed.
			{
				if(startsWith(dashing))
				{
					//So, it looks like an option, but not one we recognize, maybe it is meant for qt/chromium
					std::cout << "Argument '" << args[arg] << "' was not recognized by JASP, but it might be recognized by one of it's components in Qt (such as chromium), it will be passed on. If you really expected JASP to do something with it check '--help' again." << std::endl;
				}
				else
				{
					//if it isn't anything else it must be a file to open right?
					// Well yes, but it might also be the url of an OSF file, then we do not need to check if it exists.

					QFileInfo openMe(QSTRING_FILE_ARG(args[arg].c_str()));

					if(startsWith("https:") || startsWith("http:") || openMe.exists())
						filePath = args[arg];
					else
					{
						std::cerr << "File to open " << args[arg] << " does not exist!" << std::endl;
						letsExplainSomeThings = true;
					}
				}
			}
		}
	}

	if(letsExplainSomeThings)
	{
		std::cerr	<< "JASP can be started without arguments, or the following: { --help | -h | filename | --unitTest filename | --unitTestRecursive folder | --save | --timeOut=10 | --logToFile | --hide } \n"
					<< "If a filename is supplied JASP will try to load it. \nIf --unitTest is specified JASP will refresh all analyses in \"filename\" (which must be a JASP file) and see if the output remains the same and will then exit with an errorcode indicating succes or failure.\n"
					<< "If --unitTestRecursive is specified JASP will go through specified \"folder\" and perform a --unitTest on each JASP file. After it has done this it will exit with an errorcode indication succes or failure.\n"
					<< "For both testing arguments there is the optional --save argument, which specifies that JASP should save the file after refreshing it.\n"
					<< "For both testing arguments there is the optional --timeout argument, which specifies how many minutes JASP will wait for the analyses-refresh to take. Default is 10 minutes.\n"
					<< "If --logToFile is specified then JASP will try it's utmost to write logging to a file, this might come in handy if you want to figure out why JASP does not start in case of a bug.\n"
					<< "If --hide is specified then JASP will not be shown during recursive testing.\n"
					<< "If --safeGraphics is specified then JASP will be started with software rendering enabled.\n"
			   #ifdef _WIN32
					<< "If --setLC_CTYPE_C is specified JASP will make sure LC_CTYPE is set to \"C\" for the engine, --setLC_CTYPE_system is specified the system default is used."
					<< "If --junctions is specified JASP will recreate the junctions in Modules/ to renv-cache/, this needs to be done at least once after install, but is usually triggered automatically."
			   #endif
					<< "This text will be shown when either --help or -h is specified or something else that JASP does not understand is given as argument.\n"
					<< std::flush;

		exit(1);
	}
}

#define SEPARATE_PROCESS

void recursiveFileOpener(QFileInfo file, int & failures, int & total, int & timeOut, int argc, char *argv[], bool save, bool hideJASP)
{
	const QString jaspExtension(".jasp");

	//std::cout << "recursiveFileOpener in " << file.absoluteFilePath().toStdString() << std::endl;

	if(file.isDir())
	{
		//std::cout << "it is a directory and " << (file.exists() ? "exists" : "does not exist") << std::endl;

		QDir dir(file.absoluteFilePath());

		//std::cout << "QDir dir: " << dir.path().toStdString() << " has " << files.size() << " subfiles!" << std::endl;

		for(QFileInfo & subFile : dir.entryInfoList(QDir::Filter::NoDotAndDotDot | QDir::Files | QDir::Dirs))
			recursiveFileOpener(subFile, failures, total, timeOut, argc, argv, save, hideJASP);

	}
	else if(file.isFile())
	{
		//std::cout << "it is a file" << std::endl;

		if(file.absoluteFilePath().indexOf(jaspExtension) == file.absoluteFilePath().size() - jaspExtension.size())
		{
			//std::cout << "it has the .jasp extension so we will start JASP" << std::endl;
#ifndef SEPARATE_PROCESS
			std::cout << "Found a JASP file (" << file.absoluteFilePath().toStdString() << ") going to start JASP" << std::endl;
#endif

			int result = 1;

			try{
#ifdef SEPARATE_PROCESS
				QProcess subJasp;
				subJasp.setProgram(argv[0]);
				QStringList arguments({"--unitTest", file.absoluteFilePath()});

				if(save)
					arguments << "--save";

				arguments << QString::fromStdString("--timeOut="+std::to_string(timeOut));

				if(hideJASP)
					arguments << "-platform" << "minimal";

				std::cout << "Starting subJASP with args: " << arguments.join(' ').toStdString() << std::endl;
				subJasp.setArguments(arguments);
				subJasp.start();

				subJasp.waitForFinished((timeOut * 60000) + 10000);

				std::cerr << subJasp.readAllStandardError().toStdString() << std::endl;

				result = subJasp.exitCode();
#else
				//This seems to crash for some reason
				result = Application(argc, argv, file.absoluteFilePath(), true, timeOut).exec();
#endif
			}
			catch(...) { result = -1; }


			std::cout << "JASP file " << file.absoluteFilePath().toStdString() << (result == 0 ? " succeeded!" : " failed!") << std::endl;

			if(result != 0)
				failures++;

			total++;
		}
	}
}

int main(int argc, char *argv[])
{
#ifdef _WIN32
	// Temporary fix for #2322 by disabling opengl drawing on windows
	// Follow status of https://bugreports.qt.io/browse/QTBUG-61430 for
	// future permanent fix.
	// This does slow down QML quite a bit and disables gradients
	// qputenv("QT_QUICK_BACKEND", "software");
	//Turned this off because it causes some weird resizing problems QCoreApplication::setAttribute(Qt::AA_UseOpenGLES); //might fix weirdlooking QML on Windows when using not-so-goo drivers? ( https://github.com/jasp-stats/jasp-desktop/issues/2669 )
	
	setlocale(LC_ALL, ".UTF8"); //use utf8
#endif

	std::string filePath;
	bool		unitTest,
				dirTest,
				save,
				logToFile,
				hideJASP,
				safeGraphics,
				setLC_CTYPE_C,
				setLC_CTYPE_system;
	int			timeOut;

	QCoreApplication::setOrganizationName("JASP");
	QCoreApplication::setOrganizationDomain("jasp-stats.org");
	QCoreApplication::setApplicationName("JASP");
	
	parseArguments(argc, argv, filePath, unitTest, dirTest, timeOut, save, logToFile, hideJASP, safeGraphics, setLC_CTYPE_C, setLC_CTYPE_system);
	
	if(safeGraphics)		Settings::setValue(Settings::SAFE_GRAPHICS_MODE, true);
	else					safeGraphics = Settings::value(Settings::SAFE_GRAPHICS_MODE).toBool();
	
	//Make sure the strings are the same as enum winLcCtypeSetting in enginedefinitions.h
	if(setLC_CTYPE_C)		Settings::setValue(Settings::LC_CTYPE_C_WIN, "alwaysC"); 
	if(setLC_CTYPE_system)	Settings::setValue(Settings::LC_CTYPE_C_WIN, "neverC");

	QString filePathQ(QSTRING_FILE_ARG(filePath.c_str()));

	//Now, to allow us to add some arguments we store the ones we got in a vector
	std::vector<std::string> args(argv, argv + argc);

	boost::filesystem::path::imbue(std::locale( std::locale(), new std::codecvt_utf8_utf16<wchar_t>() ) );
	
	if(!dirTest)
		//try
		{
			if(safeGraphics)
			{
				std::cout << "Setting special options for software rendering (aka safe graphics)." << std::endl;
				QCoreApplication::setAttribute(Qt::AA_UseSoftwareOpenGL);
				args.push_back("--disable-gpu");
				putenv("LIBGL_ALWAYS_SOFTWARE=1");
			}

			PlotSchemeHandler::createUrlScheme(); //Needs to be done *before* creating PlotSchemeHandler instance and also before QApplication is instantiated

			QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts);
			QCoreApplication::setAttribute(Qt::AA_SynthesizeTouchForUnhandledMouseEvents, false); //To avoid weird splitterbehaviour with QML and a touchscreen


			QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == .

#ifdef _WIN32
			QQuickWindow::setTextRenderType(QQuickWindow::NativeTextRendering); //Doesn't improve it on anything 'cept windows
#endif

			//Now we convert all these strings in args back to an int and a char * array.
			//But to keep things easy, we are going to copy the old argv to avoid duplication (or messing up the executable name)
			char** argvs = new char*[args.size()];


			std::cout << "Making new argument list for Application startup:";

			for(size_t i = 0; i< args.size(); i++)
				{
					argvs[i] = new char[				args[i].size() + 1]; // +1 for null delimiter
					memset(argvs[i], '\0',				args[i].size() + 1);
					memcpy(argvs[i], args[i].data(),	args[i].size());
					argvs[i][							args[i].size()] = '\0';
					std::cout << " " << argvs[i];
				}

			std::cout << "\nStarting JASP " << AppInfo::version.asString() << " from commit " << AppInfo::gitCommit << " and branch " << AppInfo::gitBranch << std::endl;

			int		argvsize  = args.size();
			//To be all neat we should clean up all this stuff after we are done running JASP, but on the other hand the memory will be thrown out anyway after exit so why bother.

			JASPTIMER_START("JASP");

			QtWebEngineQuick::initialize(); // We can do this here and not in MainWindow::loadQML() (before QQmlApplicationEngine is instantiated) because that is called from a singleshot timer. And will only be executed once we enter a.exec() below!
			std::cout << "QtWebEngineQuick initialized" << std::endl;

			Application a(argvsize, argvs);
			
			std::cout << "Application initialized" << std::endl;

			PlotSchemeHandler plotSchemeHandler; //Makes sure plots can still be loaded in webengine with Qt6

#ifdef _WIN32
			// Since we introduced renv to JASP the win installer needs to recreate the junctions from Modules -> renv-cache on install. Because they do not support relative paths
			// For this JASP has the --junctions argument, and is run by JASP-*.msi during install to make sure everything is ready.
			// However, we also want to support ZIP distributions of jasp, and there is no installer. But the good thing is it also means the user has write access to the main folder. Which means we can fix it now.
			QFile junctionsCreationLog("junctions-recreated-successfully.log");
			QFile jaspEngine("JASPEngine.exe");
						
			if(jaspEngine.exists() && !junctionsCreationLog.exists())
			{
				QMessageBox::information(nullptr, Application::tr("One time setup for JASP Modules"), Application::tr("JASP has been installed from a zip and it needs to recreate certain paths for your analyses to work.\n\nPlease be patient and wait for the application to show before attempting to start JASP again."));
				
				if(!runJaspEngineJunctionFixer(argc, argv, false, false))
				{					
					std::cerr << "Modules folder missing and couldn't be created!\nContact the JASP team for support, or try the MSI." << std::endl;	
					exit(254);
				}
			}
#endif
			
			a.init(filePathQ, unitTest, timeOut, save, logToFile);
			
			try 
			{
				std::cout << "Entering eventloop" << std::endl;
				
				int exitCode = a.exec();
				JASPTIMER_STOP("JASP");
				JASPTIMER_PRINTALL();
				return exitCode;
			}
			catch(std::exception & e)
			{
				std::cerr << "Uncaught std::exception! Was: " << e.what() << "\n";
				return -1;
			}
			catch(...)
			{
				std::cerr << "Uncaught ???\n";
				return -1;
			}
		}
	else
	{
		int		failures	= 0,
				total		= 0;

		recursiveFileOpener(QFileInfo(filePathQ), failures, total, timeOut, argc, argv, save, hideJASP);

		if(total == 0)
		{
			std::cerr << "Couldn't find any jasp-files in specified directory " << filePath << ", it is be treated as a failure to notify you of this!" << std::endl;
			exit(2);
		}

		if(failures > 0)
		{
			std::cerr << "Finished running test, " << failures << " out of " << total << " jasp files FAILED!" << std::endl;
			exit(1);
		}

		std::cout << "All " << total << " jasp files succeeded in refreshing and displaying the same data afterwards!" << std::endl;
		exit(0);
	}

}
