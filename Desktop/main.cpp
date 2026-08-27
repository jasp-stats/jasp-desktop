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


#include <QDir>

#include "utilities/application.h"
#include "utilities/settings.h"
#include <QtWebEngineQuick/qtwebenginequickglobal.h>
#include <codecvt>
#include "appinfo.h"
#include <iostream>
#include "timers.h"
#include <QMessageBox>
#include "utilities/plotschemehandler.h"
#include "utilities/imgschemehandler.h"
#include "utilities/personaschemehandler.h"
#include <json/json.h>
#include "utilities/appdirs.h"
#include "parsedarguments.h"

#ifdef linux
#include "utilities/qmlutils.h"
#endif

#ifdef _WIN32
#include "utilities/dynamicruntimeinfo.h"

bool createJunctions()
{
    QProcess junctionTool;

    QString toolPath = AppDirs::programDir().absoluteFilePath("junctionTool");
    QString mapFilePath = AppDirs::programDir().absoluteFilePath("junctions_map.txt");
    QString baseDirPath =  AppDirs::bundledModulesDir();
    QString shippedModulesPath = AppDirs::programDir().absoluteFilePath("Modules");
    
    junctionTool.setProcessChannelMode(QProcess::ForwardedChannels);
    
    junctionTool.setProgram(toolPath);
    junctionTool.setArguments({"-c", QDir::toNativeSeparators(mapFilePath), QDir::toNativeSeparators(baseDirPath), QDir::toNativeSeparators(shippedModulesPath)});

    Log::log() << "Starting junction tool..." << std::endl;
    junctionTool.start();

    if (!junctionTool.waitForStarted()) {
        Log::log() << "Failed to start junction_tool.exe!" << std::endl;
        return false;
    }

    // 120 seconds is more than enough time.
    if (!junctionTool.waitForFinished(120000)) {
        Log::log() << "junction_tool.exe timed out! Terminating..." << std::endl;
        junctionTool.kill();
        junctionTool.waitForFinished();
        return false;
    }

    if (junctionTool.exitStatus() == QProcess::CrashExit) {
        Log::log() << "junction_tool.exe crashed unexpectedly!" << std::endl;
        return false;
    }

    int exitCode = junctionTool.exitCode();
    if (exitCode != 0) {
        Log::log() << "junction_tool.exe returned error code: " << exitCode << std::endl;
        return false;
    }

	return DynamicRuntimeInfo::getInstance()->writeDynamicRuntimeInfoFile(); //write file so we only do this once
}

#endif


#define SEPARATE_PROCESS

void recursiveFileOpener(const QFileInfo & file, int & failures, int & total, const ParsedArguments& arguments, char * JASPName)
{
	//std::cout << "recursiveFileOpener in " << file.absoluteFilePath().toStdString() << std::endl;

	if(file.isDir())
	{
		//std::cout << "it is a directory and " << (file.exists() ? "exists" : "does not exist") << std::endl;

		QDir dir(file.absoluteFilePath());

		//std::cout << "QDir dir: " << dir.path().toStdString() << " has " << files.size() << " subfiles!" << std::endl;

		for(QFileInfo & subFile : dir.entryInfoList(QDir::Filter::NoDotAndDotDot | QDir::Files | QDir::Dirs))
			recursiveFileOpener(subFile, failures, total, arguments, JASPName);

	}
	else if(file.isFile())
	{
		//std::cout << "it is a file" << std::endl;

		if (Utils::getTypeFromFileName(file.absoluteFilePath().toStdString()) == Utils::FileType::jasp)
		{
			//std::cout << "it has the .jasp extension so we will start JASP" << std::endl;
#ifndef SEPARATE_PROCESS
			std::cout << "Found a JASP file (" << file.absoluteFilePath().toStdString() << ") going to start JASP" << std::endl;
#endif

			int result = 1;

			try{
#ifdef SEPARATE_PROCESS
				QProcess subJasp;
				subJasp.setProgram(JASPName);
				QStringList processArguments({tq(arguments.unitTestArg), file.absoluteFilePath()});

				if(arguments.save)
					processArguments << tq(arguments.saveArg);

				processArguments << tq(arguments.timeOutArg) + QString::number(arguments.timeOut);

				if(arguments.hideJASP)
					processArguments << tq(arguments.hideArg);

				std::cout << "Starting subJASP with args: " << fq(processArguments.join(' ')) << std::endl;
				subJasp.setArguments(processArguments);
				subJasp.start();

				subJasp.waitForFinished((arguments.timeOut * 60000) + 10000);

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

void recursiveSyncDataFile(const QFileInfo& fileInfo, std::vector<QFileInfo>& dataFiles)
{
	if (fileInfo.exists() && fileInfo.isDir())
	{
		QDir dir(fileInfo.absoluteFilePath());
		for(QFileInfo & subFile : dir.entryInfoList(QDir::Filter::NoDotAndDotDot | QDir::Files | QDir::Dirs))
		{
			if (subFile.isDir())
				recursiveSyncDataFile(subFile, dataFiles);
			else if (ParsedArguments::isDataFileType(subFile.fileName()))
				dataFiles.push_back(subFile);
		}
	}
}

int syncDataFiles(const ParsedArguments& arguments, char* jaspName)
{
	std::vector<QFileInfo> dataFiles = arguments.dataFiles;

	recursiveSyncDataFile(arguments.inputDataDir, dataFiles);

	if(dataFiles.empty())
	{
		std::cerr << "No data files to synchronize with" << (arguments.inputDataDir.exists() ? " were found in " + fq(arguments.inputDataDir.absoluteFilePath()) : std::string()) << "! Treating that as a failure to notify you of it." << std::endl;
		return 1;
	}

	int failures = 0;

	for (const QFileInfo& dataFile : dataFiles)
	{
		bool failed = false;
		try{
			QProcess subJasp;
			subJasp.setProgram(jaspName);
			QStringList subArguments({arguments.mainFilePath.absoluteFilePath(), dataFile.absoluteFilePath()});

			if(arguments.outputDir.exists())
				subArguments << tq(arguments.outputDirArg) << arguments.outputDir.absoluteFilePath();

			if (arguments.exportPdf)
				subArguments << tq(arguments.exportPdfArg);

			if (arguments.dontExportResult)
				subArguments << tq(arguments.dontExportResultArg);

			if (arguments.keepMissingColsWhenSyncing)
				subArguments << tq(arguments.keepMissingColsWhenSyncingArg);

			if(arguments.save)
				subArguments << tq(arguments.saveArg);

			if(arguments.hideJASP)
				subArguments << tq(arguments.hideArg);

			subArguments << tq(arguments.timeOutArg) + QString::number(arguments.timeOut);


			std::cout << "Starting subJASP with args: " << fq(subArguments.join(' ')) << std::endl;
			subJasp.setArguments(subArguments);
			subJasp.start();

			if (!subJasp.waitForStarted())
			{
				std::cerr << "subJASP for data file " << fq(dataFile.absoluteFilePath()) << " could not be started." << std::endl;
				failed = true;
			}
			else
			{
				if (!subJasp.waitForFinished((arguments.timeOut * 60000) + 10000))
				{
					std::cerr << "subJASP for data file " << fq(dataFile.absoluteFilePath()) << " did not finish in time; killing it." << std::endl;
					subJasp.kill();
					subJasp.waitForFinished(10000);
					failed = true;
				}
				else if (subJasp.exitStatus() != QProcess::NormalExit || subJasp.exitCode() != 0)
				{
					std::cerr << "subJASP for data file " << fq(dataFile.absoluteFilePath()) << " failed (exit code " << subJasp.exitCode() << ")." << std::endl;
					failed = true;
				}

				std::cerr << subJasp.readAllStandardError().toStdString() << std::endl;
			}
		}
		catch(...)
		{
			std::cerr << "An error occurred while processing data file " << fq(dataFile.absoluteFilePath()) << std::endl;
			failed = true;
		}

		if (failed)
			failures++;
	}

	if (failures > 0)
		std::cerr << failures << " out of " << dataFiles.size() << " data file(s) FAILED to process." << std::endl;

	return failures;
}

void qtMessageHandler(QtMsgType type, const QMessageLogContext &context, const QString &msg)
{
	const char *file	= context.file ? context.file : "";
	const char *function = context.function ? context.function : "";

	switch (type) {
	case QtWarningMsg:
		Log::log() << "Msg from Qt Warning: " << msg << " [" << file << ":" << context.line << ", " << function << "]" << std::endl;
		break;
	case QtCriticalMsg:
		Log::log() << "Msg from Qt Critical: " << msg << " [" << file << ":" << context.line << ", " << function << "]" << std::endl;
		break;
	case QtFatalMsg:
		Log::log() << "Msg from Qt Fatal: " << msg << " [" << file << ":" << context.line << ", " << function << "]" << std::endl;
		break;
	case QtDebugMsg:
	case QtInfoMsg:
		Log::log() << msg << std::endl;
		break;
	}
}

int main(int argc, char *argv[])
{
	qInstallMessageHandler(qtMessageHandler);

#ifdef _WIN32
	if(DynamicRuntimeInfo::getRuntimeEnvironment() == RuntimeEnvironment::MSIX) {
		QCoreApplication::setOrganizationName("JASP-Stats-MSIX");
		QCoreApplication::setOrganizationDomain("jasp-stats.org");
		QCoreApplication::setApplicationName("JASPDesktop");
	}
	else {
		QCoreApplication::setOrganizationName("JASP");
		QCoreApplication::setOrganizationDomain("jasp-stats.org");
		QCoreApplication::setApplicationName("JASP");
	}
	QSettings::setDefaultFormat(QSettings::IniFormat);

#else
	QCoreApplication::setOrganizationName("JASP");
	QCoreApplication::setOrganizationDomain("jasp-stats.org");
	QCoreApplication::setApplicationName("JASP");
#endif
	Dirs::setLocalAppdataDir(AppDirs::appData(false).toStdString());

	ParsedArguments arguments(argc, argv);

	if(arguments.safeGraphics)				Settings::setValue(Settings::SAFE_GRAPHICS_MODE, true);
	else									arguments.safeGraphics = Settings::value(Settings::SAFE_GRAPHICS_MODE).toBool();

	if(arguments.containerSettingForced)	Settings::setValue(Settings::ENGINE_SANDBOX,	arguments.container);
	else									arguments.container = Settings::value(Settings::ENGINE_SANDBOX).toBool();


	if(arguments.reportingDir.exists())		Settings::setValue(Settings::REPORT_SHOW, true);

	if(arguments.unitTestRecursive)
	{
		int		failures	= 0,
				total		= 0;

		recursiveFileOpener(arguments.mainFilePath, failures, total, arguments, argv[0]);

		if(total == 0)
		{
			std::cerr << "Couldn't find any jasp-files in specified directory " << arguments.mainFilePath.absoluteFilePath() << ", it is be treated as a failure to notify you of this!" << std::endl;
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

	if (arguments.syncDataFileRecursive)
	{
		int failures = syncDataFiles(arguments, argv[0]);
		exit(failures > 0 ? 1 : 0);
	}


	//Now, to allow us to add some arguments we store the ones we got in a vector
	std::vector<std::string> args(argv, argv + argc);

	if(arguments.safeGraphics)
	{
		std::cout << "Setting special options for software rendering (aka safe graphics)." << std::endl;
		QCoreApplication::setAttribute(Qt::AA_UseSoftwareOpenGL);
		args.push_back("--disable-gpu");
		char dst[] = "LIBGL_ALWAYS_SOFTWARE=1";
		putenv(dst);
	}

	if(arguments.hideJASP)
	{
		args.push_back("-platform");
		args.push_back("minimal");
	}

	PlotSchemeHandler::createUrlScheme(); //Needs to be done *before* creating PlotSchemeHandler instance and also before QApplication is instantiated
	ImgSchemeHandler::createUrlScheme();
	PersonaSchemeHandler::createUrlScheme();

	QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts);
	QCoreApplication::setAttribute(Qt::AA_SynthesizeTouchForUnhandledMouseEvents, false); //To avoid weird splitterbehaviour with QML and a touchscreen

#ifdef linux
	QmlUtils::configureQMLCacheDir();
#endif


	QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == . in at least R? Anyway, this has been here forever, ill just leave it.

	char dsteng[] = "LC_ALL=en_US.UTF-8"; // See this issue about encoding problems in the results: https://github.com/jasp-stats/jasp-test-release/issues/3099 and https://github.com/jasp-stats/jasp-issues/issues/3867 for xlsx importing
	putenv(dsteng);

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
	ImgSchemeHandler  imgSchemeHandler;
	PersonaSchemeHandler personaSchemeHandler;

	personaSchemeHandler.setPersonasDir(AppDirs::appData() + "/personas");
	personaSchemeHandler.setResourcesPersonasDir(QString::fromStdString(Dirs::resourcesDir()) + "/PersonaImages");

#ifdef _WIN32
	auto runtimeEnv = DynamicRuntimeInfo::getInstance()->getRuntimeEnvironmentAsString();
	Log::log() << "Runtime Environment: " << runtimeEnv << std::endl;

	// Since we introduced renv to JASP, we need to recreate the junctions from Modules -> renv-cache on first run. Because windows does not support proper symlinks on user perms
	// For this JASP has the --junctions argument, and is run on first execution of a specific jasp version on a system.
	Log::log() << "Checking if we need to recreate junctions or not" << std::endl;
	if(!DynamicRuntimeInfo::getInstance()->bundledModulesInitialized())
	{
		Log::log() << "We need to recreate junctions!" << std::endl;

		QMessageBox *msgBox = MessageForwarder::getInfoBox("Creating Junctions, one moment please", "Creating Junctions, one moment please");
		msgBox->show();

		if(!createJunctions())
		{
			std::cerr << "Modules folder missing and couldn't be created!\nContact the JASP team for support." << std::endl;
			exit(254);
		}
		msgBox->close();
	}
#endif
	a.init(arguments);

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
