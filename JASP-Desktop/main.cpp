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

const std::string	jaspExtension	= ".jasp",
					unitTestArg		= "--unitTest",
					saveArg			= "--save",
					timeOutArg		= "--timeout=";

void parseArguments(int argc, char *argv[], std::string & filePath, bool & unitTest, bool & dirTest, int & timeOut, bool & save, bool & logToFile)
{
	filePath	= "";
	unitTest	= false,
	dirTest		= false;
	save		= false;
	logToFile	= false;
	timeOut		= 10;

	bool letsExplainSomeThings = false;

	std::vector<std::string> args(argv + 1, argv + argc); // make the arguments a little less annoying to work with

	for(int arg = 0; arg < args.size(); arg++)
	{
		if(args[arg] == saveArg)
			save = true;
		else if(args[arg] == "--logToFile")
			logToFile = true;
		else if(args[arg] == "--unitTestRecursive")
		{
			if(arg >= args.size() - 1)
				letsExplainSomeThings = true;
			else
			{
				QDir folder(QString::fromStdString(args[arg + 1]));

				if(!folder.exists())
				{
					std::cerr << "Folder for unitTestRecursive " << args[arg + 1] << " does not exist!" << std::endl;
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

				QFileInfo testMe(QString::fromStdString(filePath));

				if(!testMe.exists())
				{
					std::cerr << "File for unitTest " << filePath << " does not exist!" << std::endl;
					letsExplainSomeThings = true;
				}

				bool	isJaspFile	= filePath.size() >= jaspExtension.size()  &&  filePath.substr(filePath.size() - jaspExtension.size()) == jaspExtension;

				if(!isJaspFile)
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
			const std::string remoteDebuggingPort = "--remote-debugging-port=";

			if(args[arg] == "-platform")
				arg++; // because it is always followed by the actual platform one wants to use (minimal for example)
			else if(!(args[arg] == "-qmljsdebugger" || (args[arg].size() > remoteDebuggingPort.size() && args[arg].substr(0, remoteDebuggingPort.size()) == remoteDebuggingPort))) //Just making sure it isnt something else that should be allowed.
			{
				//if it isn't anything else it must be a file to open right?

				QFileInfo openMe(QString::fromStdString(args[arg]));

				if(openMe.exists())
					filePath = args[arg];
				else
				{
					std::cerr << "File to open " << args[arg] << " does not exist!" << std::endl;
					letsExplainSomeThings = true;
				}
			}
		}
	}

	if(letsExplainSomeThings)
	{
		std::cout	<< "JASP can be started without arguments, or the following: { filename | --unitTest filename | --unitTestRecursive folder | --save | --timeOut=10 | --logToFile } \n"
					<< "If a filename is supplied JASP will try to load it. \nIf --unitTest is specified JASP will refresh all analyses in \"filename\" (which must be a JASP file) and see if the output remains the same and will then exit with an errorcode indicating succes or failure.\n"
					<< "If --unitTestRecursive is specified JASP will go through specified \"folder\" and perform a --unitTest on each JASP file. After it has done this it will exit with an errorcode indication succes or failure.\n"
					<< "For both testing arguments there is the optional --save argument, which specifies that JASP should save the file after refreshing it.\n"
					<< "For both testing arguments there is the optional --timeout argument, which specifies how many minutes JASP will wait for the analyses-refresh to take. Default is 10 minutes.\n"
					<< "If --logToFile is specified then JASP will try it's utmost to write logging to a file, this might come in handy if you want to figure out why JASP does not start in case of a bug.\n"
					<< std::flush;

		exit(1);
	}
}

#define SEPARATE_PROCESS

void recursiveFileOpener(QFileInfo file, int & failures, int & total, int & timeOut, int argc, char *argv[], bool save)
{
	const QString jaspExtension(".jasp");

	//std::cout << "recursiveFileOpener in " << file.absoluteFilePath().toStdString() << std::endl;

	if(file.isDir())
	{
		//std::cout << "it is a directory and " << (file.exists() ? "exists" : "does not exist") << std::endl;

		QDir dir(file.absoluteFilePath());

		//std::cout << "QDir dir: " << dir.path().toStdString() << " has " << files.size() << " subfiles!" << std::endl;

		for(QFileInfo subFile : dir.entryInfoList(QDir::Filter::NoDotAndDotDot | QDir::Files | QDir::Dirs))
			recursiveFileOpener(subFile, failures, total, timeOut, argc, argv, save);

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
	QCoreApplication::setAttribute(Qt::AA_UseOpenGLES); //might fix weirdlooking QML on Windows when using not-so-goo drivers? ( https://github.com/jasp-stats/jasp-desktop/issues/2669 )
#endif

	std::string filePath;
	bool		unitTest,
				dirTest,
				save,
				logToFile;
	int			timeOut;

	parseArguments(argc, argv, filePath, unitTest, dirTest, timeOut, save, logToFile);

	QString filePathQ(QString::fromStdString(filePath));

	if(!dirTest)
		//try
		{
			QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts);
			QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
			QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts);
			QCoreApplication::setAttribute(Qt::AA_SynthesizeTouchForUnhandledMouseEvents, false); //To avoid weird splitterbehaviour with QML and a touchscreen
			QCoreApplication::setOrganizationName("JASP");
			QCoreApplication::setOrganizationDomain("jasp-stats.org");
			QCoreApplication::setApplicationName("JASP");

			QLocale::setDefault(QLocale(QLocale::English)); // make decimal points == .
#ifdef _WIN32
			QQuickWindow::setTextRenderType(QQuickWindow::NativeTextRendering); //Doesn't improve it on anything 'cept windows
#endif
			JASPTIMER_START("JASP");
			Application a(argc, argv, filePathQ, unitTest, timeOut, save, logToFile);
			int exitCode = a.exec();
			JASPTIMER_STOP("JASP");
			JASPTIMER_PRINTALL();
			return exitCode;
		}
	//	catch(std::exception & e) { std::cerr << "Expection ocurred: " << e.what() << std::endl;  return -1; }
	//	catch(...) { return -1; }
	else
	{
		int		failures	= 0,
				total		= 0;

		recursiveFileOpener(QFileInfo(filePathQ), failures, total, timeOut, argc, argv, save);

		if(total == 0)
		{
			std::cerr << "Couldn't find any jasp-files in specified directory " << filePath << ", it will be treated as a failure to notify you of this!" << std::endl;
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
