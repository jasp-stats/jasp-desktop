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

#include <QDebug>
#include <QDir>

#include "utilities/application.h"
#include <QQuickWindow>

void checkTimeOut(int argc, char *argv[], int index, int & timeOut)
{
	const int defaultTimeOut = 10;
	timeOut = defaultTimeOut;

	const std::string	timeOutIdent	= "--timeOut=";

	if(argc > index)
	{
		std::string timeOutArg = argv[index];

		if(timeOutArg.substr(0, timeOutIdent.size()) == timeOutIdent)
		{
			std::string time			= timeOutArg.substr(timeOutIdent.size());
			size_t		convertedChars	= 0;

			try								{ timeOut = std::stoi(time, &convertedChars); }
			catch(std::invalid_argument &)	{}
			catch(std::out_of_range &)		{}

			if(convertedChars == 0)
				timeOut = defaultTimeOut;
		}
	}
}

const std::string	jaspExtension	= ".jasp",
					unitTestArg		= "--unitTest",
					saveArg			= "--save";

void parseArguments(int argc, char *argv[], std::string & filePath, bool & unitTest, bool & dirTest, int & timeOut, bool & save)
{
	filePath	= "";
	unitTest	= false,
	dirTest		= false;
	save		= false;

	if (argc > 1)
	{
		std::string argFirst = argv[1];

		if (argFirst[0] != '-')
		{
					filePath	= argFirst;
			bool	isJaspFile	= filePath.size() >= jaspExtension.size()  &&  filePath.substr(filePath.size() - jaspExtension.size()) == jaspExtension;
					unitTest	= isJaspFile && argc > 2 && argv[2] == unitTestArg;
					save		= unitTest && argc > 3 && argv[3] == saveArg;

			checkTimeOut(argc, argv, save ? 4 : 3, timeOut);

		}
		else
		{
			if(argFirst == "--unitTestRecursive")
			{
				std::string argFolder = argv[2];

				QDir folder(QString::fromStdString(argFolder));

				if(!folder.exists())
				{
					std::cerr << "Folder for dir unittest " << argFolder << " does not exist!" << std::endl;
					exit(1);
				}

				dirTest		= true;
				filePath	= argFolder;
				save		= argc > 3 && argv[3] == saveArg;

				checkTimeOut(argc, argv, save ? 4 : 3, timeOut);
			}
			else if(argFirst.find("--remote-debugging-port=")	== std::string::npos &&
					argFirst.find("-qmljsdebugger")				== std::string::npos) //only other excepted argument
			{
				std::cout	<< "JASP can be started without arguments, or the following: filename {--unitTest {--save} {--timeOut=10}} | --unitTestRecursive folder {--save} {--timeOut=10}\n"
							<< "If a filename is supplied JASP will try to load it. If --unitTest is specified JASP will refresh all analyses in the JASP file and see if the output remains the same and will then exit with an errorcode indicating succes or failure.\n"
							<< "If --unitTestRecursive is specified JASP will go through specified \"folder\" and perform a --unitTest on each JASP file. After it has done this it will exit with an errorcode indication succes or failure.\n"
							<< "For both testing arguments there is the optional --save argument, which specifies that JASP should save the file after refreshing it.\n"
							<< "For both testing arguments there is the optional --timeout argument, which specifies how many minutes JASP will wait for the analyses-refresh to take. Default is 10 minutes."
							<< std::endl;
				exit(1);
			}
		}
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
				QStringList arguments({file.absoluteFilePath(), "--unitTest"});

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
				save;
	int			timeOut;

	parseArguments(argc, argv, filePath, unitTest, dirTest, timeOut, save);

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
			Application a(argc, argv, filePathQ, unitTest, timeOut, save);
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
