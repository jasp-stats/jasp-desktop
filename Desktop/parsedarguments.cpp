//
// Copyright (C) 2013-2025 University of Amsterdam
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

#include "parsedarguments.h"
#include <iostream>
#include <QFileInfo>
#include <QDir>
#include "utilenums.h"
#include "qutils.h"

const std::string
	ParsedArguments::unitTestArg			= "--unitTest",
	ParsedArguments::saveArg				= "--save",
	ParsedArguments::timeOutArg				= "--timeOut=",
	ParsedArguments::helpArg				= "--help",
	ParsedArguments::helpShortArg			= "-h",
	ParsedArguments::logToFileArg			= "--logToFile",
	ParsedArguments::hideArg				= "--hide",
	ParsedArguments::safeGraphicsArg		= "--safeGraphics",
	ParsedArguments::newDataArg				= "--newData",
	ParsedArguments::sandboxArg				= "--sandbox",
	ParsedArguments::noSandboxArg			= "--noSandbox",
	ParsedArguments::unitTestRecursiveArg	= "--unitTestRecursive",
	ParsedArguments::reportArg				= "--report",
	ParsedArguments::inputDataDirArg		= "--inputDataDir",
	ParsedArguments::outputDirArg			= "--outputDir",
	ParsedArguments::exportPdfArg			= "--exportPdf",
	ParsedArguments::keepJASPOpenArg		= "--keepJASPOpen",
	ParsedArguments::dontExportResultArg	= "--dontExportResult",
	ParsedArguments::keepMissingColsWhenSyncingArg = "--keepMissingColsWhenSyncing",
	ParsedArguments::platformQtArg			= "-platform",
	ParsedArguments::remoteDebuggingPortArg	= "--remote-debugging-port=",
	ParsedArguments::webEngineArgs			= "--webEngineArgs", // This is apparently necessary to use in front of --remote-debugging-port nowadays, see: https://doc.qt.io/qt-6/qtwebengine-debugging.html
	ParsedArguments::qmlJsDebugArg			= "-qmljsdebugger",
	ParsedArguments::dashing				= "--",
	ParsedArguments::psnArg					= "-psn";


ParsedArguments::ParsedArguments(int argc, char *argv[])
{
	bool letsExplainSomeThings = false;

	std::vector<std::string> args(argv + 1, argv + argc); // make the arguments a little less annoying to work with

	for(int argNr = 0; argNr < args.size() && !letsExplainSomeThings; argNr++)
	{
		std::string arg = args[argNr];

		if(arg == saveArg)										save						= true;
		else if(arg == helpArg || arg == helpShortArg)			letsExplainSomeThings		= true;
		else if(arg == logToFileArg)							logToFile					= true;
		else if(arg == hideArg)									hideJASP					= true;
		else if(arg == safeGraphicsArg)							safeGraphics				= true;
		else if(arg == newDataArg)								newData						= true;
#ifdef PRO
		else if(arg == exportPdfArg)							exportPdf					= true;
		else if(arg == keepJASPOpenArg)							keepJASPOpenAfterExporting	= true;
		else if(arg == dontExportResultArg)						dontExportResult			= true;
		else if(arg == keepMissingColsWhenSyncingArg)			keepMissingColsWhenSyncing	= true;
#endif
#ifdef _WIN32
		else if(arg == sandboxArg)			{				containerSettingForced	= true;		container = true; }
		else if(arg == noSandboxArg)		{				containerSettingForced	= true;		container = false; }
#endif
		else if(arg == unitTestRecursiveArg)
		{
			argNr++;
			if (checkFolder(args, argNr, mainFilePath))
				unitTestRecursive = true;
			else
				letsExplainSomeThings = true;
		}
		else if(arg == unitTestArg)
		{
			argNr++;
			if (checkFile(args, argNr, mainFilePath, true, true))
				unitTest = true;
			else
				letsExplainSomeThings = true;
		}
		else if(arg == reportArg)
		{
			argNr++;
			if (!checkFolder(args, argNr, reportingDir, true))
				letsExplainSomeThings = true;
		}
		else if(arg.size() > timeOutArg.size() && arg.substr(0, timeOutArg.size()) == timeOutArg)
		{
			std::string time			= arg.substr(timeOutArg.size());
			size_t		convertedChars	= 0;
			int			convertedTime	= 0;
			try								{ convertedTime = std::stoi(time, &convertedChars); }
			catch(std::invalid_argument &)	{}
			catch(std::out_of_range &)		{}

			if(convertedChars > 0)
				timeOut = convertedTime;
		}
#ifdef PRO
		else if (arg == inputDataDirArg)
		{
			argNr++;
			if (checkFolder(args, argNr, inputDataDir))
				syncDataFileRecursive = true;
			else
				letsExplainSomeThings = true;

		}
		else if (arg == outputDirArg)
		{
			argNr++;
			if (!checkFolder(args, argNr, outputDir))
				letsExplainSomeThings = true;
		}
#endif
		else
		{
			QString qarg = tq(arg);
			if(arg == platformQtArg)
				argNr++; // because it is always followed by the actual platform one wants to use (minimal for example)
			else if(qarg.startsWith(tq(webEngineArgs)))
				foundWebEngineArgs = true;
			else if(qarg.startsWith(tq(remoteDebuggingPortArg)) && !foundWebEngineArgs)
			{
				std::cerr << "If you want to use a remote debugging port enter the following: '--webEngineArgs --remote-debugging-port=12345' otherwise webengine will ignore it." << std::endl;
				exit(2);
			}
			else if(!(qarg.startsWith(tq(remoteDebuggingPortArg)) || qarg.startsWith(tq(qmlJsDebugArg)))) //Just making sure it isnt something else that should be allowed.
			{
				if(qarg.startsWith(tq(dashing)))
				{
					//So, it looks like an option, but not one we recognize, maybe it is meant for qt/chromium
					std::cout << "Argument '" << arg << "' was not recognized by JASP, but it might be recognized by one of it's components in Qt (such as chromium), it will be passed on. If you really expected JASP to do something with it check '--help' again." << std::endl;
				}
#ifdef __APPLE__
				else if(qarg.startsWith(tq(psnArg))) // https://github.com/jasp-stats/jasp-test-release/issues/1945
				{
					//this is one of those arguments to ignore...
					std::cout << "Ignoring " << arg << std::endl;
				}
#endif
				else
				{
					//if it isn't anything else it must be a file to open right?
					if (!mainFilePath.exists())
					{
						// The argument might be a database-connection json (as returned by DatabaseConnectionInfo)
						// rather than a file, so try that first before treating it as a (missing) file to open.
						Json::Reader	jsonReader;
						Json::Value		parsedJson;

						if (jsonReader.parse(arg, parsedJson) && parsedJson.isObject())
							dbJson = parsedJson;
						else if (checkFile(args, argNr, mainFilePath))
						{
							mainFileIsJaspFile = Utils::getTypeFromFileName(fq(mainFilePath.fileName())) == Utils::FileType::jasp;
							mainFileIsOnline = mainFilePath.filePath().startsWith("http:") ||  mainFilePath.filePath().startsWith("https:");
						}
						else
							letsExplainSomeThings = true;
					}
#ifdef PRO
					else if (mainFileIsJaspFile)
					{
						QFileInfo file;
						if (checkFile(args, argNr, file, true, false))
						{
							dataFiles.push_back(file);
							if (dataFiles.size() > 1)
								syncDataFileRecursive = true;
						}
						else
							letsExplainSomeThings = true;
					}
#endif
					else
					{
						//Check whether it can be parsed as a json and if so assume it is a database connection json as returned by DatabaseConnectionInfo

						Json::Reader jsonReader;

						if(!jsonReader.parse(arg, dbJson))
						{
							std::cerr << "File to open " << arg << " does not exist (and also is not a (database) json)!" << std::endl;
							letsExplainSomeThings = true;
						}
					}
				}
			}
		}
	}

	if(!mainFilePath.exists() && reportingDir.exists())
	{
		std::cerr << "If you want JASP to run in reportingmode you should also give it a jaspfile to run off." << std::endl;
		letsExplainSomeThings = true;
	}

	if(letsExplainSomeThings)
	{
		std::cerr	<< "JASP can be started without arguments, or the following: ";
#ifdef PRO

		std::cerr	<< "{ --help | -h | filename (filedata1 filedata2 ...) | --unitTest filename | --unitTestRecursive folder | --save | --timeOut=10 | --logToFile | --hide | --outputDir | --exportPdf | --inputDataDir | --dontExportResult | --keepMissingColsWhenSyncing | --keepJASPOpen } \n";
#else
		std::cerr	<< "{ --help | -h | filename | --unitTest filename | --unitTestRecursive folder | --save | --timeOut=10 | --logToFile | --hide } \n";
#endif
		std::cerr	<< "If a filename is supplied JASP will try to load it. \n";
#ifdef PRO
		std::cerr	<< "If a filedata or several filedata are supplied, then JASP will synchronize the JASP file with the new data. In this case it will per default export the results in HTML format (in PDF format if --exportPdf is set)\n"
					<< "If --outputDir is specified, then the results are exported in this folder, if not it wlll be exported in the same folder as the data file.\n"
					<< "if --inputDataDir is specified, all the data files in this folder (and subfolders) will be used for the synchronization.\n"
					<< "Per default after synchronizing with a data file, it will export the result, except if --dontExportResult is specified.\n"
					<< "It will also remove columns afetr synchronizing if the column did not exist, except if --keepMissingColsWhenSyncing is specified: it will then keep an empty column.\n"
					<< "Also per default JASP will be automatically closed after synchronizing (and exporting the result), except if only one data file is used and --keepJASPOpen is specified.\n";
#endif
		std::cerr	<< "\n"
					<< "If --unitTest is specified JASP will refresh all analyses in \"filename\" (which must be a JASP file) and see if the output remains the same and will then exit with an errorcode indicating succes or failure.\n"
					<< "If --unitTestRecursive is specified JASP will go through specified \"folder\" and perform a --unitTest on each JASP file. After it has done this it will exit with an errorcode indication succes or failure.\n"
					<< "For both testing arguments there is the optional --save argument, which specifies that JASP should save the file after refreshing it.\n"
					<< "For both testing arguments there is the optional --timeout argument, which specifies how many minutes JASP will wait for the analyses-refresh to take. Default is 10 minutes.\n"
					<< "If --logToFile is specified then JASP will try it's utmost to write logging to a file, this might come in handy if you want to figure out why JASP does not start in case of a bug.\n"
					<< "If --hide is specified then JASP will not be shown during recursive testing or reporting.\n"
					<< "If --safeGraphics is specified then JASP will be started with software rendering enabled, this will be saved to your settings.\n"
					<< "If --report is specified then JASP will be started in reporting mode, which requires a path to where you would like to store the results. This is usually used in conjunction with a service/daemon and in that case it might make sense to also pass --hide. Don't forget to also pass a jasp filename otherwise it won't have anything to run...\n"
#ifdef _WIN32
					<< "In case one really wants the engines to be sandboxed specify --sandbox, otherwise use --noSandbox."
#endif
					<< "This text will be shown when either --help or -h is specified or something else that JASP does not understand is given as argument.\n"
					<< std::flush;

		exit(1);
	}
}

bool ParsedArguments::checkFile(const std::vector<std::string> & args, int arg, QFileInfo & filePath, bool checkFileType, bool checkJASPType)
{
	if (arg >= args.size())
		return false;

	QString path = tq(args[arg]);
	filePath = QFileInfo(path);

	if (!path.startsWith("https:") && !path.startsWith("http:") && !filePath.exists())
	{
		std::cerr << "File " << filePath.absoluteFilePath().toStdString() << " does not exist!" << std::endl;
		return false;
	}

	Utils::FileType fileType = Utils::getTypeFromFileName(args[arg]);

	//For online files the type often can't be derived from the URL (no extension); the real type is
	//determined after downloading, so don't reject an unknown type here.
	bool online = path.startsWith("https:") || path.startsWith("http:");

	if (!online && (fileType == Utils::FileType::unknown || fileType == Utils::FileType::empty))
	{
		std::cerr << "Unknown file type: " << path << std::endl;
		return false;
	}

	if (checkFileType)
	{
		if (checkJASPType)
		{
			if (fileType != Utils::FileType::jasp)
			{
				std::cerr << "File " << path << " is not a JASP file!" << std::endl;
				return false;
			}
		}
		else if (!isDataFileType(fileType))
		{
			std::cerr << "File " << path << " is not a data file!" << std::endl;
			return false;
		}
	}

	return true;
}

bool ParsedArguments::checkFolder(const std::vector<std::string> & args, int arg, QFileInfo &folderPath, bool createIt)
{
	if(arg >= args.size())
		return false;

	QString path = tq(args[arg]);
	folderPath = QFileInfo(path);

	if (createIt && !folderPath.exists())
	{
		QDir folder(path);
		folder.mkpath(".");
	}

	if(!folderPath.exists())
	{
		std::cerr << "Folder for " << folderPath.absoluteFilePath().toStdString() << " does not exist!" << (createIt ? " and cannot be created" : "") << std::endl;
		return false;
	}

	return true;
}

bool ParsedArguments::isDataFileType(Utils::FileType type)
{
	switch(type)
	{
	case Utils::FileType::empty:
	case Utils::FileType::unknown:
	case Utils::FileType::jasp:
	case Utils::FileType::html:
	case Utils::FileType::pdf:
	case Utils::FileType::database:
		return false;
	default:
		return true;
	}
}

bool ParsedArguments::isDataFileType(const QString & filePath)
{
	return isDataFileType(Utils::getTypeFromFileName(fq(filePath)));
}

