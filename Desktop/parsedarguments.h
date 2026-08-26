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

#ifndef PARSEDARGUMENTS_H
#define PARSEDARGUMENTS_H

#include <json/json.h>
#include "utils.h"
#include <QString>
#include <QFileInfo>
#include <QDir>

struct ParsedArguments
{
public:

	ParsedArguments(int argc, char *argv[]);

	bool				mainFileIsJaspFile			= false,
						mainFileIsOnline			= false,
						newData						= false,
						unitTest					= false,
						unitTestRecursive			= false,
						syncDataFileRecursive		= false,
						save						= false,
						logToFile					= false,
						hideJASP					= false,
						safeGraphics				= false,
						containerSettingForced		= false,
						container					= false,
						exportPdf					= false,
						keepJASPOpenAfterExporting	= false,
						dontExportResult			= false,
						foundWebEngineArgs			= false,
						keepMissingColsWhenSyncing	= false;
	int					timeOut						= 10;
	Json::Value			dbJson;
	QFileInfo			mainFilePath,
						reportingDir,
						inputDataDir,
						outputDir;
	std::vector<QFileInfo>	dataFiles;

	static const std::string
		unitTestArg,
		saveArg,
		timeOutArg,
		helpArg,
		helpShortArg,
		logToFileArg,
		hideArg,
		safeGraphicsArg,
		newDataArg,
		sandboxArg,
		noSandboxArg,
		unitTestRecursiveArg,
		reportArg,
		inputDataDirArg,
		outputDirArg,
		exportPdfArg,
		keepJASPOpenArg,
		dontExportResultArg,
		platformQtArg,
		remoteDebuggingPortArg,
		webEngineArgs,
		keepMissingColsWhenSyncingArg,
		qmlJsDebugArg,
		dashing,
		psnArg;

	static bool		isDataFileType(			Utils::FileType type);
	static bool		isDataFileType(			const QString &path);


private:
	bool checkFile(const std::vector<std::string> & args, int arg, QFileInfo & filePath, bool checkFileType = false, bool checkJASPType = false);
	bool checkFolder(const std::vector<std::string> & args, int arg, QFileInfo & folderPath, bool createIt = false);



};

#endif // PARSEDARGUMENTS_H
