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

#include "batchfilemenu.h"

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>

#include "filemenu.h"
#include "log.h"
#include "parsedarguments.h"
#include "data/datasetpackage.h"
#include "utilities/messageforwarder.h"
#include "utilities/qutils.h"

///Keep this in the same order as the values of the exporttype-dropdown in Batch.qml
const std::vector<ExportType> BatchFileMenu::exportTypes = { ExportType::Html, ExportType::Pdf, ExportType::Jasp, ExportType::No };

#ifdef __APPLE__
const QString BatchFileMenu::noFocusStealingEnvVar = "QT_MAC_DISABLE_FOREGROUND_APPLICATION_TRANSFORM";
#endif

BatchFileMenu::BatchFileMenu(FileMenu * parent) : FileMenuObject{parent}
{
	//The commandline (and thus whether we can run it) is made up out of all of the settings below
	connect(this, &BatchFileMenu::jaspFileChanged,						this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::useInputFolderChanged,					this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::inputFileChanged,						this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::inputFolderChanged,					this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::outputFolderChanged,					this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::exportTypeIndexChanged,				this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::keepJASPOpenChanged,					this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::keepMissingColsWhenSyncingChanged,	this, &BatchFileMenu::commandLineChanged);
	connect(this, &BatchFileMenu::runningChanged,						this, &BatchFileMenu::commandLineChanged);
}

QString BatchFileMenu::jaspFile() const
{
	return _filemenu->getCurrentFileType() == Utils::FileType::jasp ? _filemenu->getCurrentFilePath() : "";
}

bool BatchFileMenu::jaspFileModified() const
{
	return !jaspFile().isEmpty() && DataSetPackage::pkg()->isModified();
}

int BatchFileMenu::exportTypeIndex() const
{
	for(size_t i=0; i<exportTypes.size(); i++)
		if(exportTypes[i] == _exportType)
			return int(i);

	return 0;
}

void BatchFileMenu::setUseInputFolder(bool useInputFolder)
{
	if(_useInputFolder == useInputFolder)
		return;

	_useInputFolder = useInputFolder;
	emit useInputFolderChanged();
}

void BatchFileMenu::setInputFile(const QString & inputFile)
{
	if(_inputFile == inputFile)
		return;

	_inputFile = inputFile;
	emit inputFileChanged();
}

void BatchFileMenu::setInputFolder(const QString & inputFolder)
{
	if(_inputFolder == inputFolder)
		return;

	_inputFolder = inputFolder;
	emit inputFolderChanged();
}

void BatchFileMenu::setOutputFolder(const QString & outputFolder)
{
	if(_outputFolder == outputFolder)
		return;

	_outputFolder = outputFolder;
	emit outputFolderChanged();
}

void BatchFileMenu::setExportTypeIndex(int exportTypeIndex)
{
	if(exportTypeIndex < 0 || exportTypeIndex >= int(exportTypes.size()) || exportTypes[exportTypeIndex] == _exportType)
		return;

	_exportType = exportTypes[exportTypeIndex];
	emit exportTypeIndexChanged();
}

void BatchFileMenu::setKeepJASPOpen(bool keepJASPOpen)
{
	if(_keepJASPOpen == keepJASPOpen)
		return;

	_keepJASPOpen = keepJASPOpen;
	emit keepJASPOpenChanged();
}

void BatchFileMenu::setKeepMissingColsWhenSyncing(bool keepMissingColsWhenSyncing)
{
	if(_keepMissingColsWhenSyncing == keepMissingColsWhenSyncing)
		return;

	_keepMissingColsWhenSyncing = keepMissingColsWhenSyncing;
	emit keepMissingColsWhenSyncingChanged();
}

void BatchFileMenu::refresh()
{
	//The jasp-file (and whether it was modified since it was saved) is taken from the loaded workspace,
	//so it can have changed without us hearing about it.
	emit jaspFileChanged();
}

QString BatchFileMenu::browseStartFolder() const
{
	QString chosen = _useInputFolder ? _inputFolder : QFileInfo(_inputFile).absolutePath();

	if(!chosen.isEmpty() && QFileInfo::exists(chosen))
		return chosen;

	return jaspFile().isEmpty() ? QDir::homePath() : QFileInfo(jaspFile()).absolutePath();
}

void BatchFileMenu::browseInputFile()
{
	QString filter	= tr("Data Files") + " (*.csv *.txt *.tsv *.sav *.zsav *.por *.xpt *.ods *.xls *.xlsx *.dta *.sas7bdat *.sas7bcat *.rdata *.rds *.mwx *.mpx)",
			chosen	= MessageForwarder::browseOpenFile(tr("Select a data file"), browseStartFolder(), filter);

	if(!chosen.isEmpty())
		setInputFile(chosen);
}

void BatchFileMenu::browseInputFolder()
{
	QString chosen = MessageForwarder::browseOpenFolder(tr("Select a folder with data files"), browseStartFolder());

	if(!chosen.isEmpty())
		setInputFolder(chosen);
}

void BatchFileMenu::browseOutputFolder()
{
	QString chosen = MessageForwarder::browseOpenFolder(tr("Select a folder for the results"), _outputFolder.isEmpty() ? browseStartFolder() : _outputFolder);

	if(!chosen.isEmpty())
		setOutputFolder(chosen);
}

QStringList BatchFileMenu::arguments() const
{
	QStringList args = { jaspFile() };

	if(_useInputFolder)		args << tq(ParsedArguments::inputDataDirArg) << _inputFolder;
	else					args << _inputFile;

	if(!_outputFolder.isEmpty())
		args << tq(ParsedArguments::outputDirArg) << _outputFolder;

	args << tq(ParsedArguments::exportTypeArg) + ExportTypeToQString(_exportType);

	if(_keepMissingColsWhenSyncing)
		args << tq(ParsedArguments::keepMissingColsWhenSyncingArg);

	if(_keepJASPOpen)
		args << tq(ParsedArguments::keepJASPOpenArg);

	return args;
}

QString BatchFileMenu::quoteIfNeeded(const QString & argument)
{
	return argument.contains(' ') || argument.contains('\t') ? "\"" + argument + "\"" : argument;
}

QString BatchFileMenu::commandLine() const
{
	QStringList parts;

#ifdef __APPLE__
	if(!_keepJASPOpen)
		parts << noFocusStealingEnvVar + "=1";
#endif

	parts << quoteIfNeeded(QCoreApplication::applicationFilePath());

	for(const QString & argument : arguments())
		parts << quoteIfNeeded(argument);

	return parts.join(' ');
}

QString BatchFileMenu::problem() const
{
	if(jaspFile().isEmpty())
		return tr("A batch runs the analyses of a JASP file, so open (or save) one first.");

	if(!_useInputFolder && _inputFile.isEmpty())
		return tr("Select the data file to run the JASP file against.");

	if(_useInputFolder && _inputFolder.isEmpty())
		return tr("Select the folder holding the data files to run the JASP file against.");

	return "";
}

void BatchFileMenu::runBatch()
{
	if(running())
		return;

	if(!problem().isEmpty())
	{
		MessageForwarder::showWarning(tr("Cannot run batch"), problem());
		return;
	}

	_process = new QProcess(this);

	_process->setProgram(QCoreApplication::applicationFilePath());
	_process->setArguments(arguments());
	_process->setProcessChannelMode(QProcess::MergedChannels); //Keeps the order of the progress (stdout) and the failures (stderr) intact

#ifdef __APPLE__
	//A starting JASP makes itself the foreground application and takes the focus away from the JASP you are working in,
	//once per data file. This tells Qt's cocoa plugin not to do that, so a batch can run while you keep working.
	//The JASPs started per data file inherit this environment, so setting it here is enough for all of them.
	//Not done when JASP is supposed to stay open, because then you do want to be able to get at it.
	//(--hide is not an option here: it runs JASP on the minimal platform, on which the results view crashes.)
	if(!_keepJASPOpen)
	{
		QProcessEnvironment environment = QProcessEnvironment::systemEnvironment();
		environment.insert(noFocusStealingEnvVar, "1");
		_process->setProcessEnvironment(environment);
	}
#endif

	connect(_process, &QProcess::readyReadStandardOutput,	this, &BatchFileMenu::readProcessOutput		);
	connect(_process, &QProcess::finished,					this, &BatchFileMenu::processFinished		);
	connect(_process, &QProcess::errorOccurred,				this, &BatchFileMenu::processErrorOccurred	);

	Log::log() << "Starting batch with commandline: " << commandLine() << std::endl;

	appendOutput(commandLine() + "\n");

	_process->start();

	emit runningChanged();
}

void BatchFileMenu::stopBatch()
{
	if(!running())
		return;

	//The batch waits for each JASP it starts and has no eventloop to handle a polite request with, so it is kill or nothing
	appendOutput(tr("Stopping the batch...") + "\n");
	_process->kill();
}

void BatchFileMenu::readProcessOutput()
{
	if(_process)
		appendOutput(QString::fromLocal8Bit(_process->readAllStandardOutput()));
}

void BatchFileMenu::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	readProcessOutput();

	appendOutput(	exitStatus	!= QProcess::NormalExit	? tr("The batch was stopped.")									+ "\n\n"
				:	exitCode	== 0					? tr("The batch finished, all data files were processed.")		+ "\n\n"
													    : tr("The batch finished with error code %1, see above for which data file(s) failed.").arg(exitCode) + "\n\n");

	_process->deleteLater();
	_process = nullptr;

	emit runningChanged();
}

void BatchFileMenu::processErrorOccurred(QProcess::ProcessError error)
{
	if(error != QProcess::FailedToStart)
		return; //Anything else is followed by finished(), which does the cleaning up

	appendOutput(tr("JASP could not be started to run the batch: %1").arg(_process->errorString()) + "\n\n");

	_process->deleteLater();
	_process = nullptr;

	emit runningChanged();
}

void BatchFileMenu::appendOutput(const QString & text)
{
	if(text.isEmpty())
		return;

	const int maxOutputSize = 100000; //A batch over a big folder can produce a lot of output and there is no point in keeping all of it in memory (and in a textarea)

	_output += text;

	if(_output.size() > maxOutputSize)
		_output = tr("[ earlier output was dropped ]") + "\n" + _output.right(maxOutputSize);

	emit outputChanged();
}

void BatchFileMenu::clearOutput()
{
	if(_output.isEmpty())
		return;

	_output.clear();
	emit outputChanged();
}
