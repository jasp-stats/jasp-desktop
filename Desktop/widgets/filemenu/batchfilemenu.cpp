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

#include <QClipboard>
#include <QCoreApplication>
#include <QDir>
#include <QGuiApplication>
#include <QFileInfo>

#include "filemenu.h"
#include "log.h"
#include "parsedarguments.h"
#include "utilenums.h"
#include "data/datasetpackage.h"
#include "utilities/messageforwarder.h"
#include "utilities/qutils.h"

///Keep this in the same order as the values of the exporttype-dropdown in Batch.qml
const std::vector<ExportType> BatchFileMenu::exportTypes = { ExportType::Html, ExportType::Pdf, ExportType::Jasp, ExportType::No };

#ifdef __APPLE__
const QString BatchFileMenu::noFocusStealingEnvVar = "QT_MAC_DISABLE_FOREGROUND_APPLICATION_TRANSFORM";
#endif

BatchFileMenu::BatchFileMenu(FileMenu * parent) : FileMenuObject{parent}, _inputs(new BatchInputsModel(this))
{
	//The commandline (and thus whether we can run it) is made up out of all of the settings below
	connect(this, &BatchFileMenu::jaspFileChanged,						this, &BatchFileMenu::commandLineChanged);
	connect(_inputs, &BatchInputsModel::selectionChanged,				this, &BatchFileMenu::commandLineChanged);
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

	//The same goes for the data files in the folders
	_inputs->refresh();
}

QString BatchFileMenu::browseStartFolder() const
{
	//Where the data file or folder added last is, so whatever is next to it is right there
	if(!_inputs->lastAddedPath().isEmpty())
	{
		QString whereLastAddedIs = QFileInfo(_inputs->lastAddedPath()).absolutePath();

		if(QFileInfo::exists(whereLastAddedIs))
			return whereLastAddedIs;
	}

	return jaspFile().isEmpty() ? QDir::homePath() : QFileInfo(jaspFile()).absolutePath();
}

QStringList BatchFileMenu::dataFileExtensions()
{
	QStringList extensions;

	//The same loop Utils::getTypeFromFileName uses to recognize a file by its extension
	for(int i = 0; i < int(Utils::FileType::empty); i++)
		if(ParsedArguments::isDataFileType(Utils::FileType(i)))
			extensions.push_back(tq(FileTypeBaseToString(Utils::FileType(i))));

	return extensions;
}

void BatchFileMenu::browseDataFiles()
{
	QStringList patterns;

	for(const QString & extension : dataFileExtensions())
		patterns.push_back("*." + extension);

	QString		chosen			= MessageForwarder::browseOpenFile(tr("Select the data files to run the JASP file against"), browseStartFolder(), tr("Data Files") + " (" + patterns.join(' ') + ")", true);
	QStringList	notDataFiles	= _inputs->addDataFiles(chosen.split(';', Qt::SkipEmptyParts)); //browseOpenFile joins several files with ';'

	if(!notDataFiles.isEmpty())
		MessageForwarder::showWarning(tr("Not a data file"), tr("JASP cannot import these files, so they were not added: %1").arg(notDataFiles.join(", ")));
}

void BatchFileMenu::browseDataFolder()
{
	QString chosen = MessageForwarder::browseOpenFolder(tr("Select a folder holding data files to run the JASP file against"), browseStartFolder());

	if(!chosen.isEmpty())
		_inputs->addFolder(chosen);
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

	args << _inputs->arguments();

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

void BatchFileMenu::copyCommandLine() const
{
	QGuiApplication::clipboard()->setText(commandLine());
}

QString BatchFileMenu::problem() const
{
	if(jaspFile().isEmpty())
		return tr("A batch runs the analyses of a JASP file, so open (or save) one first.");

	if(_inputs->isEmpty())
		return tr("Add the data files, or a folder holding them, to run the JASP file against.");

	if(!_inputs->missingPath().isEmpty())
		return tr("%1 does not exist anymore, remove it from the data files.").arg(_inputs->missingPath());

	if(_inputs->selectedCount() == 0)
		return tr("Select at least one of the data files to run the JASP file against.");

	return "";
}

void BatchFileMenu::runBatch()
{
	if(running())
		return;

	//What is in the folders right now is what gets run, so the list should show exactly that
	_inputs->refresh();

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
