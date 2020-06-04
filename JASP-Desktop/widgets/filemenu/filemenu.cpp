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

#include "filemenu.h"

#include <QFileInfo>
#include "utilities/settings.h"
#include "gui/messageforwarder.h"
#include "log.h"
#include "data/datasetpackage.h"
#include "mainwindow.h"

FileMenu::FileMenu(QObject *parent) : QObject(parent)
{	
	_mainWindow				= dynamic_cast<MainWindow*>(parent);
	_recentFiles			= new RecentFiles(parent);
	_currentDataFile		= new CurrentDataFile(parent);
	_computer				= new Computer(parent);
	_OSF					= new OSF(parent);
	_dataLibrary			= new DataLibrary(parent);
	_actionButtons			= new ActionButtons(this);
	_resourceButtons		= new ResourceButtons(this);
	_resourceButtonsVisible	= new ResourceButtonsVisible(this, _resourceButtons);

	connect(_recentFiles,		&FileMenuObject::dataSetIORequest,			this, &FileMenu::dataSetIORequestHandler);
	connect(_currentDataFile,	&FileMenuObject::dataSetIORequest,			this, &FileMenu::dataSetIORequestHandler);
	connect(_computer,			&FileMenuObject::dataSetIORequest,			this, &FileMenu::dataSetIORequestHandler);
	connect(_OSF,				&FileMenuObject::dataSetIORequest,			this, &FileMenu::dataSetIORequestHandler);
	connect(_dataLibrary,		&FileMenuObject::dataSetIORequest,			this, &FileMenu::dataSetIORequestHandler);
	connect(&_watcher,			&QFileSystemWatcher::fileChanged,			this, &FileMenu::dataFileModifiedHandler);
	connect(_actionButtons,		&ActionButtons::buttonClicked,				this, &FileMenu::actionButtonClicked	);
	connect(_actionButtons,		&ActionButtons::selectedActionChanged,		this, &FileMenu::setFileoperation		);
	connect(_resourceButtons,	&ResourceButtons::selectedButtonChanged,	this, &FileMenu::resourceButtonClicked	);
	connect(_currentDataFile,	&CurrentDataFile::setCheckAutomaticSync,	_mainWindow, &MainWindow::setCheckAutomaticSync	);

	_actionButtons->setEnabled(ActionButtons::Open,				true);
	_actionButtons->setEnabled(ActionButtons::Save,				false);
	_actionButtons->setEnabled(ActionButtons::SaveAs,			false);
	_actionButtons->setEnabled(ActionButtons::ExportResults,	false);
	_actionButtons->setEnabled(ActionButtons::ExportData,		false);
	_actionButtons->setEnabled(ActionButtons::SyncData,			false);
	_actionButtons->setEnabled(ActionButtons::Close,			false);
	_actionButtons->setEnabled(ActionButtons::Preferences,		true);
	_actionButtons->setEnabled(ActionButtons::About,			true);

	setResourceButtonsVisibleFor(_fileoperation);
}

void FileMenu::setFileoperation(ActionButtons::FileOperation fo)
{
	if(_fileoperation == fo)
		return;

	_fileoperation = fo;
	emit fileoperationChanged();

	setResourceButtonsVisibleFor(fo);
}

void FileMenu::setResourceButtonsVisibleFor(ActionButtons::FileOperation fo)
{
	_resourceButtons->setOnlyTheseButtonsVisible(_actionButtons->resourceButtonsForButton(fo));
}

void FileMenu::setSaveMode(FileEvent::FileMode mode)
{
	_mode = mode;

	_computer->setMode(_mode);
	
	_OSF->setMode(_mode);
	_OSF->setCurrentFileName(getDefaultOutFileName());
}

void FileMenu::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_OSF->setOnlineDataManager(odm);
}

FileEvent *FileMenu::open(const QString &path)
{
	FileEvent *event = new FileEvent(this, FileEvent::FileOpen);
	event->setPath(path);
	dataSetIORequestHandler(event);

	return event;
}

FileEvent *FileMenu::save()
{
	FileEvent *event = nullptr;

	if (_currentFileType != Utils::FileType::jasp || _currentFileReadOnly)
	{
		event = _computer->browseSave();
		if (event->isCompleted())
			return event;
	}
	else
	{
		event = new FileEvent(this, FileEvent::FileSave);
		if (!event->setPath(_currentFilePath))
		{
			MessageForwarder::showWarning(tr("File Types"), event->getLastError());
			event->setComplete(false, tr("Failed to open file from OSF"));
			return event;
		}
	}

	dataSetIORequestHandler(event);

	return event;
}

void FileMenu::sync()
{
	QString path = _currentDataFile->getCurrentFilePath();

	if (path.isEmpty())
	{
		if(!MessageForwarder::showYesNo(tr("No associated data file"),
					tr("JASP has no associated data file to be synchronized with.\nDo you want to search for such a data file on your computer?\nNB: You can also set this data file via menu File/Sync Data.")))
			return;

		path =  MessageForwarder::browseOpenFile(tr("Find Data File"), "", tr("Data File").arg("*.csv *.txt *.tsv *.sav *.ods *.dta *.por *.sas7bdat *.sas7bcat *.xpt"));
	}

	_mainWindow->setCheckAutomaticSync(false);
	setSyncRequest(path);
	
}

void FileMenu::close()
{
	FileEvent *event = new FileEvent(this, FileEvent::FileClose);
	dataSetIORequestHandler(event);

	setSaveMode(FileEvent::FileOpen);
	_actionButtons->setSelectedAction(ActionButtons::FileOperation::Open);
}

void FileMenu::setCurrentDataFile(const QString &path)
{
	QString currentPath = _currentDataFile->getCurrentFilePath();
	if (!currentPath.isEmpty())
		_watcher.removePath(currentPath);

	bool setCurrentPath = true;
	if (!path.isEmpty())
	{
		if (checkSyncFileExists(path))
		{
			bool sync = Settings::value(Settings::DATA_AUTO_SYNCHRONIZATION).toBool();
			if (sync)
				_watcher.addPath(path);
		}
		else
			setCurrentPath = false;
	}

	if (setCurrentPath)
		_currentDataFile->setCurrentFilePath(path);
}

void FileMenu::setDataFileWatcher(bool watch)
{
	QString path = _currentDataFile->getCurrentFilePath();
	if (path.isEmpty())
		return;

	if (watch && !_currentDataFile->isOnlineFile(path))	_watcher.addPath(path);
	else												_watcher.removePath(path);
}


QString FileMenu::getDefaultOutFileName()
{
	QString path				= getCurrentFilePath(),
			DefaultOutFileName	= "";

	if (path != "")
	{
		QString name	= QFileInfo(path).completeBaseName(),
				ext		= QFileInfo(path).suffix();
		switch (_mode)
		{
		case FileEvent::FileSave:			ext = "jasp";	break;
		case FileEvent::FileExportResults:	ext = "html";	break;
		case FileEvent::FileExportData:
		case FileEvent::FileGenerateData:	ext = "csv";	break;
		default:											break;
		}
		DefaultOutFileName = name + "." + ext;
	}

	return DefaultOutFileName;	
}

void FileMenu::dataSetIOCompleted(FileEvent *event)
{
	if (event->operation() == FileEvent::FileSave || event->operation() == FileEvent::FileOpen)
	{
		if (event->isSuccessful())
		{
			//  don't add examples to the recent list
			if (!event->isReadOnly())
			{
				_recentFiles->pushRecentFilePath(event->path());
				_computer->addRecentFolder(event->path());
			}

			if(event->operation() == FileEvent::FileSave || (event->operation() == FileEvent::FileOpen && !event->isReadOnly()))
			{
				QString datafile = event->dataFilePath();
				if (datafile.isEmpty())
					datafile = QString::fromStdString(DataSetPackage::pkg()->dataFilePath());
				setCurrentDataFile(datafile);
			}

			// all this stuff is a hack
			QFileInfo info(event->path());
			_computer->setFileName(info.completeBaseName());

			_currentFilePath		= event->path();
			_currentFileType		= event->type();
			_currentFileReadOnly	= event->isReadOnly();
			_OSF->setProcessing(false);
		}
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (event->isSuccessful())		setCurrentDataFile(event->dataFilePath());
		else
			Log::log() << "Sync failed: " << event->getLastError().toStdString() << std::endl;
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		_computer->clearFileName();
		_currentFilePath		= "";
		_currentFileType		= Utils::FileType::unknown;
		_currentFileReadOnly	= false;
		clearSyncData();
	}

	_resourceButtons->setButtonEnabled(ResourceButtons::CurrentFile, !_currentDataFile->getCurrentFilePath().isEmpty());
		
	if (event->isSuccessful())
	{
		switch(event->operation())
		{
		case FileEvent::FileOpen:
		case FileEvent::FileSave:
			_actionButtons->setEnabled(ActionButtons::Save,				event->type() == Utils::FileType::jasp || event->operation() == FileEvent::FileSave);
			_actionButtons->setEnabled(ActionButtons::SaveAs,			true);
			_actionButtons->setEnabled(ActionButtons::ExportResults,	true);
			_actionButtons->setEnabled(ActionButtons::ExportData,		true);
			_actionButtons->setEnabled(ActionButtons::SyncData,			true);
			_actionButtons->setEnabled(ActionButtons::Close,			true);
			break;

		case FileEvent::FileClose:
			_actionButtons->setEnabled(ActionButtons::Save,				false);
			_actionButtons->setEnabled(ActionButtons::SaveAs,			false);
			_actionButtons->setEnabled(ActionButtons::ExportResults,	false);
			_actionButtons->setEnabled(ActionButtons::ExportData,		false);
			_actionButtons->setEnabled(ActionButtons::SyncData,			false);
			_actionButtons->setEnabled(ActionButtons::Close,			false);
			_computer->setMode(FileEvent::FileOpen);
			break;

		default:
			//Do nothing?
			break;
		}
	}
}

void FileMenu::syncDataFile(const QString& path)
{
	bool autoSync = Settings::value(Settings::DATA_AUTO_SYNCHRONIZATION).toBool();
	if (autoSync)
		setSyncRequest(path);
}

void FileMenu::refresh()
{
	_resourceButtons->refresh();
	_actionButtons->refresh();
	_dataLibrary->refres();
}

void FileMenu::dataFileModifiedHandler(QString path)
{
	_mainWindow->setCheckAutomaticSync(false);
	syncDataFile(path);
}

void FileMenu::dataSetIORequestHandler(FileEvent *event)
{
	connect(event, &FileEvent::completed,		this, &FileMenu::dataSetIOCompleted			);

	emit dataSetIORequest(event);

	setVisible(false); //If we just did something we are now done with the filemenu right?
}

void FileMenu::analysisAdded(Analysis *analysis)
{
	_actionButtons->setEnabled(ActionButtons::Close,			true);
	_actionButtons->setEnabled(ActionButtons::SaveAs,			true);
	_actionButtons->setEnabled(ActionButtons::ExportResults,	true);
}

void FileMenu::setSyncFile(FileEvent *event)
{
	if (event->isSuccessful())
		setCurrentDataFile(event->path());
}

void FileMenu::dataColumnAdded(QString columnName)
{
	if(_currentDataFile->getCurrentFilePath() != "" && checkSyncFileExists(_currentDataFile->getCurrentFilePath()))
	{
		//Ok a column was added to the data but we already have a sync file so we should re-generate the data!

		FileEvent * event = new FileEvent(this, FileEvent::FileGenerateData);

		connect(event, &FileEvent::completed, this, &FileMenu::setSyncFile);
		event->setPath(_currentDataFile->getCurrentFilePath());

		dataSetIORequestHandler(event);
    }
}

void FileMenu::analysesExportResults()
{
    _computer->analysesExportResults();
}

void FileMenu::actionButtonClicked(const ActionButtons::FileOperation action)
{	
	switch (action)
	{
	case ActionButtons::FileOperation::Open:				setSaveMode(FileEvent::FileOpen);			break;
	case ActionButtons::FileOperation::SaveAs:				setSaveMode(FileEvent::FileSave);			break;
	case ActionButtons::FileOperation::ExportResults:		setSaveMode(FileEvent::FileExportResults);	break;
	case ActionButtons::FileOperation::ExportData:  		setSaveMode(FileEvent::FileExportData);		break;
	case ActionButtons::FileOperation::SyncData:			setSaveMode(FileEvent::FileSyncData);		break;
	case ActionButtons::FileOperation::Close:				close();									break;
	case ActionButtons::FileOperation::Save:
		if (getCurrentFileType() == Utils::FileType::jasp && ! isCurrentFileReadOnly())
			save();
		else
			setSaveMode(FileEvent::FileSave);			
		break;



	case ActionButtons::FileOperation::About:
		setVisible(false);
		showAboutRequest();
		break;
	default:
		break;
	}
}

void FileMenu::resourceButtonClicked(const int buttonType)
{
	if (buttonType == ResourceButtons::OSF)
		_OSF->attemptToConnect();
}

void FileMenu::showAboutRequest()
{
	emit showAbout();
}

void FileMenu::setSyncRequest(const QString& path)
{
	if (path.isEmpty())
		return;

	if (checkSyncFileExists(path, true))
	{
		FileEvent *event = new FileEvent(this, FileEvent::FileSyncData);
		event->setPath(path);

		dataSetIORequestHandler(event);
	}
}

bool FileMenu::checkSyncFileExists(const QString &path, bool waitForExistence)
{
	if (path.startsWith("http"))
		return true;

	bool exist = false;
	int maxExistTrials = waitForExistence ? 5 : 1;
	int existTrials = 1;

	while (!QFileInfo::exists(path) && existTrials < maxExistTrials)
	{
		// sometimes the file does not exist temporarily (when using Excel especially)
		Log::log() << "Could not find sync file" << std::endl;
		existTrials++;
		Utils::sleep(100);
	}

	if (QFileInfo::exists(path))
	{
		int trials = 0;
		while (Utils::getFileSize(path.toStdString()) == 0 && trials < 5)
		{
			// sometimes it is temporarily empty...
			trials++;
			Utils::sleep(100);
		}

		if (trials < 5)	exist = true;
		else			Log::log() << "Sync file is empty: " << path.toStdString() << std::endl;

	}
	else
		Log::log() << "Sync file does not exist: " << path.toStdString() << std::endl;

	if (!exist)
		clearSyncData();

	return exist;
}


void FileMenu::clearSyncData()
{
	setDataFileWatcher(false); // must be done before setting the current to empty.
	_currentDataFile->setCurrentFilePath(QString());
}

bool FileMenu::clearOSFFromRecentList(QString path)
{
	return OnlineDataManager::determineProvider(path) != OnlineDataManager::OSF;
}

void FileMenu::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);

	if(!_visible)
	{
		_resourceButtons->setSelectedButton(ResourceButtons::None);
		_actionButtons->setSelectedAction(ActionButtons::None);
	}
}

void FileMenu::showFileOpenMenu()
{
	setVisible(true);
	_actionButtons->setSelectedAction(ActionButtons::Open);
}

void FileMenu::showPreferences()
{
	setVisible(true);
	_actionButtons->setSelectedAction(ActionButtons::Preferences);
}
