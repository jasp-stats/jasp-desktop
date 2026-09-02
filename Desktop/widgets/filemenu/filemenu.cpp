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
#include "utilities/messageforwarder.h"
#include "log.h"
#include "data/datasetpackage.h"
#include "mainwindow.h"
#include "utilities/appdirs.h"
#include "data/jaspencryptiondata.h"
#include "data/fileeventrouter.h"

FileMenu::FileMenu(QObject *parent) : FileEventRouter(parent)
{
	_mainWindow				= qobject_cast<MainWindow*>(parent);
	_recentFiles			= new RecentFiles			(this);
	_currentDataFile		= new CurrentDataFile		(this);
	_autoSaves				= new AutoSaves				(this);
	_computer				= new Computer				(this);
	_OSF					= new OSF					(this);
	_database				= new DatabaseFileMenu		(this);
	_dataLibrary			= new DataLibrary			(this);
	_actionButtons			= new ActionButtons			(this);
	_resourceButtons		= new ResourceButtons		(this);
	_resourceButtonsVisible	= new ResourceButtonsVisible(this, _resourceButtons);
	
	connect(_actionButtons,		&ActionButtons::buttonClicked,				this,			&FileMenu::actionButtonClicked		);
	connect(_actionButtons,		&ActionButtons::selectedActionChanged,		this,			&FileMenu::setFileoperation			);
	connect(_resourceButtons,	&ResourceButtons::selectedButtonChanged,	this,			&FileMenu::resourceButtonClicked	);
	connect(_currentDataFile,	&CurrentDataFile::setCheckAutomaticSync,	_mainWindow,	&MainWindow::setCheckAutomaticSync	);

	_actionButtons->setEnabled(ActionButtons::New,				true);
	_actionButtons->setEnabled(ActionButtons::Open,				true);
	_actionButtons->setEnabled(ActionButtons::Save,				false);
	_actionButtons->setEnabled(ActionButtons::SaveAs,			false);
	_actionButtons->setEnabled(ActionButtons::ExportResults,	false);
	_actionButtons->setEnabled(ActionButtons::ExportData,		false);
	_actionButtons->setEnabled(ActionButtons::SyncData,			false);
	_actionButtons->setEnabled(ActionButtons::Close,			false);
	_actionButtons->setEnabled(ActionButtons::Preferences,		true);
	_actionButtons->setEnabled(ActionButtons::Contact,			true);
	_actionButtons->setEnabled(ActionButtons::Community,		true);
	_actionButtons->setEnabled(ActionButtons::About,			true);

	setResourceButtonsVisibleFor(_fileoperation);
}

FileMenu::~FileMenu()
{
	//FileEventRouter's destructor unregisters this menu as the current router, so events created
	//afterwards log about the missing target instead of connecting into freed memory.
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


void FileMenu::setMode(FileEvent::FileMode mode)
{
	_mode = mode;
	_OSF->setCurrentFileName(getDefaultOutFileName());

	emit modeChanged(_mode);
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
	event->starts();

	return event;
}

FileEvent *FileMenu::open(const Json::Value & dbJson)
{
	FileEvent *event = new FileEvent(this, FileEvent::FileOpen);
	event->setDatabase(dbJson);
	event->setFileType(Utils::FileType::database);
	event->starts();

	return event;
}

FileEvent *FileMenu::saveAs()
{
    FileEvent* event = _computer->browseSave();
    return event;
}

FileEvent *FileMenu::newData()
{
	FileEvent *event = new FileEvent(this, FileEvent::FileNew);

	event->starts();

	return event;
}

FileEvent *FileMenu::save()
{
	if (_currentFileType != Utils::FileType::jasp || DataSetPackage::pkg()->currentJaspFileIsNonSaveable())
		return saveAs();

	FileEvent *event = new FileEvent(this, FileEvent::FileSave);
	if (!event->setPath(_currentFilePath))
	{
		MessageForwarder::showWarning(tr("File Types"), event->getLastError());
		event->setComplete(false, tr("Failed to open file from OSF"));
	}
	else
		event->starts();

	return event;
}

void FileMenu::sync()
{
	DataSet * ds = DataSetPackage::pkg()->dataSet();
	if(!ds)
		return;

	if(ds->isDatabase())
	{
		ds->syncer().syncNow();
	}
	else
	{
		QString path = _currentDataFile->getCurrentFilePath();

		if (path.isEmpty())
		{
			if(!MessageForwarder::showYesNo(tr("No associated data file"),
						tr("JASP has no associated data file to be synchronized with.\nDo you want to search for such a data file on your computer?\nNB: You can also set this data file via menu File/Sync Data.")))
				return;

			path =  MessageForwarder::browseOpenFile(tr("Find Data File"), "", tr("Data File").arg("*.csv *.txt *.tsv *.sav *.zsav *.ods *.xls *.xlsx *.dta *.por *.sas7bdat *.sas7bcat *.xpt *.rdata *.rds *.mwx *.mpx"));
		}

		_mainWindow->setCheckAutomaticSync(false);
		setSyncRequest(path);
	}
}

void FileMenu::close()
{
	FileEvent *event = new FileEvent(this, FileEvent::FileClose);
	event->starts();

	setMode(FileEvent::FileOpen);
	_actionButtons->setSelectedAction(ActionButtons::FileOperation::Open);
}

void FileMenu::setCurrentDataFile(const QString &path)
{
	Log::log() << "[FileMenu::setCurrentDataFile] START: path=" << path.toStdString() << std::endl;
	QString currentPath = _currentDataFile->getCurrentFilePath();

	if (!currentPath.isEmpty())
	{
		DataSet * ds = DataSetPackage::pkg()->dataSet();
		if(ds)
			ds->syncer().stopFileSyncing();
	}

	bool setCurrentPath = true;
	if (!path.isEmpty())
	{
		Log::log() << "[FileMenu::setCurrentDataFile] Path not empty, checking if file exists" << std::endl;
		if (checkSyncFileExists(path))
		{
			Log::log() << "[FileMenu::setCurrentDataFile] File exists" << std::endl;
			DataSet * ds = DataSetPackage::pkg()->dataSet();
			if(ds)
				ds->syncer().startFileSyncing(path);
		}
		else
		{
			Log::log() << "[FileMenu::setCurrentDataFile] checkSyncFileExists returned false" << std::endl;
			setCurrentPath = false;
		}
	}
	if (setCurrentPath)
	{
		Log::log() << "[FileMenu::setCurrentDataFile] Setting current file path" << std::endl;
		_currentDataFile->setCurrentFilePath(path);
	}
	else
	{
		Log::log() << "[FileMenu::setCurrentDataFile] Not setting current file path" << std::endl;
	}
	Log::log() << "[FileMenu::setCurrentDataFile] END" << std::endl;
}

void FileMenu::setDataFileWatcher(bool watch)
{
	QString path = _currentDataFile->getCurrentFilePath();
	if (path.isEmpty())
		return;

	DataSet * ds = DataSetPackage::pkg()->dataSet();
	if(!ds)
		return;

	if(watch && !_currentDataFile->isOnlineFile(path))
		ds->syncer().startFileSyncing(path);
	else
		ds->syncer().stopFileSyncing();
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

void FileMenu::enableButtonsForOpenedWorkspace(bool enableSaveButton)
{
	_actionButtons->setEnabled(ActionButtons::Save,				enableSaveButton);
	_actionButtons->setEnabled(ActionButtons::SaveAs,			true);
    _actionButtons->setEnabled(ActionButtons::SaveAsEncrypt,		true);
	_actionButtons->setEnabled(ActionButtons::ExportResults,	true);
	_actionButtons->setEnabled(ActionButtons::ExportData,		true);
	_actionButtons->setEnabled(ActionButtons::SyncData,			true);
	_actionButtons->setEnabled(ActionButtons::Close,			true);
}

void FileMenu::buttonsForEmptyWorkspace()
{
	_actionButtons->setEnabled(ActionButtons::Save,				false);
	_actionButtons->setEnabled(ActionButtons::SaveAs,			false);
	_actionButtons->setEnabled(ActionButtons::ExportResults,	false);
	_actionButtons->setEnabled(ActionButtons::ExportData,		false);
	_actionButtons->setEnabled(ActionButtons::SyncData,			false);
	_actionButtons->setEnabled(ActionButtons::Close,			false);
}

void FileMenu::startFileEvent()
{
	FileEvent* event = qobject_cast<FileEvent*>(sender());
	if(!event)
		return;

	Log::log() << "[FileMenu::startFileEvent] START: event->operation()=" << event->operation() << ", event->isSuccessful()=" << event->isSuccessful() << std::endl;

	_mainWindow->fileEventRequestHandler(event);

}

void FileMenu::finalizeFileEvent()
{
	FileEvent* event = qobject_cast<FileEvent*>(sender());
	if(!event)
		return;

	//An event that never started (FileMenu::save's OSF failure path completes it on the spot) has
	//no request side behind it; finalizing it would run the failure handling twice.
	if(!event->isStarted())
	{
		event->cleanUp();
		return;
	}

	if(event->operation() != FileEvent::FileClose)
		setVisible(false); //If we just did something we are now done with the filemenu right? Except if we just closed a file

	if (event->operation() == FileEvent::FileSave || event->operation() == FileEvent::FileOpen)
	{
		if (event->isSuccessful() && !event->isTmp())
		{
			if (!event->isDatabase())
			{
				_recentFiles->pushRecentFilePath(event->path());
				if (!event->isExample())
					_computer->addRecentFolder(event->path());
			}

			if(event->operation() == FileEvent::FileSave || (event->operation() == FileEvent::FileOpen && !event->isReadOnly()))
			{
				QString datafile = event->dataFilePath();
				if (datafile.isEmpty())
					datafile = QString::fromStdString(DataSetPackage::pkg()->dataSet()->dataFilePath());
				setCurrentDataFile(datafile);
				if	(	event->operation() == FileEvent::FileOpen
					&& !event->isReadOnly()
					&&	event->type() == Utils::FileType::jasp
					&& !DataSetPackage::pkg()->isReadOnlyFile()
					&&	DataSetPackage::pkg()->dataSet()->dataFileSynch()
				)
					DataSetPackage::pkg()->dataSet()->syncer().startFileSyncing(datafile);
			}

			QFileInfo info(event->path());

			_computer->setFileName(info.dir() != QDir(AppDirs::autoSaveDir()) ? info.completeBaseName() : DataSetPackage::pkg()->autoSavedFileName());

			_currentFilePath		= event->path();
			_currentFileType		= event->type();

			_OSF->setProcessing(false);
		}
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		_computer->clearFileName();
		_currentFilePath		= "";
		_currentFileType		= Utils::FileType::unknown;
        JaspEncryptionData::getInstance()->reset();
		clearSyncData();
	}

	_resourceButtons->setButtonEnabled(ResourceButtons::CurrentFile, !_currentDataFile->getCurrentFilePath().isEmpty());

	if (event->isSuccessful() && !event->isTmp())
	{
		switch(event->operation())
		{
		case FileEvent::FileOpen:
		case FileEvent::FileSave:
			enableButtonsForOpenedWorkspace(
						(!event->isReadOnly() && event->type() == Utils::FileType::jasp)
						&&
						(event->operation() == FileEvent::FileOpen && !DataSetPackage::pkg()->filePathIsNonSaveable(event->path()) ));
			break;

		case FileEvent::FileClose:
			Log::log() << "[FileMenu::finalizeFileEvent] FileClose operation" << std::endl;
			buttonsForEmptyWorkspace();
			setMode(FileEvent::FileOpen);
			break;

		default:
			Log::log() << "[FileMenu::finalizeFileEvent] Default case (operation=" << event->operation() << ")" << std::endl;
			//Do nothing?
			break;
		}
	}
	Log::log() << "[FileMenu::finalizeFileEvent] END" << std::endl;

	_mainWindow->fileEventRequestFinalize(event);

	event->cleanUp();
}

void FileMenu::refresh()
{
	_resourceButtons->refresh();
	_actionButtons->refresh();
	_dataLibrary->refres();
}

void FileMenu::analysisAdded(Analysis *analysis)
{
	_actionButtons->setEnabled(ActionButtons::Close,			true);
	_actionButtons->setEnabled(ActionButtons::SaveAs,			true);
    _actionButtons->setEnabled(ActionButtons::SaveAsEncrypt,		true);
	_actionButtons->setEnabled(ActionButtons::ExportResults,	true);
}

void FileMenu::workspaceModified()
{
	if(DataSetPackage::pkg()->isLoaded() && getCurrentFileType() == Utils::FileType::jasp && !DataSetPackage::pkg()->currentJaspFileIsNonSaveable())
		_actionButtons->setEnabled(ActionButtons::Save, DataSetPackage::pkg()->isModified());
}

void FileMenu::setSyncFile(FileEvent *event)
{
	Log::log() << "[FileMenu::setSyncFile] START: event->isSuccessful()=" << event->isSuccessful() << ", event->path()=" << event->path().toStdString() << std::endl;
	if (event->isSuccessful())
	{
		Log::log() << "[FileMenu::setSyncFile] Calling setCurrentDataFile" << std::endl;
		setCurrentDataFile(event->path());
		Log::log() << "[FileMenu::setSyncFile] setCurrentDataFile returned" << std::endl;
	}
	else
	{
		Log::log() << "[FileMenu::setSyncFile] Event not successful, not updating current data file" << std::endl;
	}
	Log::log() << "[FileMenu::setSyncFile] END" << std::endl;
}

void FileMenu::analysesExportResults()
{
	_computer->analysesExportResults();
}

void FileMenu::actionButtonClicked(const ActionButtons::FileOperation action)
{	
	switch (action)
	{
	case ActionButtons::FileOperation::Open:				setMode(FileEvent::FileOpen);			break;
	case ActionButtons::FileOperation::SaveAs:				setMode(FileEvent::FileSave);			break;
	case ActionButtons::FileOperation::ExportResults:		setMode(FileEvent::FileExportResults);	break;
	case ActionButtons::FileOperation::ExportData:  		setMode(FileEvent::FileExportData);		break;
	case ActionButtons::FileOperation::SyncData:			setMode(FileEvent::FileSyncData);		break;
	case ActionButtons::FileOperation::Close:				close();								break;
	case ActionButtons::FileOperation::Save:
		if (getCurrentFileType() == Utils::FileType::jasp && !DataSetPackage::pkg()->currentJaspFileIsNonSaveable())
			save();
		else
			setMode(FileEvent::FileSave);			
		break;
	case ActionButtons::FileOperation::New:
			newData();
		break;

	case ActionButtons::FileOperation::About:
		setVisible(false);
		showAboutRequest();
		break;

	case ActionButtons::FileOperation::Contact:
		setVisible(false);
		showContactRequest();
		break;


	case ActionButtons::FileOperation::Community:
		setVisible(false);
		showCommunity();
		break;

	default:
		break;
	}
}

void FileMenu::resourceButtonClicked(const int buttonType)
{
	switch(buttonType)
	{
	case ResourceButtons::OSF:
		_OSF->attemptToConnect();
		break;

	case ResourceButtons::AutoSaves:
		_autoSaves->listModel()->refresh();
		break;
	}

}

void FileMenu::showAboutRequest()
{
	emit showAbout();
}

void FileMenu::showContactRequest()
{
	emit showContact();
}

void FileMenu::setSyncRequest(const QString& path, bool waitForExistence)
{
	if (path.isEmpty() || !DataSetPackage::pkg()->hasDataSet() || !checkSyncFileExists(path, waitForExistence))
		return;

	FileEvent *event = new FileEvent(this, FileEvent::FileSyncData);
	event->setPath(path);
	event->setDataSet(DataSetPackage::pkg()->dataSet());
	event->starts();
}

bool FileMenu::checkSyncFileExists(const QString &path, bool waitForExistence)
{
	if (path.startsWith("http"))
		return true;

	auto checkExists = [](const QString& path) { return QFileInfo::exists(path); };
	auto checkNotEmpty = [](const QString& path) { return Utils::getFileSize(path.toStdString()) > 0; };

	auto checkFn = [&](std::function<bool(const QString&)> fn)
	{
		if (!waitForExistence) return fn(path);

		for (int i = 0; i < 10; i++)
		{
			if (fn(path))	return true;
			Utils::sleep(100);
		}
		return false;
	};

	bool result = checkFn(checkExists) && checkFn(checkNotEmpty);

	if (!result)
	{
		Log::log() << "Could not find a valid Sync file in " << path << std::endl;
		clearSyncData();
	}

	return result;
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

void FileMenu::showAdvancedPreferences()
{
	setVisible(true);
	_actionButtons->setSelectedAction(ActionButtons::Preferences);
	_resourceButtons->setSelectedButton(ResourceButtons::PrefsAdvanced);
}

void FileMenu::exportResultsInteractive()
{
	actionButtonClicked(ActionButtons::ExportResults);
	_computer->browseMostRecent();
}
