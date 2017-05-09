//
// Copyright (C) 2017 University of Amsterdam
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

#include "opensavewidget.h"

#include <QGridLayout>
#include <QLabel>
#include <QFileInfo>
#include <QFileDialog>
#include <QMessageBox>

OpenSaveWidget::OpenSaveWidget(QWidget *parent) : QWidget(parent)
{
	_mode = FileEvent::FileOpen;
	_currentFileType = Utils::FileType::unknown;

	QGridLayout *layout = new QGridLayout(this);
	layout->setContentsMargins(0, 0, 0, 0);
	setLayout(layout);

	_tabWidget = new VerticalTabWidget(this);
	_tabWidget->setMaximumWidth(800);

	QWidget *webWidget = new QWidget(this);
	/*QGridLayout *webWidgetLayout = new QGridLayout(webWidget);
	webWidgetLayout->setMargin(36);

	QFrame *webFrame = new QFrame(webWidget);
	QGridLayout *webLayout = new QGridLayout(webFrame);
	webFrame->setLayout(webLayout);
	webLayout->setMargin(0);
	webFrame->setFrameShape(QFrame::Box);
	webFrame->setFrameStyle(QFrame::Panel);
	webFrame->setLineWidth(1);
	webFrame->setMinimumWidth(200);

	QWebView *webView = new QWebView(webFrame);
	webLayout->addWidget(webView);

	webWidgetLayout->addWidget(webFrame);*/

	layout->addWidget(_tabWidget, 0, 0);
	layout->addWidget(webWidget, 0, 1);

	_fsmRecent   = new FSBMRecent(this);
	_fsmCurrent  = new FSBMCurrent(this);
	_fsmExamples = new FSBMExamples(this);

	_bsRecent = new FSBrowser(_tabWidget);
	_bsRecent->setFSModel(_fsmRecent);

	_bsCurrent = new FSBrowser(_tabWidget, FSBrowser::BrowseCurrent);
	_bsCurrent->setFSModel(_fsmCurrent);

	_bsComputer = new BackstageComputer(_tabWidget);

	_bsOSF = new BackstageOSF(_tabWidget);


	_bsExamples = new FSBrowser(_tabWidget);
	_bsExamples->setFSModel(_fsmExamples);

	_tabWidget->addTab(_bsRecent, "Recent");
	_tabWidget->addTab(_bsCurrent, "Current");
	_tabWidget->addTab(_bsComputer, "Computer");
	_tabWidget->addTab(_bsOSF, "OSF");
	_tabWidget->addTab(_bsExamples, "Examples");

	_tabWidget->hideTab(_bsCurrent);

	connect(_bsRecent, SIGNAL(entryOpened(QString)), this, SLOT(dataSetOpenRequestHandler(QString)));
	connect(_bsCurrent, SIGNAL(entryOpened(QString)), this, SLOT(dataSetOpenCurrentRequestHandler(QString)));
	connect(&_watcher, SIGNAL(fileChanged(const QString&)), this, SLOT(dataFileModifiedHandler(const QString&)));
	connect(_bsComputer, SIGNAL(dataSetIORequest(FileEvent *)), this, SLOT(dataSetIORequestHandler(FileEvent *)));
	connect(_bsOSF, SIGNAL(dataSetIORequest(FileEvent *)), this, SLOT(dataSetIORequestHandler(FileEvent *)));
	connect(_bsExamples, SIGNAL(entryOpened(QString)), this, SLOT(dataSetOpenExampleRequestHandler(QString)));

	VerticalTabWidget *osvw = tabWidget();
	VerticalTabBar *vtb = osvw->tabBar();
	connect(vtb, SIGNAL(currentChanged(int)), this, SLOT(tabWidgetChanged(int)));
	connect(vtb, SIGNAL(currentChanging(int,bool&)), this, SLOT(tabWidgetChanging(int,bool&)));
}

bool OpenSaveWidget::changeTabIfCurrentFileEmpty()
{
	bool empty = false;
	if (_fsmCurrent->getCurrent().isEmpty())
	{
		_tabWidget->tabBar()->click(FileLocation::Computer);
		empty = true;
	}

	return empty;
}

void OpenSaveWidget::tabWidgetChanging(int index, bool &cancel)
{
	if (index == FileLocation::Current)
	{
		// Do not set the current tab if no current file is present.
		if (changeTabIfCurrentFileEmpty())
			cancel = true;
	}
}

void OpenSaveWidget::tabWidgetChanged(int index)
{
	//Check the OSF tab
	if (index == FileLocation::OSF)
		_bsOSF->attemptToConnect();
}

VerticalTabWidget *OpenSaveWidget::tabWidget()
{
	return _tabWidget;
}

void OpenSaveWidget::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_bsOSF->setOnlineDataManager(odm);
	connect(_odm, SIGNAL(authenticationCleared(int)), this, SLOT(clearOnlineDataFromRecentList(int)));
}

void OpenSaveWidget::setSaveMode(FileEvent::FileMode mode)
{
	_mode = mode;

	_bsComputer->setMode(_mode);

	_bsOSF->setMode(_mode);


	if (_mode == FileEvent::FileOpen)
	{
		_tabWidget->hideTab(_bsCurrent);
		_tabWidget->showTab(_bsRecent);
		_tabWidget->showTab(_bsExamples);
	}
	else if (_mode == FileEvent::FileSyncData)
	{
		_tabWidget->showTab(_bsCurrent);
		_tabWidget->hideTab(_bsRecent);
		_tabWidget->hideTab(_bsExamples);
		_tabWidget->tabBar()->setTabEnabled(FileLocation::Current, !_fsmCurrent->getCurrent().isEmpty());
	}
	else
	{
		_tabWidget->hideTab(_bsCurrent);
		_tabWidget->hideTab(_bsRecent);
		_tabWidget->hideTab(_bsExamples);
	}
}

FileEvent *OpenSaveWidget::open()
{
	FileEvent *event = _bsComputer->browseOpen();
	if ( ! event->isCompleted())
		dataSetIORequestHandler(event);
	return event;
}

FileEvent *OpenSaveWidget::open(const QString &path)
{
	FileEvent *event = new FileEvent(this, FileEvent::FileOpen);
	event->setPath(path);
	dataSetIORequestHandler(event);

	return event;
}

FileEvent *OpenSaveWidget::save()
{
	FileEvent *event;

	if (_currentFileType != Utils::FileType::jasp)
	{
		event = _bsComputer->browseSave();
		if (event->isCompleted())
			return event;
	}
	else
	{
		event = new FileEvent(this, FileEvent::FileSave);
		if (!event->setPath(_currentFilePath))
		{
			QMessageBox::warning(this, "File Types", event->getLastError());
			event->setComplete(false, "Failed to open file from OSF");
			return event;
		}
	}

	dataSetIORequestHandler(event);

	return event;
}

void OpenSaveWidget::sync()
{
	QString path = _fsmCurrent->getCurrent();
	if (path.isEmpty())
	{
		QString message = "JASP has no associated data file (csv, sav or ods file) to be synchronized with. Do you want to search for such a data file on your computer?\nNB: You can set this data file also via menu File/Sync Data.";
		QMessageBox msgBox(QMessageBox::Question, QString("No associated data file"), message,
						   QMessageBox::Yes|QMessageBox::Cancel);
		int reply = msgBox.exec();
		if (reply == QMessageBox::Cancel)
			return;

		QString caption = "Find Data File";
		QString filter = "Data File (*.csv *.txt *.sav *.ods)";
		path = QFileDialog::getOpenFileName(this, caption, "", filter);
	}

	dataSetOpenCurrentRequestHandler(path);
}

FileEvent *OpenSaveWidget::close()
{
	FileEvent *event = new FileEvent(this, FileEvent::FileClose);
	dataSetIORequestHandler(event);

	return event;
}

void OpenSaveWidget::clearOnlineDataFromRecentList(int provider)
{
	if ((OnlineDataManager::Provider)provider == OnlineDataManager::OSF)
		_fsmRecent->filter(&clearOSFFromRecentList);
}

bool OpenSaveWidget::clearOSFFromRecentList(QString path)
{
	return OnlineDataManager::determineProvider(path) != OnlineDataManager::OSF;
}

void OpenSaveWidget::dataSetIOCompleted(FileEvent *event)
{
	if (event->operation() == FileEvent::FileSave || event->operation() == FileEvent::FileOpen)
	{
		if (event->successful())
		{	
			if (_fsmExamples->contains(event->path()) == false)
			{
				//  don't add examples to the recent list
				_fsmRecent->addRecent(event->path());
				_bsComputer->addRecent(event->path());
			}

			if (event->operation() == FileEvent::FileOpen && !event->isReadOnly())
				setCurrentDataFile(event->dataFilePath());

			// all this stuff is a hack
			QFileInfo info(event->path());
			_bsComputer->setFileName(info.baseName());

			_currentFilePath = event->path();
			_currentFileType = event->type();
		}
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (event->successful())
			setCurrentDataFile(event->dataFilePath());
		else
			std::cout << "Sync failed: " << event->getLastError().toStdString() << std::endl;
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		_bsComputer->clearFileName();
		_currentFilePath = "";
		_currentFileType = Utils::FileType::unknown;
		clearSyncData();
	}
}

bool OpenSaveWidget::checkSyncFileExists(const QString &path)
{
    bool exists = path.startsWith("http") ? true : (QFileInfo::exists(path) && Utils::getFileSize(path.toStdString()) > 0);
    if (!exists)
	{
        int attempts = 1;
        while (!exists && attempts < 20)
        {
            Utils::sleep(100);
            attempts++;
            exists = QFileInfo::exists(path) && Utils::getFileSize(path.toStdString()) > 0;
        }
    }
    if (!exists)
    {
        std::cout << "Sync file does not exist: " << path.toStdString() << std::endl;
        std::cout.flush();
		clearSyncData();
	}

	return exists;
}

void OpenSaveWidget::clearSyncData()
{
	setDataFileWatcher(false); // must be done before setting the current to empty.
	_fsmCurrent->setCurrent(QString());
	_tabWidget->tabBar()->setTabEnabled(FileLocation::Current, false);
	_tabWidget->tabBar()->click(FileLocation::Computer);
}

void OpenSaveWidget::setCurrentDataFile(const QString &path)
{
	QString currentPath = _fsmCurrent->getCurrent();
	if (!currentPath.isEmpty())
		_watcher.removePath(currentPath);

	bool setCurrentPath = true;
	bool enableCurrentTab = false;
	if (!path.isEmpty())
	{
		if (checkSyncFileExists(path))
		{
			enableCurrentTab = true;
			int sync = _settings.value("dataAutoSynchronization", 1).toInt();
			if (sync > 0)
				_watcher.addPath(path);
		}
		else
			setCurrentPath = false;
	}

	if (setCurrentPath)
		_fsmCurrent->setCurrent(path);
	_tabWidget->tabBar()->setTabEnabled(FileLocation::Current, enableCurrentTab);
}

void OpenSaveWidget::dataSetIORequestHandler(FileEvent *event)
{
	connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
	connect(event, SIGNAL(dataFileChanged(QString)), this, SLOT(dataFileModifiedHandler(QString)));
	emit dataSetIORequest(event);
}

void OpenSaveWidget::dataSetOpenRequestHandler(QString path)
{
	open(path);
}

void OpenSaveWidget::dataSetOpenExampleRequestHandler(QString path)
{
	FileEvent *event = new FileEvent(this);
	event->setPath(path);
	event->setReadOnly();

	dataSetIORequestHandler(event);
}

void OpenSaveWidget::dataFileModifiedHandler(QString path)
{
	int autoSync = _settings.value("dataAutoSynchronization", 1).toInt();
	if (autoSync > 0)
		dataSetOpenCurrentRequestHandler(path);
}

void OpenSaveWidget::setDataFileWatcher(bool watch)
{
	QString path = _fsmCurrent->getCurrent();
	if (!path.isEmpty())
	{
		if (watch && !_fsmCurrent->isOnlineFile())
			_watcher.addPath(path);
		else
			_watcher.removePath(path);
	}
}

Utils::FileType OpenSaveWidget::getCurrentFileType()
{
	return _currentFileType;
}

void OpenSaveWidget::dataSetOpenCurrentRequestHandler(QString path)
{
	if (path.isEmpty())
		return;

	if (checkSyncFileExists(path))
	{
		FileEvent *event = new FileEvent(this, FileEvent::FileSyncData);
		event->setPath(path);

		dataSetIORequestHandler(event);
	}
}
