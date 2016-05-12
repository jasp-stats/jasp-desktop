//
// Copyright (C) 2016 University of Amsterdam
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

#include "fsbmrecent.h"

OpenSaveWidget::OpenSaveWidget(QWidget *parent) : QWidget(parent)
{
	_mode = FileEvent::FileOpen;
	_currentFileHasPath = false;
	_currentFileReadOnly = false;

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
	_fsmExamples = new FSBMExamples(this);

	_bsRecent = new FSBrowser(_tabWidget);
	_bsRecent->setFSModel(_fsmRecent);

	_bsComputer = new BackstageComputer(_tabWidget);

	_bsOSF = new BackstageOSF(_tabWidget);


	_bsExamples = new FSBrowser(_tabWidget);
	_bsExamples->setFSModel(_fsmExamples);

	_tabWidget->addTab(_bsRecent, "Recent");
	_tabWidget->addTab(_bsComputer, "Computer");

	_tabWidget->addTab(_bsOSF, "OSF");

	_tabWidget->addTab(_bsExamples, "Examples");

	connect(_bsRecent, SIGNAL(entryOpened(QString)), this, SLOT(dataSetOpenRequestHandler(QString)));
	connect(_bsComputer, SIGNAL(dataSetIORequest(FileEvent *)), this, SLOT(dataSetIORequestHandler(FileEvent *)));

	connect(_bsOSF, SIGNAL(dataSetIORequest(FileEvent *)), this, SLOT(dataSetIORequestHandler(FileEvent *)));

	connect(_bsExamples, SIGNAL(entryOpened(QString)), this, SLOT(dataSetOpenExampleRequestHandler(QString)));
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
		_tabWidget->showTab(_bsRecent);
		_tabWidget->showTab(_bsExamples);
	}
	else
	{
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

	if ( ! path.endsWith(".jasp", Qt::CaseInsensitive))
		event->setReadOnly();

	dataSetIORequestHandler(event);

	return event;
}

FileEvent *OpenSaveWidget::save()
{
	FileEvent *event;

	if (_currentFileHasPath == false || _currentFileReadOnly)
	{
		event = _bsComputer->browseSave();
		if (event->isCompleted())
			return event;
	}
	else
	{
		event = new FileEvent(this, FileEvent::FileSave);
		event->setPath(_currentFilePath);
	}

	dataSetIORequestHandler(event);

	return event;
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

			// all this stuff is a hack
			QFileInfo info(event->path());
			_bsComputer->setFileName(info.baseName());

			_currentFilePath = event->path();
			_currentFileHasPath = true;
			_currentFileReadOnly = event->isReadOnly();
		}
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		_bsComputer->clearFileName();

		_currentFileHasPath = false;
		_currentFilePath = "";
	}
}

void OpenSaveWidget::dataSetIORequestHandler(FileEvent *event)
{
	connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
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

