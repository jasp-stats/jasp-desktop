//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "backstagewidget.h"

#include <QHBoxLayout>
#include <QLabel>
#include <QFileInfo>

BackStageWidget::BackStageWidget(QWidget *parent) : QWidget(parent)
{
	_tabBar = new VerticalTabBar(this);
	_tabPages = new QStackedWidget(this);

	_openAndSaveWidget = new OpenSaveWidget();

	QHBoxLayout *layout = new QHBoxLayout(this);
	layout->setSpacing(0);
	layout->setContentsMargins(0, 0, 0, 0);

	layout->addWidget(_tabBar);
	layout->addWidget(_tabPages);

	QString styleSheet;

	QFile firstStyleSheetFile(":/backstage/firsttabsstylesheet.qss");
	firstStyleSheetFile.open(QFile::ReadOnly);
	styleSheet = QString::fromUtf8(firstStyleSheetFile.readAll());

	_tabBar->setMinimumWidth(150);
	_tabBar->setStyleSheet(styleSheet);


	QFile secondStyleSheetFile(":/backstage/secondtabsstylesheet.qss");
	secondStyleSheetFile.open(QFile::ReadOnly);
	styleSheet = QString::fromUtf8(secondStyleSheetFile.readAll());

	_openAndSaveWidget->tabWidget()->tabBar()->setMinimumWidth(200);
	_openAndSaveWidget->tabWidget()->tabBar()->setStyleSheet(styleSheet);

	_tabPages->addWidget(_openAndSaveWidget);

	_tabBar->addTab("Open");
	_tabBar->addTab("Save");
	_tabBar->addTab("Save As");
	_tabBar->addTab("Export Results");
	_tabBar->addTab("Export Data");
	_tabBar->addTab("Sync Data");
	_tabBar->addTab("Close");

	_tabBar->setTabEnabled(FileOperation::Save, false);
	_tabBar->setTabEnabled(FileOperation::SaveAs, false);
	_tabBar->setTabEnabled(FileOperation::ExportResults, false);
	_tabBar->setTabEnabled(FileOperation::ExportData, false);
	_tabBar->setTabEnabled(FileOperation::SyncData, false);
	_tabBar->setTabEnabled(FileOperation::Close, false);

	connect(_openAndSaveWidget, SIGNAL(dataSetIORequest(FileEvent*)), this, SLOT(dataSetIORequestHandler(FileEvent*)));
	connect(_tabBar, SIGNAL(currentChanging(int,bool&)), this, SLOT(tabPageChanging(int,bool&)));
}

void BackStageWidget::analysisAdded(Analysis *analysis) {
	_tabBar->setTabEnabled(FileOperation::SaveAs, true);
	_tabBar->setTabEnabled(FileOperation::ExportResults, true);
}

void BackStageWidget::setOnlineDataManager(OnlineDataManager *odm)
{
	_openAndSaveWidget->setOnlineDataManager(odm);
}

void BackStageWidget::setLog(ActivityLog *log)
{

}

FileEvent *BackStageWidget::open()
{
	return _openAndSaveWidget->open();
}

FileEvent *BackStageWidget::open(const QString &filepath)
{
	return _openAndSaveWidget->open(filepath);
}

FileEvent *BackStageWidget::save()
{
	return _openAndSaveWidget->save();
}

void BackStageWidget::sync()
{
	_openAndSaveWidget->sync();
}

FileEvent *BackStageWidget::close()
{
	return _openAndSaveWidget->close();
}

void BackStageWidget::dataSetIORequestHandler(FileEvent *event)
{
	connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIORequestCompleted(FileEvent*)));
	emit dataSetIORequest(event);
}

void BackStageWidget::dataSetIORequestCompleted(FileEvent *event)
{
	if (event->successful())
	{
		if (event->operation() == FileEvent::FileOpen)
		{
			_tabBar->setTabEnabled(FileOperation::Save, event->type() == Utils::FileType::jasp); //Save
			_tabBar->setTabEnabled(FileOperation::SaveAs, true); //Save As
			_tabBar->setTabEnabled(FileOperation::ExportResults, true); //Export Results
			_tabBar->setTabEnabled(FileOperation::ExportData, true); //Export Data
			_tabBar->setTabEnabled(FileOperation::SyncData, true); //Close
			_tabBar->setTabEnabled(FileOperation::Close, true); //Close
		}
		else if (event->operation() == FileEvent::FileSave)
		{
			_tabBar->setTabEnabled(FileOperation::Save, true);
		}
		else if (event->operation() == FileEvent::FileClose)
		{
			_tabBar->setTabEnabled(FileOperation::Save, false);
			_tabBar->setTabEnabled(FileOperation::SaveAs, false);
			_tabBar->setTabEnabled(FileOperation::ExportResults, false);
			_tabBar->setTabEnabled(FileOperation::ExportData, false);
			_tabBar->setTabEnabled(FileOperation::SyncData, false);
			_tabBar->setTabEnabled(FileOperation::Close, false);
		}
	}
}

void BackStageWidget::tabPageChanging(int index, bool &cancel)
{
	switch (index)
	{
	case FileOperation::Open:  // Open
		_openAndSaveWidget->setSaveMode(FileEvent::FileOpen);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case FileOperation::Save:  // Save
		if (_openAndSaveWidget->getCurrentFileType() == Utils::FileType::jasp)
			_openAndSaveWidget->save();
		else
		{
			_tabBar->setCurrentIndex(FileOperation::SaveAs);
			_openAndSaveWidget->setSaveMode(FileEvent::FileSave);
			_tabPages->setCurrentWidget(_openAndSaveWidget);
		}
		cancel = true;
		break;

	case FileOperation::SaveAs:  // Save As
		_openAndSaveWidget->setSaveMode(FileEvent::FileSave);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case FileOperation::ExportResults:  // Export Results
		_openAndSaveWidget->setSaveMode(FileEvent::FileExportResults);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case FileOperation::ExportData:  // Export Data
		_openAndSaveWidget->setSaveMode(FileEvent::FileExportData);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case FileOperation::SyncData:  // Sync Data
		_openAndSaveWidget->setSaveMode(FileEvent::FileSyncData);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		_openAndSaveWidget->changeTabIfCurrentFileEmpty();
		break;

	case FileOperation::Close: // Close
		_openAndSaveWidget->close();
		_tabBar->setCurrentIndex(FileOperation::Open);
		_openAndSaveWidget->setSaveMode(FileEvent::FileOpen);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		cancel = true;
		break;
	}
}

void BackStageWidget::setSyncFile(FileEvent *event)
{
	if (event->successful())
	{
		_openAndSaveWidget->setCurrentDataFile(event->path());
	}
}

void BackStageWidget::dataAutoSynchronizationChanged(bool on)
{
	_openAndSaveWidget->setDataFileWatcher(on);
}
