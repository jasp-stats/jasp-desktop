//
// Copyright (C) 2013-2016 University of Amsterdam
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
	_dataSetHasPathAndIsntReadOnly = false;

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
	_tabBar->addTab("Close");

	_tabBar->setTabEnabled(FileOperation::Save, false);
	_tabBar->setTabEnabled(FileOperation::SaveAs, false);
	_tabBar->setTabEnabled(FileOperation::ExportResults, false);
	_tabBar->setTabEnabled(FileOperation::ExportData, false);
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
			_dataSetHasPathAndIsntReadOnly = ! event->isReadOnly();

			_tabBar->setTabEnabled(FileOperation::Save, _dataSetHasPathAndIsntReadOnly); //Save
			_tabBar->setTabEnabled(FileOperation::SaveAs, true); //Save As
			_tabBar->setTabEnabled(FileOperation::ExportResults, true); //Export Results
			_tabBar->setTabEnabled(FileOperation::ExportData, true); //Export Data
			_tabBar->setTabEnabled(FileOperation::Close, true); //Close
		}
		else if (event->operation() == FileEvent::FileSave)
		{
			_tabBar->setTabEnabled(FileOperation::Save, true);
			_dataSetHasPathAndIsntReadOnly = true;
		}
		else if (event->operation() == FileEvent::FileClose)
		{
			_dataSetHasPathAndIsntReadOnly = true;
			_tabBar->setTabEnabled(FileOperation::Save, false);
			_tabBar->setTabEnabled(FileOperation::SaveAs, false);
			_tabBar->setTabEnabled(FileOperation::ExportResults, false);
			_tabBar->setTabEnabled(FileOperation::ExportData, false);
			_tabBar->setTabEnabled(FileOperation::Close, false);
		}
	}
}

void BackStageWidget::tabPageChanging(int index, bool &cancel)
{
	switch (index)
	{
	case 0:  // Open
		_openAndSaveWidget->setSaveMode(FileEvent::FileOpen);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case 1:  // Save
		if (_dataSetHasPathAndIsntReadOnly)
			_openAndSaveWidget->save();
		else
		{
			_tabBar->setCurrentIndex(2);
			_openAndSaveWidget->setSaveMode(FileEvent::FileSave);
			_tabPages->setCurrentWidget(_openAndSaveWidget);
		}
		cancel = true;
		break;

	case 2:  // Save As
		_openAndSaveWidget->setSaveMode(FileEvent::FileSave);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case 3:  // Export Results
		_openAndSaveWidget->setSaveMode(FileEvent::FileExportResults);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case 4:  // Export Data
		_openAndSaveWidget->setSaveMode(FileEvent::FileExportData);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		break;

	case 5: // Close
		_openAndSaveWidget->close();
		_tabBar->setCurrentIndex(0);
		_openAndSaveWidget->setSaveMode(FileEvent::FileOpen);
		_tabPages->setCurrentWidget(_openAndSaveWidget);
		cancel = true;
		break;
	}
}

