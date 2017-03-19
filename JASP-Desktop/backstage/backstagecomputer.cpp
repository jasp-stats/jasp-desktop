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

#include "backstagecomputer.h"
#include "ui_backstagecomputer.h"

#include <QFileDialog>

#include "fsbmrecentfolders.h"


BackstageComputer::BackstageComputer(QWidget *parent) :
	BackstagePage(parent),
	ui(new Ui::BackstageComputer)
{
	_hasFileName = false;

	ui->setupUi(this);

	_model = new FSBMRecentFolders(this);

	ui->browser->setBrowseMode(FSBrowser::BrowseOpenFolder);
	ui->browser->setViewType(FSBrowser::ListView);
	ui->browser->setFSModel(_model);

	installEventFilter(this);

	connect(ui->browser, SIGNAL(entryOpened(QString)), this, SLOT(selectionMade(QString)));
	connect(ui->browseButton, SIGNAL(clicked(bool)), this, SLOT(browseSelected()));
}

BackstageComputer::~BackstageComputer()
{
	delete ui;
}

FileEvent *BackstageComputer::browseOpen(const QString &path)
{
	QString browsePath;
	if (path == "")
		browsePath = _model->mostRecent();
	else
		browsePath = path;

	QString filter = "Data Sets (*.jasp *.csv *.txt *.sav *.ods)";
	if (_mode == FileEvent::FileSyncData)
		filter = "Data Sets (*.csv *.txt *.sav *.ods)";
	QString finalPath = QFileDialog::getOpenFileName(this, "Open", browsePath, filter);

	FileEvent *event = new FileEvent(this, _mode);

	if (finalPath != "")
	{
		event->setPath(finalPath);
		emit dataSetIORequest(event);
	}
	else
	{
		event->setComplete(false);
	}

	return event;
}

FileEvent *BackstageComputer::browseSave(const QString &path, FileEvent::FileMode mode)
{
	QString caption = "Save";
	QString filter = "JASP Files (*.jasp)";

	QString browsePath = path;
	if (path == "")
		browsePath = _model->mostRecent();

	if (_hasFileName)
		browsePath += QDir::separator() + _fileName;

	if (mode==FileEvent::FileExportResults)
	{
		caption = "Export Result as HTML";
#ifdef QT_DEBUG
		// In debug mode enable pdf export
		filter = "HTML Files (*.html *.pdf)";
#else
		// For future use of pdf export switch to line above
		filter = "HTML Files (*.html)";
#endif
	}
	else if (mode==FileEvent::FileExportData)
	{
		caption = "Export Data as CSV";
		filter = "CSV Files (*.csv *.txt)";
	}
	else if (mode==FileEvent::FileSyncData)
	{
		caption = "Sync Data";
		filter = "Data Files (*.csv *.txt *.sav *.ods)";
	}

	QString finalPath = QFileDialog::getSaveFileName(this, caption, browsePath, filter);

	FileEvent *event = new FileEvent(this, mode);

	if (finalPath != "")
	{
		// force the filename end with .jasp - workaround for linux saving issue
		if (mode == FileEvent::FileSave && !finalPath.endsWith(".jasp", Qt::CaseInsensitive))
			finalPath.append(QString(".jasp"));

		event->setPath(finalPath);

		emit dataSetIORequest(event);
	}
	else
		event->setComplete(false);

	return event;

}

void BackstageComputer::addRecent(const QString &path)
{
	_model->addRecent(path);
}

void BackstageComputer::setFileName(const QString &filename)
{
	_fileName = filename;
	_hasFileName = true;
}

void BackstageComputer::clearFileName()
{
	_hasFileName = false;
}

bool BackstageComputer::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::Show || event->type() == QEvent::WindowActivate)
		_model->refresh();

	return QWidget::eventFilter(object, event);
}

void BackstageComputer::selectionMade(QString path)
{
	if (_mode == FileEvent::FileOpen || _mode == FileEvent::FileSyncData)
		browseOpen(path);
	else
		browseSave(path, _mode);
}

void BackstageComputer::browseSelected()
{
	selectionMade(_model->mostRecent());
}
