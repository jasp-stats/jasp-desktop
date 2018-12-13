//
// Copyright (C) 2018 University of Amsterdam
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
#include <QDir>

BackstageComputer::BackstageComputer(QObject *parent): BackstagePage(parent)
{
	setListModel(new ComputerListModel(this));
}

FileEvent *BackstageComputer::browseOpen(const QString &path)
{
	QString browsePath;
	if (path == "")
		browsePath = _computerListModel->getMostRecent();
	else
		browsePath = path;

	QString filter = "Data Sets (*.jasp *.csv *.txt *.sav *.ods)";
	if (_mode == FileEvent::FileSyncData)
		filter = "Data Sets (*.csv *.txt *.sav *.ods)";

	QString finalPath = "???";

	std::cerr << "finalPath not being set cause we aint got filedialog: QFileDialog::getOpenFileName(this, \"Open\", browsePath, filter);" << std::endl;

	throw std::runtime_error("AAARGH");

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
		browsePath = _computerListModel->getMostRecent();

	if (_hasFileName)
		browsePath += QDir::separator() + _fileName;

	switch(mode)
	{
	case FileEvent::FileExportResults:
		caption = "Export Result as HTML";
#ifdef JASP_DEBUG
		// In debug mode enable pdf export
		filter = "HTML Files (*.html *.pdf)";
#else
		// For future use of pdf export switch to line above
		filter = "HTML Files (*.html)";
#endif
		break;

	case FileEvent::FileGenerateData:
	case FileEvent::FileExportData:
		caption	= "Export Data as CSV";
		filter	= "CSV Files (*.csv *.txt)";
		break;

	case FileEvent::FileSyncData:
		caption = "Sync Data";
		filter = "Data Files (*.csv *.txt *.sav *.ods)";
		break;

	case FileEvent::FileSave:
		break;

	default:
		throw std::runtime_error("Wrong FileEvent type for saving!");
	}

	QString finalPath = "???";

	std::cerr << "QFileDialog::getSaveFileName(this, caption, browsePath, filter);" << std::endl;;

	throw std::runtime_error("AAARG!");

	FileEvent *event = new FileEvent(this, mode);

	if (finalPath != "")
	{
		// Default file extensions if not specified
		if		(mode == FileEvent::FileSave			&&	!finalPath.endsWith(".jasp", Qt::CaseInsensitive))
			finalPath.append(QString(".jasp"));
		else if	(mode == FileEvent::FileExportResults	&&	(!finalPath.endsWith(".html", Qt::CaseInsensitive) &&
															!finalPath.endsWith(".pdf", Qt::CaseInsensitive)))
			finalPath.append(QString(".html"));
		else if	(mode == FileEvent::FileExportData		&&	(!finalPath.endsWith(".csv", Qt::CaseInsensitive) &&
															!finalPath.endsWith(".txt", Qt::CaseInsensitive)))

			finalPath.append(QString(".csv"));

		event->setPath(finalPath);
		emit dataSetIORequest(event);
	}
	else
		event->setComplete(false);

	return event;

}

void BackstageComputer::addRecentFolder(const QString &path)
{
	_computerListModel->addRecentFolder(path);
}

void BackstageComputer::setFileName(const QString &filename)
{
	_fileName = filename;
	_hasFileName = true;
}

void BackstageComputer::clearFileName()
{
	_fileName = "";
	_hasFileName = false;
}


//Slots
void BackstageComputer::browsePath(QString path)
{
	if (_mode == FileEvent::FileOpen || _mode == FileEvent::FileSyncData)
		browseOpen(path);
	else
		browseSave(path, _mode);
}

void BackstageComputer::browseMostRecent()
{
	QString mostrecent = _computerListModel->getMostRecent();
	browsePath(mostrecent);
}


void BackstageComputer::setListModel(ComputerListModel * listModel)
{
	if (_computerListModel == listModel)
		return;

	_computerListModel = listModel;
	emit listModelChanged(_computerListModel);
}
