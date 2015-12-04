//
// Copyright (C) 2013-2015 University of Amsterdam
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

#include "asyncloader.h"

#include "exporters/jaspexporter.h"
#include "exporters/csvexporter.h"

#include <iostream>
#include <fstream>
#include <QTimer>
#include <QFileInfo>

#include <boost/bind.hpp>

#include "qutils.h"
#include "utils.h"
#include "onlinedatamanager.h"

using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{ 
	this->moveToThread(&_thread);

	connect(this, SIGNAL(beginLoad(FileEvent*, DataSetPackage*)), this, SLOT(loadTask(FileEvent*, DataSetPackage*)));
	connect(this, SIGNAL(beginSave(FileEvent*, DataSetPackage*)), this, SLOT(saveTask(FileEvent*, DataSetPackage*)));

	_thread.start();
}

void AsyncLoader::io(FileEvent *event, DataSetPackage *package)
{
	if (event->operation() == FileEvent::FileOpen)
	{
		emit progress("Loading Data Set", 0);
		emit beginLoad(event, package);
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		emit progress("Saving Data Set", 0);
		emit beginSave(event, package);
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		event->setComplete();
	}
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void AsyncLoader::loadTask(FileEvent *event, DataSetPackage *package)
{
	_currentEvent = event;
	_currentPackage = package;

	if (event->IsOnlineNode())
		QMetaObject::invokeMethod(_odm, "beginDownloadFile", Qt::AutoConnection, Q_ARG(QString, event->path()), Q_ARG(QString, "asyncloader"));
	else
		this->loadPackage("asyncloader");
}

void AsyncLoader::saveTask(FileEvent *event, DataSetPackage *package)
{

	_currentEvent = event;

	QString path = event->path();
	if (event->IsOnlineNode())
		path = _odm->getLocalPath(path);

	QString tempPath = path + QString(".tmp");

	try
	{
		int maxSleepTime = 2000;
		int sleepTime = 100;
		int delay = 0;
		while (package->isReady() == false)
		{
			if (delay > maxSleepTime)
				break;

			sleep(sleepTime);
			delay += sleepTime;
		}

		JASPExporter::saveDataSet(fq(tempPath), package, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		if ( ! Utils::renameOverwrite(fq(tempPath), fq(path)))
			throw runtime_error("File '" + fq(path) + "' is being used by another application.");

		if (event->IsOnlineNode())
			QMetaObject::invokeMethod(_odm, "beginUploadFile", Qt::AutoConnection, Q_ARG(QString, event->path()), Q_ARG(QString, "asyncloader"));
		else
			event->setComplete();
	}
	catch (runtime_error e)
	{
		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
	catch (exception e)
	{
		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
}

void AsyncLoader::progressHandler(string status, int progress)
{
	emit this->progress(QString::fromUtf8(status.c_str(), status.length()), progress);
}



void AsyncLoader::sleep(int ms)
{

#ifdef __WIN32__
	Sleep(uint(ms));
#else
	struct timespec ts = { ms / 1000, (ms % 1000) * 1000 * 1000 };
	nanosleep(&ts, NULL);
#endif
}

void AsyncLoader::setOnlineDataManager(OnlineDataManager *odm)
{
	if (_odm != NULL)
	{
		disconnect(_odm, SIGNAL(uploadFileFinished(QString)), this, SLOT(uploadFileFinished(QString)));
		disconnect(_odm, SIGNAL(downloadFileFinished(QString)), this, SLOT(loadPackage(QString)));
		disconnect(_odm, SIGNAL(error(QString, QString)), this, SLOT(errorFlagged(QString, QString)));
	}

	_odm = odm;

	if (_odm != NULL)
	{
		connect(_odm, SIGNAL(uploadFileFinished(QString)), this, SLOT(uploadFileFinished(QString)));
		connect(_odm, SIGNAL(downloadFileFinished(QString)), this, SLOT(loadPackage(QString)));
		connect(_odm, SIGNAL(error(QString, QString)), this, SLOT(errorFlagged(QString, QString)));
	}
}

void AsyncLoader::errorFlagged(QString msg, QString id)
{
	if (id != "asyncloader")
		return;

	_currentEvent->setComplete(false, msg);
}

void AsyncLoader::loadPackage(QString id)
{
	if (id != "asyncloader")
		return;

	try
	{
		string path = fq(_currentEvent->path());
		if (_currentEvent->IsOnlineNode())
			path = fq(_odm->getLocalPath(_currentEvent->path()));

		_loader.loadPackage(_currentPackage, path, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));
		_currentEvent->setComplete();
	}
	catch (runtime_error e)
	{
		_currentEvent->setComplete(false, e.what());
	}
	catch (exception e)
	{
		_currentEvent->setComplete(false, e.what());
	}
}

void AsyncLoader::uploadFileFinished(QString id)
{
	_currentEvent->setComplete();
}
