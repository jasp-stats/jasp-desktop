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

#include "asyncloader.h"


#include <fstream>
#include <QTimer>
#include <QFileInfo>

#include <boost/bind.hpp>

#include "utilities/qutils.h"
#include "utils.h"
#include "osf/onlinedatamanager.h"
#include "log.h"


using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{ 
	connect(this, &AsyncLoader::beginLoad, this, &AsyncLoader::loadTask);
	connect(this, &AsyncLoader::beginSave, this, &AsyncLoader::saveTask);
}

void AsyncLoader::io(FileEvent *event, DataSetPackage *package)
{

	switch (event->operation())
	{
	case FileEvent::FileOpen:
		emit progress("Loading Data Set", 0);
		emit beginLoad(event, package);
		break;

	case FileEvent::FileSave:
		emit progress("Saving Data Set", 0);
		emit beginSave(event, package);
		break;

	case FileEvent::FileExportResults:
		emit progress("Exporting Result Set", 0);
		emit beginSave(event, package);
		break;

	case FileEvent::FileExportData:
	case FileEvent::FileGenerateData:
		emit progress("Exporting Data Set", 0);
		emit beginSave(event, package);
		break;

	case FileEvent::FileSyncData:
	{
		emit progress("Sync Data Set", 0);
		emit beginLoad(event, package);
		break;
	}

	case FileEvent::FileClose:
		event->setComplete();
		break;
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

	if (event->isOnlineNode())
		QMetaObject::invokeMethod(_odm, "beginDownloadFile", Qt::AutoConnection, Q_ARG(QString, event->path()), Q_ARG(QString, "asyncloader"));
	else
		this->loadPackage("asyncloader");
}

void AsyncLoader::saveTask(FileEvent *event, DataSetPackage *package)
{

	_currentEvent = event;

	QString path = event->path();
	if (event->isOnlineNode())
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

			Utils::sleep(sleepTime);
			delay += sleepTime;
		}

		Exporter *exporter = event->exporter();
		if (exporter)
		{
			exporter->saveDataSet(fq(tempPath), package, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));
		} else {
			throw runtime_error("No Exporter found!");
		}

		if ( ! Utils::renameOverwrite(fq(tempPath), fq(path)))
			throw runtime_error("File '" + fq(path) + "' is being used by another application.");

		if (event->isOnlineNode())
			QMetaObject::invokeMethod(_odm, "beginUploadFile", Qt::AutoConnection, Q_ARG(QString, event->path()), Q_ARG(QString, "asyncloader"), Q_ARG(QString, tq(package->id())), Q_ARG(QString, tq(package->initialMD5())));
		else
			event->setComplete();
	}
	catch (runtime_error e)
	{
		Log::log() << "Runtime Exception in saveTask: " << e.what() << std::endl;

		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
	catch (exception e)
	{
		Log::log() << "Exception in saveTask: " << e.what() << std::endl;

		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
}

void AsyncLoader::progressHandler(string status, int progress)
{
	emit this->progress(QString::fromUtf8(status.c_str(), status.length()), progress);
}

void AsyncLoader::setOnlineDataManager(OnlineDataManager *odm)
{
	if (_odm != nullptr)
	{
		disconnect(_odm, SIGNAL(uploadFileFinished(QString)), this, SLOT(uploadFileFinished(QString)));
		disconnect(_odm, SIGNAL(downloadFileFinished(QString)), this, SLOT(loadPackage(QString)));
	}

	_odm = odm;

	if (_odm != nullptr)
	{
		connect(_odm, SIGNAL(uploadFileFinished(QString)), this, SLOT(uploadFileFinished(QString)));
		connect(_odm, SIGNAL(downloadFileFinished(QString)), this, SLOT(loadPackage(QString)));
	}
}


void AsyncLoader::loadPackage(QString id)
{
	if (id == "asyncloader")
	{
		OnlineDataNode *dataNode = nullptr;

		try
		{
			Log::log()  << "AsyncLoader::loadPackage(" << id.toStdString() << ")" << std::endl;
			string path = fq(_currentEvent->path());
			string extension = "";

			if (_currentEvent->isOnlineNode())
			{
				//Find file extension in the OSF
				extension=".jasp"; //default
				QString qpath(path.c_str());
				int slashPos = qpath.lastIndexOf("/");
				int dotPos = qpath.lastIndexOf('.');
				if (dotPos != -1 && dotPos > slashPos)
				{
					QString ext = qpath.mid(dotPos);
					extension=ext.toStdString();
				}

				dataNode = _odm->getActionDataNode(id);

				if (dataNode != nullptr && dataNode->error())
					throw runtime_error(fq(dataNode->errorMessage()));

				//Generated local path has no extension
				path = fq(_odm->getLocalPath(_currentEvent->path()));
			}

			if (_currentEvent->operation() == FileEvent::FileSyncData)
			{
				_loader.syncPackage(_currentPackage, path, extension, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));
			}
			else
			{
				//loadPackage argument extension determines type
				_loader.loadPackage(_currentPackage, path, extension, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));
			}

			QString calcMD5 = fileChecksum(tq(path), QCryptographicHash::Md5);

			if (dataNode != nullptr)
			{
				if (calcMD5 != dataNode->md5().toLower())
					throw runtime_error("The securtiy check of the downloaded file has failed.\n\nLoading has been cancelled due to an MD5 mismatch.");
			}

			_currentPackage->setInitialMD5(fq(calcMD5));

			if (dataNode != nullptr)
			{
				_currentPackage->setId(fq(dataNode->nodeId()));
				_currentEvent->setPath(dataNode->path());
			}
			else
				_currentPackage->setId(path);

			if (_currentEvent->type() != Utils::FileType::jasp)
			{
				_currentPackage->setDataFilePath(_currentEvent->path().toStdString());
				_currentPackage->setDataFileTimestamp(_currentEvent->isOnlineNode() ? 0 : QFileInfo(_currentEvent->path()).lastModified().toTime_t());
			}
			_currentPackage->setDataFileReadOnly(_currentEvent->isReadOnly());
			_currentEvent->setDataFilePath(QString::fromStdString(_currentPackage->dataFilePath()));
			_currentEvent->setComplete();

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
		}
		catch (runtime_error e)
		{
			Log::log() << "Runtime Exception in loadPackage: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
		catch (exception e)
		{
			Log::log() << "Exception in loadPackage: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
	}
}

QString AsyncLoader::fileChecksum(const QString &fileName, QCryptographicHash::Algorithm hashAlgorithm)
{
	QString hashString = "";
	QFile f(fileName);
	if (f.open(QFile::ReadOnly)) {
		QCryptographicHash hash(hashAlgorithm);
		if (hash.addData(&f)) {
			hashString = (QString)hash.result().toHex();
		}
		f.close();
	}
	return hashString.toLower();
}

void AsyncLoader::uploadFileFinished(QString id)
{
	if (id == "asyncloader")
	{
		OnlineDataNode *dataNode = nullptr;

		try
		{
			string path = fq(_currentEvent->path());

			if (_currentEvent->isOnlineNode())
			{
				dataNode = _odm->getActionDataNode(id);

				if (dataNode->error())
					throw runtime_error(fq(dataNode->errorMessage()));

				path = fq(_odm->getLocalPath(_currentEvent->path()));

				_currentEvent->setPath(dataNode->path());
			}

			_currentPackage->setInitialMD5(fq(fileChecksum(tq(path), QCryptographicHash::Md5)));
			_currentPackage->setId(dataNode != nullptr ? fq(dataNode->nodeId()) : path);

			_currentEvent->setComplete();

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
		}
		catch (runtime_error e)
		{
			Log::log() << "Runtime Exception in uploadFileFinished: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
		catch (exception e)
		{
			Log::log() << "Exception in uploadFileFinished: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
	}
}
