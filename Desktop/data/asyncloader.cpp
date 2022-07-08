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
	connect(this, &AsyncLoader::beginLoad, this, &AsyncLoader::loadTask, Qt::QueuedConnection);
	connect(this, &AsyncLoader::beginSave, this, &AsyncLoader::saveTask, Qt::QueuedConnection);
}

void AsyncLoader::io(FileEvent *event)
{

	switch (event->operation())
	{
	case FileEvent::FileOpen:
		emit progress(tr("Loading Data Set"), 0);
		emit beginLoad(event);
		break;

	case FileEvent::FileSave:
		emit progress(tr("Saving Data Set"), 0);
		emit beginSave(event);
		break;

	case FileEvent::FileExportResults:
		emit progress(tr("Exporting Result Set"), 0);
		emit beginSave(event);
		break;

	case FileEvent::FileExportData:
	case FileEvent::FileGenerateData:
		emit progress(tr("Exporting Data Set"), 0);
		emit beginSave(event);
		break;

	case FileEvent::FileSyncData:
	{
		emit progress(tr("Sync Data Set"), 0);
		emit beginLoad(event);
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

void AsyncLoader::loadTask(FileEvent *event)
{
	_currentEvent = event;

	if (event->isOnlineNode())
		QMetaObject::invokeMethod(_odm, "beginDownloadFile", Qt::AutoConnection, Q_ARG(QString, event->path()), Q_ARG(QString, "asyncloader"));
	else
		this->loadPackage("asyncloader");
}

void AsyncLoader::saveTask(FileEvent *event)
{

	_currentEvent = event;

	QString path = event->path();
	if (event->isOnlineNode())
		path = _odm->getLocalPath(path);

	QString tempPath = path + QString(".tmp");

	try
	{
		int	maxSleepTime	= 2000,
			sleepTime		= 100,
			delay			= 0;
		
		while (DataSetPackage::pkg()->isReady() == false)
		{
			if (delay > maxSleepTime)
				break;

			Utils::sleep(sleepTime);
			delay += sleepTime;
		}

		Exporter *exporter = event->exporter();
		if (exporter)	exporter->saveDataSet(fq(tempPath), boost::bind(&AsyncLoader::progressHandler, this, _1));
		else			throw runtime_error("No Exporter found!");

		int attempts = 1;

#ifdef _WIN32
		if(event->type() == Utils::FileType::pdf)
			attempts = 5;
#endif		
		bool renameSucceeded = false;
		
		while(
			  !		(renameSucceeded = Utils::renameOverwrite(fq(tempPath), fq(path))) 
			  &&	--attempts > 0)
		{
			Utils::sleep(sleepTime); //Yes Bruno, I can hear you laugh. But it seems webengine is not releasing the pdf.tmp file quickly enough on Windows... See: https://github.com/jasp-stats/jasp-test-release/issues/957
		}
		
		if(!renameSucceeded)
			throw runtime_error("File '" + fq(path) + "' or '" + fq(tempPath) + "' is being used by another application.");

		
		if (event->isOnlineNode())	// Not really sure why we would need to do the invokeMethod here?
			QMetaObject::invokeMethod(
						_odm, "beginUploadFile", Qt::AutoConnection,
						Q_ARG(QString, event->path()),
						Q_ARG(QString, "asyncloader"),
						Q_ARG(QString, tq(DataSetPackage::pkg()->id())),
						Q_ARG(QString, tq(DataSetPackage::pkg()->initialMD5())));
		else
			event->setComplete();
	}
	catch (runtime_error & e)
	{
		Log::log() << "Runtime Exception in saveTask: " << e.what() << std::endl;

		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
	catch (exception & e)
	{
		Log::log() << "Exception in saveTask: " << e.what() << std::endl;

		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
}

void AsyncLoader::progressHandler(int progress)
{
	emit this->progress(_currentEvent->getProgressMsg(), progress);
}

void AsyncLoader::setOnlineDataManager(OnlineDataManager *odm)
{
	if (_odm != nullptr)
	{
		disconnect(_odm, QOverload<QString>::of(&OnlineDataManager::uploadFileFinished),	this, &AsyncLoader::uploadFileFinished);
		disconnect(_odm, QOverload<QString>::of(&OnlineDataManager::downloadFileFinished),	this, &AsyncLoader::loadPackage);
	}

	_odm = odm;

	if (_odm != nullptr)
	{
		connect(_odm, QOverload<QString>::of(&OnlineDataManager::uploadFileFinished),	this, &AsyncLoader::uploadFileFinished, Qt::QueuedConnection);
		connect(_odm, QOverload<QString>::of(&OnlineDataManager::downloadFileFinished), this, &AsyncLoader::loadPackage,		Qt::QueuedConnection);
	}
}


void AsyncLoader::loadPackage(QString id)
{
	if (id == "asyncloader")
	{
		OnlineDataNode *dataNode = nullptr;

		try
		{
			JASPTIMER_RESUME(AsyncLoader::loadPackage);
			Log::log()  << "AsyncLoader::loadPackage(" << id.toStdString() << ")" << std::endl;
			string path = fq(_currentEvent->path());
			string extension = "";

			if (_currentEvent->isOnlineNode()) //Find file extension in the OSF
			{
						extension	= ".jasp"; //default
				QString	qpath		= path.c_str();
				int		slashPos	= qpath.lastIndexOf("/"),
						dotPos		= qpath.lastIndexOf('.');

				if (dotPos != -1 && dotPos > slashPos)
					extension = qpath.mid(dotPos).toStdString();

				dataNode = _odm->getActionDataNode(id);

				if (dataNode != nullptr && dataNode->error())
					throw runtime_error(fq(dataNode->errorMessage()));

				//Generated local path has no extension
				path = fq(_odm->getLocalPath(_currentEvent->path()));
			}

			extension = _loader.getExtension(path, extension); //Because it might still be ""...

			if (_currentEvent->operation() == FileEvent::FileSyncData)
					_loader.syncPackage(path, extension, boost::bind(&AsyncLoader::progressHandler, this, _1));
			else	_loader.loadPackage(path, extension, boost::bind(&AsyncLoader::progressHandler, this, _1));

			QString calcMD5 = fileChecksum(tq(path), QCryptographicHash::Md5);

			if (dataNode != nullptr && calcMD5 != dataNode->md5().toLower())
				throw runtime_error("The security check of the downloaded file has failed.\n\nLoading has been cancelled due to an MD5 mismatch.");

			DataSetPackage * pkg = DataSetPackage::pkg();

			pkg->setInitialMD5(fq(calcMD5));

			if (dataNode != nullptr)
			{
				pkg->setId(fq(dataNode->nodeId()));
				_currentEvent->setPath(dataNode->path());
			}
			else
				pkg->setId(path);

			if (_currentEvent->type() != Utils::FileType::jasp)
			{
				pkg->setDataFilePath(_currentEvent->path().toStdString());
                pkg->setDataFileTimestamp(_currentEvent->isOnlineNode() ? 0 : QFileInfo(_currentEvent->path()).lastModified().toSecsSinceEpoch());
			}
			pkg->setDataFileReadOnly(_currentEvent->isReadOnly());
			_currentEvent->setDataFilePath(QString::fromStdString(pkg->dataFilePath()));
			_currentEvent->setComplete();

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
		}
		catch (runtime_error & e)
		{
			Log::log() << "Runtime Exception in loadPackage: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
		catch (exception & e)
		{
			Log::log() << "Exception in loadPackage: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}

		JASPTIMER_STOP(AsyncLoader::loadPackage);
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

			DataSetPackage::pkg()->setInitialMD5(fq(fileChecksum(tq(path), QCryptographicHash::Md5)));
			DataSetPackage::pkg()->setId(dataNode != nullptr ? fq(dataNode->nodeId()) : path);

			_currentEvent->setComplete();

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
		}
		catch (runtime_error & e)
		{
			Log::log() << "Runtime Exception in uploadFileFinished: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
		catch (exception & e)
		{
			Log::log() << "Exception in uploadFileFinished: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}
	}
}
