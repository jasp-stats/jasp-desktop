//
// Copyright (C) 2013-2026 University of Amsterdam
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
#include <QThread>

#include <boost/bind.hpp>

#include "qutils.h"
#include "utils.h"
#include "osf/onlinedatamanager.h"
#include "log.h"
#include "utilenums.h"
#include "appinfo.h"
#include "gui/preferencesmodel.h"
#include "data/datasetpackage.h"
#include "databaseinterface.h"
#include "data/exporters/exporter.h"
#include "data/exporters/jaspexporter.h"
#include "utilities/desktopcommunicator.h"

using namespace std;

LoaderException::LoaderException(const std::string & _problemDescription, bool _cancelled)
	: std::runtime_error(_problemDescription), cancelled(_cancelled)
{}

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{ 
	connect(this, &AsyncLoader::beginLoad, this, &AsyncLoader::loadTask, Qt::QueuedConnection);
	connect(this, &AsyncLoader::beginSave, this, &AsyncLoader::saveTask, Qt::QueuedConnection);
}

void AsyncLoader::onSyncRequired(DataSet * dataSet, const QString & locator, const QString & extension, const QString & databaseJson)
{
	Log::log() << "[AsyncLoader::onSyncRequired] START: dataSetId=" << (dataSet ? dataSet->id() : -1) << ", locator=" << locator.toStdString() << ", extension=" << extension.toStdString() << std::endl;

	//Owns the per-dataset sync lifecycle on behalf of the (data) syncer. The DataSet pointer is
	//carried across the queued hand-off so this slot (which runs on our worker thread) never has to
	//read the GUI-owned workspace map. The id is kept for the syncCompleted completion routing.
	//
	//Every exit path releases the syncer's re-entrancy guard (via syncCompleted) so _isSyncing is
	//cleared exactly once for whichever dataset syncs.
	//
	//Note: For database sync, locator can be empty because the database path is in databaseJson.
	//The check below allows empty locator when databaseJson is not empty.
	if((locator.isEmpty() && databaseJson.isEmpty()) || !dataSet)
	{
		Log::log() << "[AsyncLoader::onSyncRequired] EMPTY locator/databaseJson or NULL dataSet, aborting" << std::endl;
		emit syncCompleted(dataSet->id(), false); //Nothing sensible to sync; release the syncer guard.
		return;
	}

	FileEvent * event = new FileEvent(this, FileEvent::FileSyncData);
	event->setDataSet(dataSet);
	event->setPath(locator);
	if(!databaseJson.isEmpty())
	{
		Json::Value db;
		Json::Reader().parse(databaseJson.toStdString(), db);
		event->setDatabase(db);
	}

	Log::log() << "[AsyncLoader::onSyncRequired] Calling io(event)" << std::endl;
	event->starts();
	Log::log() << "[AsyncLoader::onSyncRequired] io(event) returned" << std::endl;
}

void AsyncLoader::io(FileEvent *event)
{
	switch (event->operation())
	{
	case FileEvent::FileSyncData:
	case FileEvent::FileNew:
		emit progress(tr("Loading New Data Set"), 0);
		emit beginLoad(event);
		break;

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

	case FileEvent::FileClose:
		event->setComplete();
		break;
	}
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
		else			throw LoaderException("No Exporter found!");

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
			throw LoaderException("File '" + fq(path) + "' or '" + fq(tempPath) + "' is being used by another application.");

		
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
	catch (LoaderException & e)
	{
		Log::log() << "Loader Exception in saveTask: " << e.what() << std::endl;
		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what(), e.cancelled);
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
		Log::log() << "[AsyncLoader::loadPackage] START: id=" << id.toStdString() << std::endl;
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
					throw LoaderException(fq(dataNode->errorMessage()));

				//Generated local path has no extension
				path = fq(_odm->getLocalPath(_currentEvent->path()));
			}

			if(!_currentEvent->isDatabase())
				extension = _loader.getExtension(path, extension); //Because it might still be ""...
			else
			{
				extension = "DATABASE"; //Lets be clear what this is ;)
				path = _currentEvent->databaseStr();
			}

			DataSetPackage * pkg = DataSetPackage::pkg();

			//For a FileSyncData event the reload targets a specific (possibly non-shown) dataset; keep that
			//reference so the bookkeeping below (and the syncer lifecycle completion) apply to the right one.
			DataSet * syncTargetDataSet = nullptr;

			if (_currentEvent->operation() == FileEvent::FileSyncData)
			{
				Log::log() << "[AsyncLoader::loadPackage] FileSyncData operation detected" << std::endl;
				syncTargetDataSet = _currentEvent->dataSet(); //QPointer; null if the dataset was destroyed meanwhile
				if(!syncTargetDataSet)
				{
					Log::log() << "[AsyncLoader::loadPackage] syncTargetDataSet is NULL after _currentEvent->dataSet()" << std::endl;
					_currentEvent->setComplete(false, "No dataset found for sync");

					//Release the syncer guard exactly once, like every other exit of this branch.
					Log::log() << "[AsyncLoader::loadPackage] Emitting syncCompleted with success=false (no dataset)" << std::endl;
					emit syncCompleted(_currentEvent->dataSetId(), false);
					return;
				}
				Log::log() << "[AsyncLoader::loadPackage] Calling syncPackage for datasetId=" << syncTargetDataSet->id() << std::endl;
				_loader.syncPackage(path, extension, syncTargetDataSet, boost::bind(&AsyncLoader::progressHandler, this, _1));
				Log::log() << "[AsyncLoader::loadPackage] syncPackage returned" << std::endl;
			}
			else
				_loader.loadPackage(path, extension, boost::bind(&AsyncLoader::progressHandler, this, _1));

			//The (non-sync) load above may have added a dataset to the workspace table model. The
			//model was mutated on this (worker) thread, so let the GUI thread know it must refresh
			//its views (dataset tabbuttons etc.) of the new row-count.
			if(!syncTargetDataSet)
				emit dataSetsChanged();

			QString calcMD5 = fileChecksum(tq(path), QCryptographicHash::Md5);

			if (dataNode != nullptr && calcMD5 != dataNode->md5().toLower())
				throw LoaderException("The security check of the downloaded file has failed.\n\nLoading has been cancelled due to an MD5 mismatch.");

			//Timestamp/databaseJson bookkeeping applies to whichever dataset was reloaded (the target for a sync).
			DataSet * bookkeepingDataSet = syncTargetDataSet ? syncTargetDataSet : pkg->dataSet();
			if (bookkeepingDataSet)
			{
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
					QFileInfo fileInfo(_currentEvent->path());
					long timestamp = fileInfo.isFile() ? fileInfo.lastModified().toSecsSinceEpoch() : 0;

					bookkeepingDataSet->setDataFileAndTimeStamp(_currentEvent->path().toStdString(), timestamp);
					bookkeepingDataSet->setDatabaseJson(_currentEvent->database());
				}

				pkg->setFileReadOnly(_currentEvent->isReadOnly());
				_currentEvent->setDataFilePath(QString::fromStdString(bookkeepingDataSet->dataFilePath()));
			}

			//Sync completion is delivered through AsyncLoader::syncCompleted (slot on the GUI thread via a
			//QueuedConnection), which covers success here and failure in the catch blocks below, so the
			//syncer's re-entrancy guard (_isSyncing) is released exactly once.
			if(syncTargetDataSet)
			{
				Log::log() << "[AsyncLoader::loadPackage] Emitting syncCompleted for datasetId=" << _currentEvent->dataSetId() << ", success=" << _currentEvent->isSuccessful() << std::endl;
				emit syncCompleted(_currentEvent->dataSetId(), _currentEvent->isSuccessful());
			}
			else
			{
				Log::log() << "[AsyncLoader::loadPackage] syncTargetDataSet is NULL, NOT emitting syncCompleted" << std::endl;
			}

			_currentEvent->setComplete();

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
		}
		catch (LoaderException & e)
		{
			Log::log() << "Loader Exception in loadPackage: " << e.what() << std::endl;

			//For a sync we only reload one existing dataset; a (transient) failure must not destroy the
			//whole workspace. Release the syncer's re-entrancy guard via syncCompleted so later syncs can
			//still run; the dataset stays alive on the GUI thread.
			if (_currentEvent->operation() == FileEvent::FileSyncData)
			{
				Log::log() << "[AsyncLoader::loadPackage] Emitting syncCompleted for datasetId=" << _currentEvent->dataSetId() << ", success=false (exception)" << std::endl;
				emit syncCompleted(_currentEvent->dataSetId(), false);
			}
			else
				DataSetPackage::pkg()->deleteWorkspace(false); //Make sure we dont keep failed stuff in memory

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what(), e.cancelled);
		}
		catch (exception & e)
		{
			Log::log() << "Exception in loadPackage: " << e.what() << std::endl;

			if (_currentEvent->operation() == FileEvent::FileSyncData)
			{
				Log::log() << "[AsyncLoader::loadPackage] Emitting syncCompleted for datasetId=" << _currentEvent->dataSetId() << ", success=false (exception)" << std::endl;
				emit syncCompleted(_currentEvent->dataSetId(), false);
			}
			else
				DataSetPackage::pkg()->deleteWorkspace(true); //Make sure we dont keep failed stuff in memory

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what());
		}

		JASPTIMER_STOP(AsyncLoader::loadPackage);
		Log::log() << "[AsyncLoader::loadPackage] END" << std::endl;
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
					throw LoaderException(fq(dataNode->errorMessage()));

				path = fq(_odm->getLocalPath(_currentEvent->path()));

				_currentEvent->setPath(dataNode->path());
			}

			DataSetPackage::pkg()->setInitialMD5(fq(fileChecksum(tq(path), QCryptographicHash::Md5)));
			DataSetPackage::pkg()->setId(dataNode != nullptr ? fq(dataNode->nodeId()) : path);

			_currentEvent->setComplete();

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
		}
		catch (LoaderException & e)
		{
			Log::log() << "Loader Exception in uploadFileFinished: " << e.what() << std::endl;

			if (dataNode != nullptr)
				_odm->deleteActionDataNode(id);
			_currentEvent->setComplete(false, e.what(), e.cancelled);
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
