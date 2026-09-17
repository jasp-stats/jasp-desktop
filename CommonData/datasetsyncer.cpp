#include "datasetsyncer.h"
#include "dataset.h"
#include "log.h"

#include <QFileInfo>

DataSetSyncer::DataSetSyncer(DataSet * dataSet, QObject * parent)
	: QObject(parent), _dataSet(dataSet)
{
}

DataSetSyncer::~DataSetSyncer()
{
	stopFileSyncing(true);
	stopDatabaseSyncing(true);
	
}

void DataSetSyncer::startFileSyncing(const QString & filePath)
{
	QFileInfo fi(filePath);
	if(!fi.exists())
	{
		Log::log() << "DataSetSyncer::startFileSyncing: File does not exist: " << filePath.toStdString() << std::endl;
		return;
	}

	QString absPath = fi.absoluteFilePath();

	if(_fileWatcher && _fileWatcher->files().contains(absPath))
	{
		_dataSet->setDataFile(absPath.toStdString(), fi.lastModified().toSecsSinceEpoch());
		return;
	}

	if(!_fileWatcher)
	{
		_fileWatcher = new QFileSystemWatcher(this);
		connect(_fileWatcher, &QFileSystemWatcher::fileChanged, this, &DataSetSyncer::fileChanged);
	}
	else if(!_fileWatcher->files().isEmpty())
		_fileWatcher->removePaths(_fileWatcher->files());

	_fileWatcher->addPath(absPath);
	_dataSet->setDataFile(absPath.toStdString(), fi.lastModified().toSecsSinceEpoch());
	_dataSet->setDataFileSynch(true);
}

void DataSetSyncer::stopFileSyncing(bool isExit)
{
	if(_fileWatcher)
	{
		_fileWatcher->removePaths(_fileWatcher->files());
		delete _fileWatcher;
		_fileWatcher = nullptr;
	}

	if(!isExit)
		_dataSet->setDataFileSynch(false);
}

void DataSetSyncer::startDatabaseSyncing(const Json::Value & dbJson, bool syncImmediately)
{
	_databaseJson = dbJson;

	stopDatabaseSyncing();

	if(dbJson == Json::nullValue)
		return;

	_dbInfo = new DatabaseConnectionInfo(dbJson, this);

	connect(_dbInfo, &DatabaseConnectionInfo::synchingIntervalPassed,	this, &DataSetSyncer::databaseSyncIntervalPassed);
	connect(_dbInfo, &DatabaseConnectionInfo::askPassword, [this](QString title, QString message) -> QString {
		return emit askPassword(_dataSet->id(), title, message);
	});
	connect(_dbInfo, &DatabaseConnectionInfo::showYesNo, [this](QString title, QString message) -> bool {
		return emit askYesNo(_dataSet->id(), title, message);
	});
	connect(_dbInfo, &DatabaseConnectionInfo::showWarning, [this](QString title, QString message) {
		emit showWarning(_dataSet->id(), title, message);
	});

	_dataSet->setDatabaseJson(dbJson.toStyledString());

	if(_dbInfo->_interval > 0)
		_dbInfo->startSynching(syncImmediately);
}

void DataSetSyncer::stopDatabaseSyncing(bool isExit)
{
	if(_dbInfo)
	{
		_dbInfo->stopSynching();
		_dbInfo->deleteLater();
		_dbInfo = nullptr;
	}

	if(!isExit)
		_dataSet->setDatabaseJson(Json::nullValue);
}

void DataSetSyncer::syncNow()
{
	if(isDatabaseSyncing())
		databaseSyncIntervalPassed();
	else if(isFileSyncing() && !_fileWatcher->files().isEmpty())
		fileChanged(_fileWatcher->files().first());
	else
		emit askUserForRelink(_dataSet->id());
}

void DataSetSyncer::fileChanged(const QString & path)
{
	Log::log() << "[DataSetSyncer::fileChanged] START: path=" << path.toStdString() << ", dataFileSynch=" << _dataSet->dataFileSynch() << std::endl;

	//Manual edits disable file synching (DataSet::setDataFileSynch(false)); honour that here so a
	//stale file watcher doesn't overwrite the user's edits on the next external modification.
	if(!_dataSet->dataFileSynch())
	{
		Log::log() << "[DataSetSyncer::fileChanged] dataFileSynch is false, returning early" << std::endl;
		return;
	}

	QFileInfo fi(path);
	if(!fi.exists())
	{
		Log::log() << "DataSetSyncer: Synced file no longer exists: " << path.toStdString() << std::endl;
		return;
	}

	long newTimestamp = fi.lastModified().toSecsSinceEpoch();
	if(newTimestamp <= _dataSet->dataFileTimestamp())
	{
		Log::log() << "[DataSetSyncer::fileChanged] newTimestamp=" << newTimestamp << " <= oldTimestamp=" << _dataSet->dataFileTimestamp() << ", returning early" << std::endl;
		return;
	}

	Log::log() << "[DataSetSyncer::fileChanged] newTimestamp=" << newTimestamp << " > oldTimestamp=" << _dataSet->dataFileTimestamp() << ", updating" << std::endl;
	_dataSet->setDataFile(path.toStdString(), newTimestamp);

	if(_isSyncing)
		//QFileSystemWatcher won't re-fire for this modification, so remember we missed it and retry
		//once the in-flight sync completes (setSyncingResult), otherwise the change would be lost.
		_isPendingFileSync = true;
	else
	{
		Log::log() << "[DataSetSyncer::fileChanged] Calling doSync()" << std::endl;
		doSync();
		Log::log() << "[DataSetSyncer::fileChanged] doSync() returned" << std::endl;
	}
}

void DataSetSyncer::databaseSyncIntervalPassed()
{
	doSync();
}

void DataSetSyncer::doSync()
{
	Log::log() << "[DataSetSyncer::doSync] START: _dataSet=" << (_dataSet ? QString::number(_dataSet->id()) : "NULL") << ", _isSyncing=" << _isSyncing << std::endl;

	if(!_dataSet || _isSyncing)
	{
		Log::log() << "[DataSetSyncer::doSync] No dataset or already syncing, returning" << std::endl;
		return;
	}

	_isSyncing = true;
	int id = _dataSet->id();
	Log::log() << "[DataSetSyncer::doSync] Emitting syncingStarted for datasetId=" << id << std::endl;
	emit syncingStarted(id);

  QString locator;
  QString extension;
  QString dbJson;

  Log::log() << "[DataSetSyncer::doSync] Building sync parameters" << std::endl;

  if(_databaseJson != Json::nullValue)
  {
    Log::log() << "[DataSetSyncer::doSync] Database sync detected" << std::endl;
    locator		= QString::fromStdString(_dataSet->dataFilePath());
    extension	= "DATABASE";
    dbJson		= QString::fromStdString(_databaseJson.toStyledString());
  }
  else
  {
    Log::log() << "[DataSetSyncer::doSync] File sync detected" << std::endl;
	locator		= QString::fromStdString(_dataSet->dataFilePath());
    extension	= QFileInfo(locator).suffix();
  }

  Log::log() << "[DataSetSyncer::doSync] Emitting syncRequired: datasetId=" << _dataSet->id() << ", locator=" << locator.toStdString() << ", extension=" << extension.toStdString() << std::endl;
  emit syncRequired(_dataSet, locator, extension, dbJson);
  Log::log() << "[DataSetSyncer::doSync] syncRequired emitted" << std::endl;
}

void DataSetSyncer::setSyncingResult(bool success)
{
	Log::log() << "[DataSetSyncer::setSyncingResult] START: success=" << success << ", _isSyncing=" << _isSyncing << ", _isPendingFileSync=" << _isPendingFileSync << std::endl;

	//The re-entrancy guard is only released on *real* completion (or an explicit abort),
	//not when the request is launched, so overlapping syncs can't start while one is in flight.
	_isSyncing = false;

	Log::log() << "[DataSetSyncer::setSyncingResult] _isSyncing set to false" << std::endl;

	//Pick up any file change that was dropped during the just-finished sync. fileChanged already
	//bumped the dataset timestamp when it was seen, so re-running it would be a no-op; a direct
	//doSync() re-issues the (now-current) file/database state so the change is not lost.
	if(_isPendingFileSync)
	{
		Log::log() << "[DataSetSyncer::setSyncingResult] _isPendingFileSync is true, calling doSync()" << std::endl;
		_isPendingFileSync = false;
		doSync();
	}
	else
	{
		Log::log() << "[DataSetSyncer::setSyncingResult] _isPendingFileSync is false" << std::endl;
	}

	Log::log() << "[DataSetSyncer::setSyncingResult] Emitting syncingFinished: datasetId=" << (_dataSet ? QString::number(_dataSet->id()) : "-1") << ", success=" << success << std::endl;
	emit syncingFinished(_dataSet ? _dataSet->id() : -1, success);
	Log::log() << "[DataSetSyncer::setSyncingResult] END" << std::endl;
}
