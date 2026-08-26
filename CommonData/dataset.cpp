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
#include "log.h"
#include <cassert>
#include "timers.h"
#include "qutils.h"
#include "dataset.h"
#include "appinfo.h"
#include "workspace.h"
#include "dataenums.h"
#include "columnencoder.h"
#include "jsonutilities.h"
#include "databaseinterface.h"
#include "undostack.h"

stringset DataSet::_defaultEmptyvalues;

DataSet::DataSet(Workspace * workspace, int id)
	: DataSetBaseNode(dataSetBaseNodeType::dataSet, workspace),
	  _workspace(workspace)
{
	Log::log() << "DataSet::DataSet(id=" << id << ")" << std::endl;

	_encoder = new ColumnEncoder();
	_syncer  = new DataSetSyncer(this);
	_emptyValues	= new EmptyValues(nullptr);
	connect(_emptyValues,	&EmptyValues::emptyValuesChanged,	this,		&DataSet::emptyValuesChanged			);
	connect(this,			&DataSet::emptyValuesChanged,		_workspace, &Workspace::emptyValuesChanged			);
	
	if(id == -1)			dbCreate();
	else if(id > 0)			dbLoad(id);
	
	_undoStack = new UndoStack(this);
	
	connect(this,			&DataSet::datasetChanged,			this,		&DataSet::handleDataSetChanged			);
	
	connect(this,			&DataSet::showYesNo,				_workspace, &Workspace::showYesNo					);
	connect(this,			&DataSet::askPassword,				_workspace, &Workspace::askPassword					);
	connect(this,			&DataSet::showWarning,				_workspace, &Workspace::showWarning					);
	connect(this,			&DataSet::manualEditMade,			_workspace, &Workspace::manualEditMade				);
	connect(this,			&DataSet::datasetChanged,			_workspace, &Workspace::datasetChanged				);
	connect(this,			&DataSet::labelsReordered,			_workspace, &Workspace::labelsReordered				);

	connect(this,			&DataSet::somethingModified,		_workspace, &Workspace::enableModified				);
	connect(this,			&DataSet::sendFilter,				_workspace, &Workspace::sendFilter					);
	connect(this,			&DataSet::sendFilterByName,			_workspace, &Workspace::sendFilterByName			);
	connect(this,			&DataSet::filtersCountChanged,		_workspace, &Workspace::filtersCountChanged			);
	connect(this,			&DataSet::refreshAllAnalyses,		_workspace, &Workspace::refreshAllAnalyses			);
	connect(this,			&DataSet::refreshAllCompCols,		_workspace, &Workspace::refreshAllCompCols			);
	
	connect(_workspace,		&Workspace::filterByNameDone,		this,		&DataSet::filterByNameDone				);

	_description = fq(tr("Originally created empty by %1 on %2").arg(tq(AppInfo::getShortDesc())).arg(tq(Utils::currentDateTime())));

	connect(_syncer, &DataSetSyncer::askPassword,  this, [this](int, QString title, QString msg) -> QString { return emit askPassword(title, msg); });
	connect(_syncer, &DataSetSyncer::askYesNo,     this, [this](int, QString title, QString msg) -> bool   { return emit showYesNo(title, msg); });
	connect(_syncer, &DataSetSyncer::showWarning,  this, [this](int, QString title, QString msg)          { emit showWarning(title, msg); });
	connect(_syncer, &DataSetSyncer::syncRequired, this, &DataSet::syncRequired);
}

DataSet::~DataSet()
{
	JASPTIMER_SCOPE(DataSet::~DataSet);

	//If this dataset's encoder is the one currently active (shown), make sure the indirection doesn't
	//keep pointing at it once it's freed. Otherwise later encode/decode would dereference freed memory.
	if(ColumnEncoder::currentEncoder() == _encoder)
		ColumnEncoder::setCurrentEncoder(nullptr);

	delete _syncer;
	_syncer = nullptr;
	delete _encoder;
	_encoder = nullptr;

	for(Column * col : _columns)
		unregisterNode(col);

	_columns.clear();
	
	delete _emptyValues;
	
	for(Filter * f : _filters)
		unregisterNode(f);
		
	_emptyValues	= nullptr;
	_defaultFilter	= nullptr;
}

void DataSet::deleteShownFilter()
{
	if(_shownFilter != _defaultFilter)
		removeFilter(_shownFilter);
}

void DataSet::addFilter()
{
	std::string filterName;
	
	int filterId = 0;
	do
	{
		filterName = fq(tr("Filter %1").arg(filterId++));
	}
	while(filterExists(filterName));
	
	showFilter(createFilter(filterName));
}

void DataSet::showFilter(Filter * f)
{
	if(f->data() != this)
		return;
	
	_shownFilter = f;
	emit shownFilterChanged(this);
	refresh();	
}

Filter * DataSet::showFilter(const std::string &filterName)
{
	if(filterName == "")
	{
		_shownFilter = nullptr;
		return nullptr;
	}
	
	Filter * found = filter(filterName);;
	
	try
	{
		if(!found)
			found = new Filter(this, filterName, false);
	}
	catch(...){}
	
	if(!found)
		found = defaultFilter();
	
	if(found && found != _shownFilter)
		showFilter(found);
	
	return found;
}
	

Filter * DataSet::showFilter(const QString &filterName)
{
	return showFilter(fq(filterName));
}

QString DataSet::name() const
{
	return tq(db().dataSetName(id()));
}

QString DataSet::title() const
{
	return _title.empty() ? name().replace("_", " ") : tq(_title);
}


Filter * DataSet::filter(const std::string &name)
{
	for(Filter * f : _filters)
		if(f->name() == name)
			return f;
	
	return _defaultFilter && _defaultFilter->name() == name ? _defaultFilter : nullptr;
}

Filter *DataSet::filter(int id)
{
	for(Filter * f : _filters)
		if(f->id() == id)
			return f;
	
	return _defaultFilter && _defaultFilter->id() == id ? _defaultFilter : nullptr;
}

void DataSet::registerFilter(Filter *f)
{
	_filters.push_back(f);
	emit filtersCountChanged();
}

void DataSet::removeFilter(Filter *f)
{
	if(!f || f == _defaultFilter)
		return;

	const int removedId = f->id();

	f->dbDelete();

	//Computed datasets that used this filter as their input would otherwise keep a dangling
	//defaultInputFilterId. Clear it and surface an error so the user knows why the computed dataset
	//can no longer be produced.
	if(removedId > 0 && _workspace)
		for(DataSet * ds : _workspace->dataSets())
			if(ds != this && ds->isComputed() && ds->defaultInputFilterId() == removedId)
			{
				ds->setDefaultInputFilterId(-1);
				ds->setError("The filter used as input for this computed dataset was removed.");
			}

	//If the removed filter is the shown one, pick a surviving replacement so _shownFilter never dangles.
	bool wasShown = _shownFilter == f;
	size_t indexWas = 0;

	Filters newList;
	for(size_t i=0; i<_filters.size(); i++)
		if(_filters[i] != f)
			newList.push_back(_filters[i]);
		else
			indexWas = i;

	_filters = newList;

	emit filterRemoved(f);
	emit filtersCountChanged();

	if(wasShown)
	{
		_shownFilter = _filters.empty() ? _defaultFilter : _filters[std::min(indexWas, _filters.size() - 1)];
		emit shownFilterChanged(this);
		incRevision();
		refresh();
	}

	delete f;
}

void DataSet::dbDelete()
{
	JASPTIMER_SCOPE(DataSet::dbDelete);
	
	assert(_dataSetId != -1);
	
	for(Filter * f : _filters)
	{
		f->dbDelete();
		delete f;
	}
	
	_filters.clear();
	
	
	for(Column * c : _columns)
	{
		c->dbDelete();
		delete c;
	}
	
	_columns.clear();
	_shownColumn	= nullptr; //children are freed above; don't leave a dangling reference

	db().dataSetDelete(_dataSetId);
	
	_dataSetId = -1;
}

void DataSet::beginBatchedToDB()
{
	if(_writeBatchedToDBDepth == 0)
		_changedDuringBatch = {};
	
	_writeBatchedToDBDepth++;
}

void DataSet::endBatchedToDB(std::function<void(float)> progressCallback, Columns columns)
{
	if(columns.size() == 0)
		columns = _columns;

	assert(columns.size() != _columns.size() || _writeBatchedToDBDepth);

	if(_writeBatchedToDBDepth > 0)
		_writeBatchedToDBDepth--;
	
	if(_writeBatchedToDBDepth == 0)
	{
		if(columns.size())
			db().dataSetBatchedValuesUpdate(this, columns, [&progressCallback](float f){ progressCallback(0.75 + (f * 0.25));});
		else
			progressCallback(1);

		//Column names/types have (just) been (re)loaded into this DataSet, so keep our own encoder in
		//sync; when this dataset is the shown/current one it is what the encoder indirection points at.
		_encoder->setCurrentNames(getColumnNames());

		incRevision(); //Should trigger reload at engine end
	}
}

int DataSet::getColumnIndex(const std::string & name) const 
{
	for(size_t i=0; i<_columns.size(); i++)
		if(_columns[i]->name() == name)
			return i;
	return -1;
}

int DataSet::columnIndex(const Column * col) const
{
	for(size_t i=0; i<_columns.size(); i++)
		if(_columns[i] == col)
			return i;
	return -1;
}

void DataSet::columnsReorder(stringvec order)
{
	//Perhaps the new order is derived from a synched datafile, which lacks any computed columns.
	stringset	compCols,
				orderSet(order.begin(), order.end()),
				colSet;
	
	for(size_t i=0; i<_columns.size(); i++)
	{
		Column * col = _columns[i];
		
		if(col->isComputed())
		{
			if(!orderSet.count(col->name()))
			{
				order.insert(order.begin() + i, col->name()); //Put the computed column right in the data where it used to be
				orderSet.insert(col->name());
			}
			
			compCols.insert(col->name());
		}
	}
	
	assert(order.size() == _columns.size());
	assert(order.size() == orderSet.size());
	
	std::map<std::string, Column*> nameColMap;
	
	for(Column * col : _columns)
	{
		assert(col->name() != "");
		nameColMap[col->name()] = col;
		colSet.insert(col->name());
	}
	
	assert(colSet == orderSet);
	
	for(size_t i=0; i<_columns.size(); i++)
	{
		_columns[i] =  nameColMap[order[i]];
		_columns[i] -> setIndex(i);
	}
	
	incRevision();
	
	refresh();
}

void DataSet::columnRefreshed(Column *column)
{
	int idx = columnIndex(column);
	emit dataChanged(index(0, idx), index(qMax(rowCount() - 1, 0), idx), roleNames().keys());
}

Column *DataSet::column(const std::string &name)
{
	for(Column * column : _columns)
		if(column->name() == name)
			return column;

	return nullptr;
}

Column *DataSet::column(const QString &name)
{
	return column(fq(name));
}

Column *DataSet::column(int index)
{
	if(index < 0 || index >= _columns.size())
		return nullptr;

	return _columns[index];
}


void DataSet::removeColumn(size_t index)
{
	assert(_dataSetId > 0);

	beginRemoveColumns(QModelIndex(), index, index);
	Column * removeMe = _columns[index];
	_columns.erase(_columns.begin() + index);

	removeMe->dbDelete();
	delete removeMe;
	
	endRemoveColumns();

	incRevision();
}

void DataSet::removeColumn(const std::string & name)
{
	assert(_dataSetId > 0);
	Column * col = column(name);
	
	if(col)
		removeColumn(columnIndex(col));
}

void DataSet::insertColumns(size_t index, size_t count,	bool alterDataSetTable)
{

	assert(_dataSetId > 0);

	beginInsertColumns(QModelIndex(), index, index + count);
	
	intvec colIds = db().columnsInsert(_dataSetId, count, index, "", columnType::unknown, alterDataSetTable);
	
	for(int c = 0; c<colIds.size(); c++)
	{
		Column * newColumn = new Column(this, colIds[c]);

		_columns.insert(_columns.begin()+index+c, newColumn);

		newColumn->setRowCount(_rowCount);
	}
	
	endInsertColumns();

	incRevision();
}

void DataSet::insertColumn(size_t index, bool alterDataSetTable)
{
	insertColumns(index, 1, alterDataSetTable);
}


QString DataSet::insertColumnSpecial(int columnIndex, const QMap<QString, QVariant>& props)
{
	columnIndex = std::min(std::max(0, columnIndex), columnCount());

	insertColumn(columnIndex);
	
	Column * col = column(columnIndex);

	col->setName(				props.contains("name")			? fq(props["name"].toString())					: freeNewColumnName(columnIndex)	);
	col->setDefaultValues(		props.contains("type")			? columnType(props["type"].toInt())				: columnType::scale					);
	col->setCodeType(			props.contains("computed")		? computedColumnType(props["computed"].toInt())	: computedColumnType::notComputed	);
	col->setComputeFilter(fq(	props.contains("computeFilter")	? props["computeFilter"].toString()				: ""								));

	incRevision();
	
	emit datasetChanged(_dataSetId, tq(stringvec{col->name()}), {}, {}, false, true);

	_encoder->setCurrentNames(	getColumnTypesMap());
	
	if(col->codeType() == computedColumnType::constructorCode || col->codeType() == computedColumnType::rCode)
		setShownColumn(col);
	
	refresh();

	return tq(col->name());
}

Column * DataSet::createColumn(const std::string & name, columnType columnType)
{

	if(getColumnIndex(name) >= 0)
		return nullptr;

	beginInsertColumns(QModelIndex(), columnCount(), columnCount());
	
	Column * col = new Column(this, db().columnInsert(_dataSetId, -1, name));
	col->setName(name);
	col->setDefaultValues(columnType, false);
	_columns.push_back(col);
	endInsertColumns();

	incRevision();
	
	refresh();
	emit manualEditMade();

	return col;
}


size_t DataSet::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	if(columnIndex >= columnCount())
		return 0;

	return _columns[columnIndex]->getMaximumWidthInCharactersIncludingShadow();
}

stringvec DataSet::getColumnNames()
{
	stringvec names;

	for(Column * col : _columns)
		names.push_back(col->name());

	return names;
}


std::map<std::string,columnType> DataSet::getColumnTypesMap()
{
	std::map<std::string,columnType> theMap;

	for(const Column * col : columns())
		theMap[col->name()] = col->type();

	return theMap;
}

void DataSet::setupEncoderPrefix()
{
	//Make the encoder prefix globally unique (carries the dataset id) so ALL datasets loaded into the
	//engine can coexist without encoded-name collisions. Must run after the id has been finalized
	//(dbCreate/dbLoad), which is why it is called at the end of those, not in the constructor.
	_encoder->_encodePrefix = "JASPColumn_" + std::to_string(_dataSetId) + "_";
	_encoder->setCurrentNames(getColumnTypesMap()); //regenerate all encoded names with the new prefix
}

void DataSet::setDataFileAndTimeStamp(const std::string &dataFilePath, long timestamp)
{
	bool isChange		= _dataFilePath	!= dataFilePath || _dataFileTimestamp	!= timestamp;
	_dataFileTimestamp	= timestamp;		
	_dataFilePath		= dataFilePath;
	if(isChange) dbUpdate(); 
	
	if(isChange)
	{
		emit dataFileChanged();
		emit dataTimestampChanged();
	}
}

void DataSet::setDataFile(const std::string &dataFilePath)	
{ 
	bool isChange	= _dataFilePath	!= dataFilePath;
	_dataFilePath	= dataFilePath;
	if(isChange) dbUpdate(); 
	
	if(isChange)
		emit dataFileChanged();
}

void DataSet::setDataTimestamp(long timestamp)						
{ 
	bool isChange		= _dataFileTimestamp	!= timestamp;
	_dataFileTimestamp	= timestamp;		
	if(isChange) dbUpdate(); 
	
	if(isChange)
		emit dataTimestampChanged();
}

void DataSet::setDatabaseJson(const Json::Value & databaseJson)
{ 

	bool isChange	= _database	!= databaseJson;
	_database	= databaseJson;
	if(isChange) dbUpdate(); 
	
	if(isChange)
		emit databaseJsonChanged(); 
}

void DataSet::setDataFileSynch(bool synchronizing)					
{ 
	bool isChange	= _dataFileSynch	!= synchronizing;
	_dataFileSynch	= synchronizing;	
	if(isChange) dbUpdate(); 
	
	if(isChange)
		emit dataFileSynchChanged();
}

void DataSet::dbCreate()
{
	JASPTIMER_SCOPE(DataSet::dbCreate);
	
	assert(!_defaultFilter && _dataSetId == -1);
	
	db().transactionWriteBegin();

	//The variables are probably empty though:
	_dataSetId		= db().dataSetInsert(_dataFilePath, _dataFileTimestamp, _description, _database.toStyledString(), _emptyValues->toJson().toStyledString(), _dataFileSynch, _csvDelimiter);
	_defaultFilter	= new Filter(this);
	
	_defaultFilter->dbCreate();
	_columns.clear();

	db().transactionWriteEnd();

	_rowCount		= 0;

	setupEncoderPrefix();
}

void DataSet::dbUpdate()
{
assert(_dataSetId > 0);
	db().dataSetUpdate(_dataSetId, _title, _dataFilePath, _dataFileTimestamp, _description, _database.toStyledString(), _emptyValues->toJson().toStyledString(), _dataFileSynch, _csvDelimiter);
	incRevision();
}

void DataSet::dbLoad(int id, std::function<void(float)> progressCallback, Version doUpgradeFrom)
{
	JASPTIMER_SCOPE(DataSet::dbLoad);

	assert(_dataSetId == -1 || _dataSetId == id || (_dataSetId != -1 && id == -1));

	if(id != -1 && !db().dataSetExists(id))
	{
		Log::log() << "No DataSet with id " << id << "!" << std::endl;
		return;
	}
		
	if(id != -1)
		_dataSetId	= id;

	assert(_dataSetId > 0);

	std::string emptyVals, databaseJson;

	db().dataSetLoad(_dataSetId, _title, _dataFilePath, _dataFileTimestamp, _description, databaseJson, emptyVals, _revision, _dataFileSynch, _csvDelimiter);

	db().dataSetGetComputedInfo(_dataSetId, _invalidated, _codeType, _rCode, _error, _defaultInputFilterId);

	Json::Reader().parse(databaseJson,	_database);

	progressCallback(0.1);

	if(!_defaultFilter)
		_defaultFilter = new Filter(this);
	_defaultFilter->dbLoad();
	
	progressCallback(0.2);

	int colCount	= db().dataSetColCount(_dataSetId);
	_rowCount		= db().dataSetRowCount(_dataSetId);
	//Log::log() << "colCount: " << colCount << ", " << "rowCount: " << rowCount() << std::endl;

	float colProgressMult = 1.0 / colCount;
	
	bool	do019Fix	= doUpgradeFrom != Version() && doUpgradeFrom < "0.19",
			do0961Fix	= doUpgradeFrom != Version() && doUpgradeFrom < "0.96.1";

	//Ideally we have the emptyvalues before loading the columns, so we get the right labels in the labeleditor, butr for older than 0.19 stuff is complicated so we do that later.
	Json::Value emptyValsJson;
	Json::Reader().parse(emptyVals,		emptyValsJson);

	if(!do019Fix && !do0961Fix)
	{
		_emptyValues->fromJson(emptyValsJson);

		for(size_t i=0; i<colCount; i++)
		{
			if(_columns.size() == i)
				_columns.push_back(new Column(this));
	
			_columns[i]->dbLoadIndex(i, false);
			
			progressCallback(0.2 + (i * colProgressMult * 0.3)); //should end at 0.5
		}
	
		for(size_t i=colCount; i<_columns.size(); i++)
			delete _columns[i];
	
		_columns.resize(colCount);
			
		db().dataSetBatchedValuesLoad(this, [&](float p){ progressCallback(0.50 + (p * 0.25)); });
		db().dataSetBatchedLabelsLoad(this, [&](float p){ progressCallback(0.75 + (p * 0.25)); });
	}
	else
	{
		if(do019Fix)	upgradeEmptyValsFrom018To019(emptyValsJson);
		else			_emptyValues->fromJson(emptyValsJson);
			
		for(size_t i=0; i<colCount; i++)
		{
			if(_columns.size() == i)
				_columns.push_back(new Column(this));
	
			_columns[i]->dbLoadOldIndex(i);
			
			progressCallback(0.2 + (i * colProgressMult * 0.6));
		}
		
		//Now we will recreate the dataset, but because Audit can make special Filters we need to handle that here now, otherwise they dissappear		
		intset allFilters = db().dataSetGetFilters(_dataSetId);
		Filters	loadedFilters;
		
		for(int id : allFilters)
		{
			const std::string & fName = db().filterGetName(id);
			
			if(fName != DEFAULT_FILTER_NAME)
				loadedFilters.push_back(new Filter(this, fName, false)); //registers itself into _filters
		}
		
		db().dataSetCreateTable(this);
		db().dataSetBatchedValuesUpdate(this, _columns, [&](float p){ progressCallback(0.8 + (p * 0.2)); });
		
		//Persist the (recreated) filter data; the Filter objects stay owned by this DataSet.
		for(Filter * f : loadedFilters)
			f->dbUpdate(true);
	}

	setupEncoderPrefix();
}


void DataSet::upgradeEmptyValsFrom018To019(const Json::Value & emptyVals)
{
	//So, 0.18.0, 0.18.1, 0.18.2 jaspfiles cant be loaded in 0.18.3
	//also, those versions were pretty buggy, so here we will just try to handle the case of 0.18.3
	//above we made sure _ints and _dbls are synched again.
	//now we will extract the missing data map and turn it into emptyvalues and proper values
	
	// The emptyValues json contains
	const Json::Value	& emptyValuesPerColumn = emptyVals["emptyValuesPerColumn"], // object, names=columnnames: array of empty value strings
						& missingDataPerColumn = emptyVals["missingDataPerColumn"], // object, names=columnnames: object { "row#": "original display" }
						& workspaceEmptyValues = emptyVals["workspaceEmptyValues"]; // array of empty value strings
	
	Log::log() << "Upgrading empty values from 0.18 to higher looked at jsons:\nemptyValuesPerColumn: " << emptyValuesPerColumn.toStyledString() << "\nmissingDataPerColumn: " << missingDataPerColumn.toStyledString() << "\nworkspaceEmptyValues: " << workspaceEmptyValues.toStyledString() << std::endl;
	
	stringset workspaceEmpty = JsonUtilities::jsonStringArrayToSet(workspaceEmptyValues);
	
	for(Column * column : _columns)
	{
		if(column->type() == columnType::nominalText)
			column->setType(columnType::nominal);
		
		const Json::Value	& missingData = !missingDataPerColumn.isMember(column->name()) ? Json::nullValue : missingDataPerColumn[column->name()],
							& emptyValues = !emptyValuesPerColumn.isMember(column->name()) ? Json::nullValue : emptyValuesPerColumn[column->name()];
		
		stringset emptyValSet;
		
		if(emptyValues.isArray())
			for(const Json::Value & val : emptyValues)
				emptyValSet.insert(val.asString());
		
		if(missingData.isObject())
		{
			stringset localEmpties = column->mergeOldMissingDataMap(missingData);
			emptyValSet.insert(localEmpties.begin(), localEmpties.end());
		}
		
		//If the column and workspace sets are not the same size, and there are actually values here that are not a subset of the workspace values then that means we really do have emptyvalues for this column
		if(emptyValSet != workspaceEmpty && emptyValSet.size() && !std::includes(workspaceEmpty.begin(), workspaceEmpty.end(), emptyValSet.begin(), emptyValSet.end()))
		{
			column->setHasCustomEmptyValues(true		);
			column->setCustomEmptyValues(	emptyValSet	);
			
			Log::log() << "Based on this the new column emtpy values for " << column->name() << " are:\n" << column->emptyValues()->toJson().toStyledString() << std::endl;
		}
	}
	
	
	
	_emptyValues->setEmptyValues(workspaceEmpty);
	
	
	Log::log() << "Based on this the new workspace emtpy values are:\n" << _emptyValues->toJson().toStyledString() << std::endl;
	
	dbUpdate();
}

void DataSet::batchColumnHadChange(Column *col)
{
	_changedDuringBatch.insert(col);
}

void DataSet::setColumnCount(size_t colCount)
{
	db().transactionWriteBegin();

	int curCount = _columns.size();
	
	bool alterTableAfterwards = curCount == 0 && colCount > 0;

	if(colCount > curCount)
		insertColumns(curCount, colCount-curCount, !alterTableAfterwards);

	else if(colCount < curCount)
		for(size_t i=curCount-1; i>=colCount; i--)
			removeColumn(i);
	

	incRevision();

	db().transactionWriteEnd();
	
	if(alterTableAfterwards)
		db().dataSetCreateTable(this);
}

void DataSet::setRowCount(size_t rowCount, bool alsoLoadData)
{
	_rowCount = rowCount; //Make sure we do set the rowCount variable here so the batch can easily see how big it ought to be in DatabaseInterface::dataSetBatchedValuesUpdate

	if(!writeBatchedToDB() && alsoLoadData)
	{
		db().dataSetSetRowCount(_dataSetId, rowCount);
		dbLoad(); //Make sure columns have the right data in them
	}
	else
	{
		//We are doing things batched, so its possible that a function like DatabaseInterface::dataSetBatchedValuesUpdate tries to fill up the columns.
		//It also might use the size of the vectors to know what to delete. So lets just resize those vectors a bit
		for(Column * col : _columns)
			col->setRowCount(_rowCount);
	}

	_defaultFilter->reset();
	
	refresh();
}

void DataSet::incRevision()
{
	assert(_dataSetId != -1);

	if(!writeBatchedToDB())
	{
		_revision = db().dataSetIncRevision(_dataSetId);
		checkForChanges();
	}
}

bool DataSet::checkForUpdates(std::function<void(float)> progressCallback)
{
	JASPTIMER_SCOPE(DataSet::checkForUpdates);

	if(_dataSetId == -1)
		return false;
	
	stringset prevCols;
	for(Column * col : _columns)
		prevCols.insert(col->name());
	
	size_t		rowCountPrev = rowCount();
	stringvec	colsChanged, 
				colsRemoved;
	bool		newColumns,
				rowCountChanged;
	
		
	if(_revision < db().dataSetGetRevision(_dataSetId))
	{
		dbLoad(-1, progressCallback);
		
		newColumns		= prevCols.size() < _columns.size();
		rowCountChanged = rowCountPrev != rowCount();
		
		
		colsChanged.clear();
		for(Column * col : _columns)
		{
			colsChanged.push_back(col->name());
			prevCols.erase(col->name());
		}
		
		colsRemoved = stringvec(prevCols.begin(), prevCols.end());
		
		emit datasetChanged(_dataSetId, tq(colsChanged), tq(colsRemoved), {}, rowCountChanged, newColumns);
		
		refresh();
		
		return true;
	}
	else
	{
		bool somethingChanged = _defaultFilter->checkForUpdates();
		
		stringset	dbFilterNames = db().dataSetFilterNames(_dataSetId);
		FilterSet	destroyUs;
		Filters		keepUs;
		
		for(Filter * f : _filters)
			if(!dbFilterNames.count(f->name()))
				destroyUs.insert(f);
			else
			{
				keepUs.push_back(f);
				
				if(f != _defaultFilter && f->checkForUpdates())
					somethingChanged = true;
			}
		
		for(const std::string & fName : dbFilterNames)
			if(!filter(fName))
			{
				Filter * missingFilter = new Filter(this, fName, false);
				
				keepUs.push_back(missingFilter);
				somethingChanged = true;
			}

		for(Column * col : _columns)
			if(col->checkForUpdates())
			{
				somethingChanged = true;
				colsChanged.push_back(col->name());
			}
		
		colsRemoved		. clear();
		newColumns		= false;
		rowCountChanged = rowCountPrev != rowCount();
		
		if(destroyUs.size() > 0)
		{
			somethingChanged = true;

			Filter * newShownFilter = nullptr;

			//If the shown filter is among the pruned (its DB row vanished), pick a surviving replacement
			//BEFORE deleting, mirroring deleteShownFilter(), so we never leave a dangling _shownFilter.
			if(_shownFilter && _shownFilter != _defaultFilter && destroyUs.count(_shownFilter))
			{
				size_t indexWas = 0;
				for(size_t i=0; i<_filters.size(); i++)
					if(_filters[i] == _shownFilter)
					{
						indexWas = i;
						break;
					}

				newShownFilter = keepUs.empty() ? _defaultFilter : keepUs[std::min(indexWas, keepUs.size() - 1)];
			}

			_filters = keepUs;

			if(newShownFilter)
			{
				_shownFilter = newShownFilter;
				emit shownFilterChanged(this);
			}

			for(Filter * f : destroyUs)
			{
				emit filterRemoved(f);
				delete f;
			}
		}
		
		if(somethingChanged || colsChanged.size() || colsRemoved.size() || rowCountChanged || newColumns)
		{
			emit datasetChanged(_dataSetId, tq(colsChanged), tq(colsRemoved), {}, rowCountChanged, newColumns);
		
			refresh();
		}

		return somethingChanged || rowCountChanged;
	}
}

void DataSet::runComputedColumn(QString columnName, QString code, columnType columnType)
{
	emit _workspace->runComputedColumn(id(), columnName, code, columnType);
}

void DataSet::runComputedDataset(QString code, int defaultInputFilterId)
{
	emit _workspace->runComputedDataSet(id(), code, defaultInputFilterId);
}

std::string DataSet::rCodeStripped() const
{
	return stringUtils::stripRComments(_rCode);
}

Filter * DataSet::defaultInputFilter() const
{
	return _workspace ? _workspace->filterById(_defaultInputFilterId) : nullptr;
}

DataSet * DataSet::defaultInputDataSet() const
{
	Filter * input = defaultInputFilter();
	return input ? input->data() : nullptr;
}

bool DataSet::iShouldBeSentAgain()
{
	if(!invalidated())
		return false;

	DataSet * input = defaultInputDataSet();

	if(input && input->isComputed() && input->invalidated())
		return false;

	return true;
}

void DataSet::dbUpdateComputedDatasetStuff()
{
	std::string oldError = _error;

	db().dataSetSetComputedInfo(_dataSetId, _invalidated, _codeType, _rCode, _error, _defaultInputFilterId);
	incRevision();

	if(oldError != _error)
		emit errorChanged();
}

bool DataSet::setRCode(const std::string & rCode)
{
	if(_rCode == rCode)
		return false;

	_rCode		= rCode;
	invalidate();
	dbUpdateComputedDatasetStuff();
	emit rCodeChanged();
	checkForDependentDatasetsToBeSent(true);

	return true;
}

void DataSet::setCodeType(computedColumnType codeType)
{
	if(codeType == _codeType)
		return;

	_codeType = codeType;

	dbUpdateComputedDatasetStuff();
	emit codeTypeChanged();
}

void DataSet::setInvalidated(bool invalidated)
{
	if(_invalidated == invalidated)
		return;

	_invalidated = invalidated;
	db().dataSetSetComputedInfo(_dataSetId, _invalidated, _codeType, _rCode, _error, _defaultInputFilterId);
	incRevision();
	emit invalidatedChanged();
}

bool DataSet::setError(const std::string & error)
{
	if(error == _error)
		return false;

	_error = error;
	dbUpdateComputedDatasetStuff();

	//dbUpdateComputedDatasetStuff() snapshots oldError *after* we already changed _error, so it can't
	//detect this change itself: emit explicitly (mirrors Column::setError) so QML's `error` binding
	//gets notified that a computed dataset failed.
	emit errorChanged();

	return true;
}

bool DataSet::setDefaultInputFilterId(int defaultInputFilterId)
{
	if(defaultInputFilterId == _defaultInputFilterId)
		return true;

	//Refuse to introduce a cycle (A <- B <- A) between computed datasets: a computed dataset must
	//not depend on an input filter that (transitively) depends on it, or the recompute cascade would livelock.
	if(_workspace && defaultInputFilterId >= 0)
	{
		Filter * inputFilter = _workspace->filterById(defaultInputFilterId);
		DataSet * target = inputFilter ? inputFilter->data() : nullptr;
		if (target && _workspace->wouldCreateComputedDataSetLoop(this, target))
		{
			setError("The filter chosen as input for this computed dataset would create a loop between the computed datasets.");
			dbUpdateComputedDatasetStuff();
			return false;
		}
	}

	_defaultInputFilterId = defaultInputFilterId;
	invalidate();

	//Clear any previously surfaced input-loop error: picking a valid input must not leave the earlier
	//"would create a loop" error persisting.
	dbUpdateComputedDatasetStuff();
	if(!_error.empty())
		setError("");

	emit defaultInputFilterChanged();

	//Changing only the *input* must still trigger a recompute (setRCode no-ops when the code text is
	//unchanged, so input-only edits used to leave the computed dataset stuck invalidated).
	checkForDependentDatasetsToBeSent(true);

	return true;
}

bool DataSet::tryAndRunComputedDataset()
{
	const std::string code = rCodeStripped();

	if(code.empty())
		return false;

	runComputedDataset(tq(code), _defaultInputFilterId);

	return true;
}

void DataSet::checkForDependentDatasetsToBeSent(bool refreshMe)
{
	//Invalidate this (if refreshMe) and every computed dataset that reads from this one.
	for(DataSet * ds : _workspace->dataSets())
		if(ds->isComputed() && ((ds == this && refreshMe) || ds->defaultInputDataSet() == this))
			ds->invalidate();

	//Anti-livelock guard: if the computed-dataset dependency graph somehow contains a cycle (e.g.
	//restored from an old file), do not keep requesting computations; mark the participants with an
	//error instead so the user breaks the circle.
	std::string loopError;
	if (_workspace->computedDataSetsHaveLoop(loopError))
	{
		for (DataSet * ds : _workspace->dataSets())
			if (ds->isComputed() && ds->invalidated())
				ds->setError(loopError);
		return;
	}

	//Re-dispatch only the datasets that were invalidated above (plus dependents). Crucially, this must
	//NOT re-dispatch `this` when refreshMe is false: handleDataSetChanged() (which runs on any data
	//reload, including the reload right after a successful compute) calls this with refreshMe=false and
	//`this` still invalidated at that moment; re-dispatching it there would recompute it forever.
	for(DataSet * ds : _workspace->dataSets())
		if(ds->isComputed() && ((ds == this && refreshMe) || ds->defaultInputDataSet() == this))
			if(ds->iShouldBeSentAgain())
				ds->tryAndRunComputedDataset();
}

Columns DataSet::computedColumns() const
{
	Columns computedColumns;

	for(Column * column : _columns)
		if(column->isComputed())
			computedColumns.push_back(column);

	return computedColumns;
}

void DataSet::loadOldComputedColumnsJson(const Json::Value &json)
{
	if (!json.isArray()) return;

	for(const Json::Value & colJson : json)
	{
		if (!colJson.isObject() || colJson["error"].asString().rfind("The engine crashed", 0) == 0) continue;

		const std::string name = colJson["name"].asString();

		Column * col = column(name);

		if(!col && !name.empty())
			col = createColumn(name);

		if(!col)
			continue;

		col->loadComputedColumnJsonBackwardsCompatibly(colJson);
	}

	for(Column * col : computedColumns())
		col->findDependencies();
}

void DataSet::setEmptyValuesJsonOldStuff(const Json::Value &emptyValues)
{
	// For backward compatibility we take the default ones if the workspaceEmptyValues are not specified
	Json::Value updatedEmptyValues = emptyValues;
	Json::Value emptyValuesJson(Json::arrayValue);
	for (const std::string& val : _defaultEmptyvalues)
		emptyValuesJson.append(val);
	updatedEmptyValues["workspaceEmptyValues"] = emptyValuesJson;
	_emptyValues->fromJson(updatedEmptyValues);
}

void DataSet::setEmptyValuesJson(const Json::Value &emptyValues, bool updateDB)
{
	try
	{
		if (emptyValues.isMember("workspaceEmptyValues"))
			setEmptyValuesJsonOldStuff(emptyValues);
		else
			_emptyValues->fromJson(emptyValues);
	}
	catch(std::exception & e)
	{
		Log::log() << "DataSet::setEmptyValuesJson got exception: " << e.what() << std::endl;
	}

	if (updateDB)
		dbUpdate();
}

void DataSet::setEmptyValuesFromStrings(const stringset &values)
{
	_emptyValues->setEmptyValues(values);
	for(Column * column : _columns)
		column->nonFilteredCountersReset();
	dbUpdate();
}

void DataSet::setDescription(const std::string &desc)
{
	bool isChange	= _description != desc;
	_description	= desc;
	dbUpdate();
	
	if(isChange)
		emit descriptionChanged();
}

void DataSet::refresh(bool doColumnsToo)	
{ 
	beginResetModel(); 
	
	
	if(doColumnsToo)
		for(Column * c : _columns)
			c->refresh(false);
	
	endResetModel(); 

	//Emit these after the reset completes: they connect into models that may re-query this DataSet,
	//which must not happen while a reset is still in progress.
	emit descriptionChanged();
	emit dataFileChanged();
	emit databaseJsonChanged();
	emit dataFileSynchChanged();
	emit dataTimestampChanged();
	emit columnsLabelFilteredCountChanged();
	emit shownFilterChanged(this);
	emit shownColumnChanged();
	emit titleChanged();
}

void DataSet::runFilters()
{
	_defaultFilter->setInvalidated(true);
	
	for(Filter * f : _filters)
		f->setInvalidated(true);
}

DatabaseInterface &DataSet::db()	
{ 
	return *DatabaseInterface::singleton(); 
}

const DatabaseInterface &DataSet::db() const
{ 
	return *DatabaseInterface::singleton(); 
}

stringset DataSet::findUsedColumnNames(std::string searchThis)
{
	stringset columnsFound, columnsWithTypeFound;
	encoder().encodeRScript(searchThis, &columnsWithTypeFound);
	
	//The found columns now also include the type, but we dont really care about that right now.
	//Instead we'll make use of the encode->decode not being symmetrical (for the results to be less ugly) and dropping the type
	
	for(const std::string & colPlusType : columnsWithTypeFound)
		columnsFound.insert(encoder().decode(encoder().encode(colPlusType)));
	
	return columnsFound;
}

Json::Value DataSet::jsonForCompare() const
{
	Json::Value json(Json::objectValue);

	//json["description"]			= _description; //Contains datetime...
	json["customEmptyValues"]	= _emptyValues->toJson();
	json["columns"]				= Json::arrayValue;

	for(Column * column : _columns)
		json["columns"].append(column->jsonForCompare());

	//std::cerr << json.toStyledString() << std::endl;

	return json;
}

int DataSet::columnsLabelFilteredCount() const
{
	int colsFiltered = 0;

	for(Column * col : columns())
		if(col->hasLabelFilter())
			colsFiltered++;

	return colsFiltered;
}

int DataSet::rowCount(const QModelIndex &) const
{
	return _rowCount;
}

int DataSet::columnCount(const QModelIndex &) const
{
	return _columns.size();
}

QVariant DataSet::data(const QModelIndex &index, int role) const
{
	if(!index.isValid())
		return QVariant();
	
	
	if(index.row() >= rowCount() || index.column() >= columnCount())
		return QVariant(); // if there is no data then it doesn't matter what role we play
	
	JASPTIMER_SCOPE(DataSet::data);
	
	Column * column = columns()[index.column()];

	switch(role)
	{
	case Qt::DisplayRole:									return tq(column->getDisplay(index.row(), true, true));
	case int(dataPkgRoles::noSepaDisplay):					return tq(column->getDisplay(index.row(), false, false));
	case int(dataPkgRoles::label):							return tq(column->getLabel(index.row(), false, true));
	case int(dataPkgRoles::value):							return tq(column->getValue(index.row()));
	case int(dataPkgRoles::name):							return tq(column->name());
	case int(dataPkgRoles::title):							return tq(column->title());
	case int(dataPkgRoles::filter):							return getRowFilter(index.row());
	case int(dataPkgRoles::columnType):						return int(column->type());
	case int(dataPkgRoles::description):					return tq(column->description());
	case int(dataPkgRoles::inEasyFilter):					return getColumnInDragNDropShownFilter(column);
	case int(dataPkgRoles::shadowDisplay):					return tq(column->getShadow(index.row()));
	case int(dataPkgRoles::valuesDblList):					return column->getColumnValuesAsDoubleList();
	case int(dataPkgRoles::nonFilteredNumericValuesCount):	return column->nonFilteredNumericsCount();
	case int(dataPkgRoles::nonFilteredLevels):				return tq(column->nonFilteredLevels());
	case int(dataPkgRoles::computedColumnType):				return int(column->codeType());
	case int(dataPkgRoles::columnPkgIndex):					return index.column();
	case int(dataPkgRoles::lines):
	{
		bool	iAmActive		= getRowFilter(index.row()),
				belowMeIsActive = index.row() < column->rowCount() - 1	&& getRowFilter(index.row() + 1);

		return getDataSetViewLines(
			iAmActive,
			iAmActive,
			iAmActive && !belowMeIsActive,
			iAmActive //&& index.column() == columnCount() - 1 //always draw left line and right line only if last col
		);
	}
	}
	
	return QVariant();
}

bool DataSet::setData(const QModelIndex &index, const QVariant &value, int role)
{
	JASPTIMER_SCOPE(DataSet::setData);
		
	if(!index.isValid() || index.column() < 0 || index.column() >= columnCount()) 
		return false;

	Column	* column	= static_cast<Column*>(columns()[index.column()]);
	
	if(role == Qt::DisplayRole || role == Qt::EditRole || role == int(dataPkgRoles::value) || role == int(dataPkgRoles::valueLabelPair) || role == int(dataPkgRoles::valuesStrList))
	{				
		bool				isPair	= role == int(dataPkgRoles::valueLabelPair),
							isVals	= role == int(dataPkgRoles::valuesStrList);
		QVariantList		listVar	= isPair || isVals ? value.toList()	: QVariantList{ value };
		bool				aChange = false;
		
		if(!isVals)
		{
			const std::string	val		= fq(listVar[0].toString()),
								label	= fq(isPair ? listVar[1].toString() : "");
								aChange	= !isPair	
										? column->setStringValue(index.row(), val == EmptyValues::displayString() ? "" : val)
										: column->setValue(index.row(), val, label);
		}
		else //Its a list of values, for instance "intial values"
		{
			int r=0;
			for(const QVariant & val : listVar)
				if(column->setStringValue(index.row() + r++, fq(val.toString() == tq(EmptyValues::displayString()) ? "" : val.toString())))
					aChange = true;
		}
		
		if(aChange)
		{
			JASPTIMER_SCOPE(Column::setData reset model);

			emit manualEditMade();
			
			column->labelsRemoveOrphans();
			column->nonFilteredCountersReset();
			column->labelsHandleAutoSort();

			refresh();
			handleColumnChanged(column);
			handleLabelsReordered(column);
			
			//Probably the labelfilter thing and the constructor thing should 
			if(column->hasLabelFilter())
			{
				emit labelFilterChanged();
				runFilters();
			}
		}
		
		return true;
	}
	else
	{
		bool aChange = false;

		switch(role)
		{
		case int(dataPkgRoles::description):
			column->setDescription(value.toString().toStdString());
			aChange = true;
			break;

		case int(dataPkgRoles::title):
			column->setTitle(value.toString().toStdString());
			aChange = true;
			break;

		case int(dataPkgRoles::columnType):
			if(value.toInt() >= int(columnType::unknown) && value.toInt() <= int(columnType::scale))
			{
				columnType converted = static_cast<columnType>(value.toInt());
				if(converted != column->type())
				{
					if(column->changeType(converted) == columnTypeChangeResult::generatedFromAnalysis)
						emit showWarning(tr("Changing column type failed"), tr("The column '%1' is generated by an analysis and its type is fixed.").arg(tq(column->name())));
					else
						aChange = true;
				}
			}
			break;
		}

		if(aChange)
			emit manualEditMade();

		return true;
	}	
}

QVariant DataSet::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (section < 0 || section >= (orientation == Qt::Horizontal ? columnCount() : rowCount()))
			return QVariant();
		
	JASPTIMER_SCOPE(DataSet::headerData);
	
	if(orientation == Qt::Vertical)
		switch(role)
		{
		default:
			return QVariant();

		case int(dataPkgRoles::maxRowHeaderString):
			return QString::number(rowCount()) + "XXX";

		case Qt::DisplayRole:
			return QVariant(section + 1);
			
		case int(dataPkgRoles::filter):
			return !(section >= 0 && shownFilter()->filtered().size() > 0) || shownFilter()->filtered()[section];
		}
	else
	{
		Column * col = columns()[section];
				
		switch(role)
		{
		case int(dataPkgRoles::maxColString):
		{
			//calculate some kind of maximum string to give views an expectation of the width needed for a column
			bool		hasFilter	= col && (col->hasLabelFilter() || getColumnInDragNDropShownFilter(col));
			QString		dummyText	= headerData(section, orientation, int(dataPkgRoles::maxColumnHeaderString)).toString() + (col->isComputed() ? "XXX" : "") + (hasFilter ? "XXX" : ""); //Bit of padding for hamburger, filtersymbol and columnIcon
			qsizetype	colWidth	= getMaximumColumnWidthInCharacters(section);

			while(colWidth > dummyText.length())
				dummyText += "X";

			return dummyText;
		}
		case int(dataPkgRoles::maxColumnHeaderString):			return headerData(section, orientation, Qt::DisplayRole).toString() + "XXX";
		case int(dataPkgRoles::maxRowHeaderString):				return QString::number(rowCount())		+ "XXX";
		case Qt::TextAlignmentRole:								return QVariant(Qt::AlignCenter);
		case int(dataPkgRoles::filter):							return		!col ? false							: col->hasLabelFilter() || getColumnInDragNDropShownFilter(col);
		case Qt::DisplayRole:									[[fallthrough]];
		case int(dataPkgRoles::name):							return tq(	!col ? "?"								: col->name());
		case int(dataPkgRoles::labelsHasFilter):				return		!col ? false							: col->hasLabelFilter();
		case int(dataPkgRoles::columnIsComputed):				return		!col ? false							: col->isComputed() && col->codeType() != computedColumnType::analysisNotComputed;
		case int(dataPkgRoles::computedColumnError):			return tq(	!col ? "?"								: col->error());
		case int(dataPkgRoles::computedColumnIsInvalidated):	return		!col ? false							: col->invalidated();
		case int(dataPkgRoles::columnType):						return int(	!col ? columnType::unknown				: col->type());
		case int(dataPkgRoles::computedColumnType):				return int(	!col ? computedColumnType::notComputed	: col->codeType());
		case int(dataPkgRoles::description):					return tq(	!col ? "?"								: col->description());
		case int(dataPkgRoles::title):							return tq(	!col ? "?"								: col->title());
		case int(dataPkgRoles::previewScale):
		case int(dataPkgRoles::previewOrdinal):					
		case int(dataPkgRoles::previewNominal):					
		{
			columnType colTypeWanted = 
					role == int(dataPkgRoles::previewNominal) 
					? columnType::nominal 
					: role == int(dataPkgRoles::previewOrdinal)
					? columnType::ordinal
					: columnType::scale;
			
			stringvec preview = !col ? stringvec() : col->previewTransform(colTypeWanted);
			
			if(preview.size() != 4)
				return QVariant();
			
			QString	levelsTotal		= tq(preview[0]),
					levelsNums		= tq(preview[1]),
					vals			= tq(preview[2]),
					empties			= tq(preview[3]);
			
			if(colTypeWanted == columnType::scale)
				return	tr("There are %1 total levels, of which %2 have a numeric value.\nAs a '%3' it looks like: %4\n%5")
						.arg(levelsTotal)
						.arg(levelsNums)
						.arg(QColumnUtils::getTypeFriendly(colTypeWanted))
						.arg(vals)
						.arg(
							empties == "" 
							? "" 
							: tr("Implicit missing values: %1").arg(empties)
						);
			else
				return tr("There are %1 total levels.\nAs a '%2' it looks like: %3")
					.arg(levelsTotal)
					.arg(QColumnUtils::getTypeFriendly(colTypeWanted))
					.arg(vals);
		}
		}
	}
	
	return QVariant();
}

Qt::ItemFlags DataSet::flags(const QModelIndex &index) const
{
	bool	isEditable	= _workspace && _workspace->dataMode() && index.column() >= 0 && index.column() < columnCount() && !columns()[index.column()]->isComputed();

	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | (isEditable ? Qt::ItemIsEditable : Qt::NoItemFlags);
}

bool DataSet::insertRows(int row, int count, const QModelIndex &)
{
	if(row > rowCount())
			row = rowCount();
	
	emit manualEditMade();
	
	beginInsertRows(QModelIndex(), row, row + count - 1);

	stringvec changed;

	beginBatchedToDB();
	for(int c=0; c<columnCount(); c++)
	{
		changed.push_back(column(c)->name());

		for(int r=row; r<row+count; r++)
			column(c)->rowInsertEmptyVal(r);
	}

	setRowCount(rowCount() + count);
	incRevision();
	endBatchedToDB();
	
	endInsertRows();

	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	emit datasetChanged(_dataSetId, tq(changed), tq(missingColumns), tq(changeNameColumns), true, false);

	return true;
}

bool DataSet::removeRows(int row, int count, const QModelIndex &)
{
	if(row == -1)
		return false;
	
	emit manualEditMade();
	
	beginRemoveRows(QModelIndex(), row, row + count - 1);

	stringvec changed;

	beginBatchedToDB();
	
	for(Column * column : columns())
	{
		changed.push_back(column->name());
		
		for(int r=row+count; r>row; r--)
			column->rowDelete(r-1);
	}

	setRowCount(rowCount() - count);
	incRevision();
	endBatchedToDB();

	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	endRemoveRows();

	emit datasetChanged(_dataSetId, tq(changed), tq(missingColumns), tq(changeNameColumns), true, false);

	return true;
}

bool DataSet::isColumnNameFree(const std::string & name) const
{
	return getColumnIndex(name) == -1;	
}

std::string DataSet::freeNewColumnName(size_t startAfterThis) const
{
	const QString nameBase = tr("Column %1");

	while(true)
	{
		const std::string & newColName = fq(nameBase.arg(++startAfterThis));
		if(isColumnNameFree(newColName))
			return newColName;
	}
}

bool DataSet::insertColumns(int column, int count, const QModelIndex &)
{
	if(column > columnCount())
			column = columnCount(); //the column will be created if necessary but only if it is in a logical place. So the end of the vector
	
	emit manualEditMade();
	
	beginInsertColumns(QModelIndex(), column, column + count - 1);
	
	stringvec changed;

	for(int c = column; c<column+count; c++)
	{
		const std::string & name = freeNewColumnName(c);
		
		DataSet::insertColumn(c);
		DataSet::column(c)->setName(name);
		DataSet::column(c)->setDefaultValues(columnType::scale);

		changed.push_back(name);
	}

	endInsertColumns();

	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	emit datasetChanged(_dataSetId, tq(changed), tq(missingColumns), tq(changeNameColumns), false, true);

	_encoder->setCurrentNames(getColumnNames());

	return true;
}

bool DataSet::removeColumns(int column, int count, const QModelIndex &)
{
	if(column == -1)
		return false;

	emit manualEditMade();
	
	beginRemoveColumns(QModelIndex(), column, column + count - 1);

	stringvec	changed;
	strstrmap	changeNameColumns;
	stringvec	missingColumns;

	for(int c = column + count; c>column; c--)
	{
		missingColumns.push_back(columns()[c - 1]->name());
		DataSet::removeColumn(c - 1);
	}

	endRemoveColumns();

	emit datasetChanged(_dataSetId, tq(changed), tq(missingColumns), tq(changeNameColumns), false, false);

	_encoder->setCurrentNames(getColumnNames());

	return true;
}

void DataSet::handleColumnChanged(const Column * column)
{
	emit datasetChanged(_dataSetId, tq(stringvec({column->name()})), {}, {}, false, false);
	emit manualEditMade();
}

void DataSet::handleLabelsReordered(const Column *column)
{
	emit labelsReordered(tq(column->name()));
}


void DataSet::handleDataSetChanged( int						dataSetID,
									QStringList				changedColumns,
									QStringList				missingColumns,
									QMap<QString, QString>	changeNameColumns,
									bool					rowCountChanged,
									bool					hasNewColumns)
{
	assert(_dataSetId == dataSetID);

	std::string concatenatedMissings = fq(missingColumns.join(", "));

	for(Column * col : computedColumns())
	{
		if(rowCountChanged)
			col->invalidate();

		for(const QString & changed : changedColumns)
			if(col->dependsOn(fq(changed), false))
				col->invalidate();

		bool containsAChangedName = false;
		for(const auto & changedNames : changeNameColumns.keys())
			if(col->dependsOn(fq(changedNames), false))
			{
				containsAChangedName = true;
				break;
			}

		if(containsAChangedName)
		{
			auto stdChangeNameCols = fq(changeNameColumns);
			col->setRCode(ColumnEncoder::replaceColumnNamesInRScript(col->rCode(), stdChangeNameCols));
			col->setConstructorJson(JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(col->constructorJson(), stdChangeNameCols));
			col->invalidate();
		}

		if(col->codeType() == computedColumnType::constructorCode)
		{
			if(col->setConstructorJson(JsonUtilities::removeColumnsFromDragNDropFilterJSON(col->constructorJson(), fq(missingColumns))))
			{
				//So some column was removed from the json

				col->setRCode("stop('Certain columns where removed from the definition of this computed column.\nColumns that could`ve been here are: " + concatenatedMissings + "')");
				col->invalidate();
			}
		}
		else if(col->codeType() == computedColumnType::rCode &&	col->setRCode(ColumnEncoder::removeColumnNamesFromRScript(col->rCode(), fq(missingColumns))))
				col->invalidate();

	}

	_encoder->setCurrentNames(	getColumnTypesMap());

	for(Column * col : computedColumns())
	{
		col->findDependencies(); //columnNames might have changed right? so check it again
	

		if(col->iShouldBeSentAgain())
			col->tryAndRunComputedColumn();
	}

	//Computed datasets that read from this dataset must be recomputed too.
	if(
		changedColumns.size()		||
		missingColumns.size()		||
		changeNameColumns.size()	||
		rowCountChanged				||
		hasNewColumns				
	)
		checkForDependentDatasetsToBeSent();

	
}





bool DataSet::getRowFilter(int row) const
{
	const Filter * filter = shownFilter();
	if(!filter)
		return true;

	const std::vector<bool> & filtered = filter->filtered();
	return filtered.empty() || (row >= 0 && static_cast<size_t>(row) < filtered.size() && filtered[row]);
}

QVariant DataSet::getDataSetViewLines(bool up, bool left, bool down, bool right)
{
	return			(left ?		1 : 0) +
					(right ?	2 : 0) +
					(up ?		4 : 0) +
					(down ?		8 : 0);
}

bool DataSet::getColumnInDragNDropShownFilter(int columnIndex) const
{
	if(columnIndex < 0 || columnIndex >= columnCount()) 
		return false;
	
	return getColumnInDragNDropShownFilter(columns()[columnIndex]);
}

bool DataSet::getColumnInDragNDropShownFilter(Column * column) const
{
	return shownFilter()->columnsUsedInConstructor().count(column->name());
}

QString DataSet::descriptionQ() const
{
	return tq(description());
}

void DataSet::setDescriptionQ(const QString & newDescription)
{
	setDescription(fq(newDescription));
}

QString DataSet::dataFileQ() const
{
	return tq(dataFilePath());
}

void DataSet::setDataFileQ(const QString &newDataFile)
{
	setDataFile(fq(newDataFile));
}

void DataSet::setTitle(const QString &title)
{
	QString uniqueTitle = _workspace ? _workspace->makeDataSetTitleUnique(title, this) : title;

	if(_title == fq(uniqueTitle))
		return;

	_title = fq(uniqueTitle);

	emit titleChanged();

	if(_workspace)
		emit _workspace->dataSetTitleChanged(id());

	dbUpdate();
}

bool DataSet::dataFileCanHaveLabels() const
{
	return !tq(dataFilePath()).endsWith(".csv");
}

void DataSet::resetAllFilters()
{
	for(Column * col : columns())
		col->resetFilter();

	resetFilterCounters();

	emit allFiltersReset();
	emit columnsLabelFilteredCountChanged();
	//this is only used in conjunction with a reset so dont do: emit headerDataChanged(Qt::Horizontal, 0, columnCount());
}

void DataSet::resetFilterCounters()
{
	for(Column * col : columns())
		col->nonFilteredCountersReset();
}


bool DataSet::setColumnTypes(stringset columnIndexes, columnType newColumnType)
{
	bool somethingChanged = false;

	for(const std::string & columnIndex : columnIndexes)
	{
		Column *col = column(columnIndex);

		if (col->type() == newColumnType)
			continue;


		//the only possible "fail" is when an analysis made the column and thus decides the type
		//the user might bet
		if(col->changeType(newColumnType) == columnTypeChangeResult::generatedFromAnalysis)
		{
			emit showWarning(tr("Changing column type failed"), tr("The column '%1' is generated by an analysis and its type is fixed.").arg(tq(col->name())));
			continue;
		}

		somethingChanged = true;
	}

	if(somethingChanged)
		refresh();

	return somethingChanged;
}

void DataSet::filterByNameDone(int dataSetID, const QString &name, const QString &error)
{
	//Every DataSet is connected to the shared Workspace::filterByNameDone; only act on completions
	//that target this dataset, otherwise a same-named filter in another dataset would be reloaded.
	if(dataSetID != id())
		return;

	Filter * f = filter(fq(name));
	
	if(f && f->dbLoadResultAndError())
	{
		emit f->refreshAllAnalyses(f);
		
		if(shownFilter() == f)
			refresh();
	}
}


void DataSet::resetVariableTypes(int thresholdScale)
{
	for (Column * col : columns())
	{
		columnType guessedType = col->resetValues(thresholdScale);

		if(guessedType != col->type() && col->changeType(guessedType) == columnTypeChangeResult::changed)
			refresh();
	}
}

void DataSet::emitColumnChanged(const QString & colName)
{
	emit datasetChanged(_dataSetId, {colName}, {}, {}, false, false);
	
	int colIndex = columnIndex(column(fq(colName)));
	if(colIndex >= 0)
		emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
}

void DataSet::pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> &  labels, const intvec & coltypes, const QStringList & colNames, const std::vector<boolvec> & selected)
{
	JASPTIMER_SCOPE(DataSet::pasteSpreadsheet);

	int		rowMax			= ( values.size() > 0 ? values[0].size() : 0), 
			colMax			= values.size();
	bool	rowCountChanged = int(row + rowMax) > rowCount()	,
			colCountChanged = int(col + colMax) > columnCount()	;
	
	auto isSelected = [&selected](int row, int col)
	{
		return selected.size() == 0 || 	selected[col][row];
	};

	beginBatchedToDB();

	size_t oldColCount = columnCount();
	
	if(colCountChanged)
		setColumnCount(std::max(size_t(columnCount()), colMax + col));
	
	if(rowCountChanged)
		setRowCount(std::max(size_t(rowCount()), rowMax + row));
	
	stringvec changed;
	strstrmap changeNameColumns;

	for(int c=0; c<colMax; c++)
	{
		Column	*	dataColumn	= column(c + col);
		columnType	desiredType	= coltypes.size() > c								? columnType(coltypes[c])	: dataColumn->type();
					desiredType = desiredType == columnType::unknown				? columnType::scale			: desiredType;
		std::string colName		= (colNames.size() > c && !colNames[c].isEmpty())	? fq(colNames[c])			: dataColumn->name();
		
		// A column that only came into existence to hold the pasted data must get a default name.
		if (colName.empty() && size_t(c + col) >= oldColCount)
			colName = freeNewColumnName(c + col);
		
		dataColumn->setType(desiredType);

		bool aChange = false;
		for(int r=0; r<rowMax; r++)
			if(isSelected(r, c))
				aChange = dataColumn->setStringValue(r+row, fq(values[c][r]), labels.size() <= c || labels[c].size() <= r ? "" : fq(labels[c][r])) || aChange;
			
		aChange = aChange || colName != dataColumn->name() || desiredType != dataColumn->type();
		
		if(colName != dataColumn->name())
			changeNameColumns[dataColumn->name()] = colName;
		
		dataColumn->setName(colName);

		if(aChange)
		{
			changed.push_back(colName);
			dataColumn->nonFilteredCountersReset();
		}
	}

	endBatchedToDB();
	
	stringvec		missingColumns;

	emit datasetChanged(_dataSetId, tq(changed), tq(missingColumns), tq(changeNameColumns), rowCountChanged, colCountChanged);
}


void DataSet::columnsApply(intset columnIndexes, std::function<bool(Column * column, int col)> applyThis)
{
	QStringList changedCols;

	for(int columnIndex : columnIndexes)
	{
		Column* dataColumn = column(columnIndex);
	
		if(dataColumn && applyThis(dataColumn, columnIndex))
				changedCols << dataColumn->nameQ();
	}
	
	if(changedCols.size() > 0)
	{
		refresh();
		emit datasetChanged(_dataSetId, changedCols, {}, {}, false, false);
	}
}

void DataSet::columnsApply(intset columnIndexes, std::function<bool(Column * column)> applyThis)
{
	columnsApply(columnIndexes, [&](Column * column, int){ return applyThis(column); });
}

void DataSet::columnsApply(stringset columnNames, std::function<bool(Column * column)> applyThis)
{
	intset columnIndexes;
	
	for(auto & n : columnNames)
		columnIndexes.insert(getColumnIndex(n));
	
	columnsApply(columnIndexes, [&](Column * column, int){ return applyThis(column); });
}

void DataSet::columnsApply(stringset columnNames, std::function<bool(Column * column, int colIndex)> applyThis)
{
	intset columnIndexes;
	
	for(auto & n : columnNames)
		columnIndexes.insert(getColumnIndex(n));
	
	columnsApply(columnIndexes, applyThis);
}

void DataSet::columnsReverseValues(stringset columnIndexes)
{
	columnsApply(columnIndexes, [&](Column * column) 
	{ 
		column->valuesReverse();
		return true;
	});
}

void DataSet::columnsSetAutoSortForColumns(std::map<std::string,bool> sortPerColumn)
{
	stringset cols;
	for(auto & colSort : sortPerColumn)
		cols.insert(colSort.first);
	
	columnsApply(cols, [&](Column * column, int colIdx) 
	{ 
		column->setAutoSortByValue(sortPerColumn[column->name()]);
		
		if(cols.size() == 1)
			workspace()->setShownColumn(column);
		
		return true;
	});
}

Column * DataSet::createComputedColumn(const std::string & name, columnType type, computedColumnType desiredType, int analysisId)
{
	Column	* newComputedColumn = createColumn(name, type);

	newComputedColumn->setAnalysisId(analysisId);
	newComputedColumn->setCodeType(desiredType);

	refresh();

	return newComputedColumn;
}


void DataSet::invalidateAllComputedColumns()
{
	for(Column * col : computedColumns())
		if(	col->codeType() != computedColumnType::analysis	&& col->codeType() != computedColumnType::analysisNotComputed)
			col->invalidate();
	
	for(Column * col : computedColumns())
		if(	col->codeType() != computedColumnType::analysis				&&
			col->codeType() != computedColumnType::analysisNotComputed	&&
			col->iShouldBeSentAgain()
		)
			col->tryAndRunComputedColumn();
}


Column *DataSet::shownColumn() const
{
	return _shownColumn;
}

void DataSet::setShownColumn(Column *newShownColumn)
{
	if (_shownColumn == newShownColumn)
		return;
	
	_workspace->setShownDataSet(this);
	
	_shownColumn = newShownColumn;
	emit shownColumnChanged();
}


void DataSet::writeToOStream(std::ostream & out, bool includeComputed)
{
	std::vector<const Column*> cols;

	//Add a UTF-8 BOM
	out.put(0xEF);
	out.put(0xBB);
	out.put(0xBF);


	for (Column		*	column : columns())
		if(!column->isComputed() || includeComputed)
			cols.push_back(column);
	

	for (size_t i = 0; i < cols.size(); i++)
	{
		const Column *	column	= cols[i];
		std::string		name	= column->name();

		if (stringUtils::escapeValue(name))	out << '"' << name << '"';
		else								out << name;

		if (i < cols.size()-1)	out << ",";
		else					out << "\n";

	}

	size_t		rows = rowCount();
	std::string value;

	for (size_t r = 0; r < rows; r++)
		for (size_t i = 0; i < cols.size(); i++)
		{
			const Column * column = cols[i];

			value = column->getValue(r);
			
			if (value != "")
			{
				if (stringUtils::escapeValue(value))	out << '"' << value << '"';
				else									out << value;
			}

			if (i < cols.size()-1)		out << ",";
			else if (r != rows-1)		out << "\n";
		}
}
