#include "log.h"
#include <regex>
#include "timers.h"
#include "dataset.h"
#include "columnencoder.h"
#include "jsonutilities.h"
#include "databaseinterface.h"

stringset DataSet::_defaultEmptyvalues;

DataSet::DataSet(int index)
	: DataSetBaseNode(dataSetBaseNodeType::dataSet, nullptr)
{
	Log::log() << "DataSet::DataSet(index=" << index << ")" << std::endl;

	_dataNode		= new DataSetBaseNode(dataSetBaseNodeType::data,	this);
	_filtersNode	= new DataSetBaseNode(dataSetBaseNodeType::filters, this);
	_emptyValues	= new EmptyValues();
	
	if(index == -1)         dbCreate();
	else if(index > 0)		dbLoad(index);
}

DataSet::~DataSet()
{
	JASPTIMER_SCOPE(DataSet::~DataSet);
	//delete columns before dataNode as they depend on it via DataSetBaseNode inheritance
	for(Column * col : _columns)
		delete col;

	_columns.clear();
	
	delete _emptyValues;
	delete _dataNode;
	delete _filter;
	
	_emptyValues	= nullptr;
	_dataNode		= nullptr;
	_filter			= nullptr;
	
	
}

void DataSet::dbDelete()
{
	JASPTIMER_SCOPE(DataSet::dbDelete);

	assert(_dataSetID != -1);

	db().transactionWriteBegin();

	if(_filter && _filter->id() != -1)
		_filter->dbDelete();
	_filter = nullptr;

	for(Column * col : _columns)
		col->dbDelete(false);

	db().dataSetDelete(_dataSetID);

	_dataSetID = -1;

	
	db().transactionWriteEnd();
}

void DataSet::beginBatchedToDB()
{
	assert(!_writeBatchedToDB);
	_writeBatchedToDB = true;
}

void DataSet::endBatchedToDB(std::function<void(float)> progressCallback, Columns columns)
{
	assert(_writeBatchedToDB);
	_writeBatchedToDB = false;
	
	if(columns.size() == 0)
		columns = _columns;

	db().dataSetBatchedValuesUpdate(this, columns, progressCallback);
	incRevision(); //Should trigger reload at engine end
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

void DataSet::columnsReorder(const stringvec &order)
{
	assert(order.size() == _columns								.size());
	
	stringset	orderSet(order.begin(), order.end()),
				colSet;
	
	assert(order.size() == orderSet.size());
	
	std::map<std::string, Column*> nameColMap;
	
	for(Column * col : _columns)
	{
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
}

Column *DataSet::column(const std::string &name)
{
	for(Column * column : _columns)
		if(column->name() == name)
			return column;

	return nullptr;
}

Column *DataSet::column(size_t index)
{
	if(index < 0 || index >= _columns.size())
		return nullptr;

	return _columns[index];
}

void DataSet::removeColumn(size_t index)
{
	assert(_dataSetID > 0);

	Column * removeMe = _columns[index];
	_columns.erase(_columns.begin() + index);

	removeMe->dbDelete();
	delete removeMe;

	incRevision();
}

void DataSet::removeColumn(const std::string & name)
{
	assert(_dataSetID > 0);

	for(auto col = _columns.begin() ; col != _columns.end(); col++)
		if((*col)->name() == name)
		{
			(*col)->dbDelete();
			delete *col;
			
			_columns.erase(col);
				
			incRevision();
			return;
		}
}

void DataSet::insertColumn(size_t index,	bool alterDataSetTable)
{

	assert(_dataSetID > 0);

	Column * newColumn = new Column(this, db().columnInsert(_dataSetID, index, "", columnType::unknown, alterDataSetTable));

	_columns.insert(_columns.begin()+index, newColumn);

	newColumn->setRowCount(_rowCount);

	incRevision();
}

Column * DataSet::newColumn(const std::string &name)
{
	assert(_dataSetID > 0);
	Column * col = new Column(this, db().columnInsert(_dataSetID, -1, name));
	col->setName(name);

	_columns.push_back(col);

	incRevision();

	return col;
}

qsizetype DataSet::getMaximumColumnWidthInCharacters(size_t columnIndex) const
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

void DataSet::dbCreate()
{
	JASPTIMER_SCOPE(DataSet::dbCreate);

	assert(!_filter && _dataSetID == -1);

	db().transactionWriteBegin();

	//The variables are probably empty though:
	_dataSetID	= db().dataSetInsert(_dataFilePath, _dataFileTimestamp, _description, _databaseJson, _emptyValues->toJson().toStyledString(), _dataFileSynch);
	_filter = new Filter(this);
	_filter->dbCreate();
	_columns.clear();

	db().transactionWriteEnd();

	_rowCount		= 0;
}

void DataSet::dbUpdate()
{
	assert(_dataSetID > 0);
	db().dataSetUpdate(_dataSetID, _dataFilePath, _dataFileTimestamp, _description, _databaseJson, _emptyValues->toJson().toStyledString(), _dataFileSynch);
	incRevision();
}

void DataSet::dbLoad(int index, std::function<void(float)> progressCallback, bool do019Fix)
{
	//Log::log() << "loadDataSet(index=" << index << "), _dataSetID="<< _dataSetID <<";" << std::endl;

	JASPTIMER_SCOPE(DataSet::dbLoad);

	assert(_dataSetID == -1 || _dataSetID == index || (_dataSetID != -1 && index == -1));

	if(index != -1 && !db().dataSetExists(index))
	{
		Log::log() << "No DataSet with id " << index << "!" << std::endl;
		return;
	}
		
	if(index != -1)
		_dataSetID	= index;

	assert(_dataSetID > 0);

	std::string emptyVals;

	db().dataSetLoad(_dataSetID, _dataFilePath, _dataFileTimestamp, _description, _databaseJson, emptyVals, _revision, _dataFileSynch);
	progressCallback(0.1);

	if(!_filter)
		_filter = new Filter(this);
	_filter->dbLoad();
	progressCallback(0.2);

	int colCount	= db().dataSetColCount(_dataSetID);
	_rowCount		= db().dataSetRowCount(_dataSetID);
	//Log::log() << "colCount: " << colCount << ", " << "rowCount: " << rowCount() << std::endl;

	float colProgressMult = 1.0 / colCount;
			
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

	db().dataSetBatchedValuesLoad(this, [&](float p){ progressCallback(0.5 + p * 0.5); });
	
	Json::Value emptyValsJson;
	Json::Reader().parse(emptyVals, emptyValsJson);
	
	if(do019Fix)	upgradeTo019(emptyValsJson);
	else			_emptyValues->fromJson(emptyValsJson);
}

void DataSet::upgradeTo019(const Json::Value & emptyVals)
{
	for(Column * column : _columns)
	{
		switch(column->type())
		{
		case columnType::scale:
			column->upgradeSetDoubleLabelsInInts();
			break;
		
		case columnType::ordinal:
		case columnType::nominal:
		case columnType::nominalText:
			column->upgradeExtractDoublesIntsFromLabels();
			break;
			
		default:
			Log::log() << "Column " << column->name() << " has unknown type, id: " << column->id() << std::endl;
			break;
		}
	}
	
	//So, 0.18.0, 0.18.1, 0.18.2 jaspfiles cant be loaded in 0.18.3
	//also, those versions were pretty buggy, so here we will just try to handle the case of 0.18.3
	//above we made sure _ints and _dbls are synched again.
	//now we will extract the missing data map and turn it into emptyvalues and proper values
	
	// The emptyValues json contains
	const Json::Value	& emptyValuesPerColumn = emptyVals["emptyValuesPerColumn"], // object, names=columnnames: array of empty value strings
						& missingDataPerColumn = emptyVals["missingDataPerColumn"], // object, names=columnnames: object { "row#": "original display" }
						& workspaceEmptyValues = emptyVals["workspaceEmptyValues"]; // array of empty value strings
	
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
		}
	}
	
	_emptyValues->setEmptyValues(workspaceEmpty);
	incRevision();
}

int DataSet::columnCount() const
{
	return _columns.size();
}

int DataSet::rowCount() const
{
	return _rowCount;
}

void DataSet::setColumnCount(size_t colCount)
{
	db().transactionWriteBegin();

	int curCount = columns().size();
	
	bool alterTableAfterwards = curCount == 0 && colCount > 0;

	if(colCount > curCount)
		for(size_t i=curCount; i<colCount; i++)
			insertColumn(i, !alterTableAfterwards);

	else if(colCount < curCount)
		for(size_t i=curCount-1; i>=colCount; i--)
			removeColumn(i);
	

	incRevision();

	db().transactionWriteEnd();
	
	if(alterTableAfterwards)
		db().dataSetCreateTable(this);
}

void DataSet::setRowCount(size_t rowCount)
{
	_rowCount = rowCount; //Make sure we do set the rowCount variable here so the batch can easily see how big it ought to be in DatabaseInterface::dataSetBatchedValuesUpdate

	if(!writeBatchedToDB())
	{
		db().dataSetSetRowCount(_dataSetID, rowCount);
		dbLoad(); //Make sure columns have the right data in them
	}

	_filter->reset();
}

void DataSet::incRevision()
{
	assert(_dataSetID != -1);

	if(!writeBatchedToDB())
	{
		_revision = db().dataSetIncRevision(_dataSetID);
		checkForChanges();
	}
}

bool DataSet::checkForUpdates(stringvec * colsChanged, stringvec * colsRemoved, bool * newColumns, bool * rowCountChanged)
{
	JASPTIMER_SCOPE(DataSet::checkForUpdates);

	if(_dataSetID == -1)
		return false;
	
	stringset prevCols;
	for(Column * col : _columns)
		prevCols.insert(col->name());
	
	size_t rowCountPrev = rowCount();
	
		
	if(_revision != db().dataSetGetRevision(_dataSetID))
	{
		dbLoad();
		
		if(newColumns)
			(*newColumns) = prevCols.size() < _columns.size();
		
		if(rowCountChanged)
			(*rowCountChanged) = rowCountPrev != rowCount();
		
		if(colsChanged)
		{
			colsChanged->clear();
			for(Column * col : _columns)
			{
				colsChanged->push_back(col->name());
				prevCols.erase(col->name());
			}
		}
		
		if(colsRemoved)
			(*colsRemoved) = stringvec(prevCols.begin(), prevCols.end());
		
		return true;
	}
	else
	{
		bool somethingChanged = _filter->checkForUpdates();

		for(Column * col : _columns)
			if(col->checkForUpdates())
			{
				somethingChanged = true;

				if(colsChanged)
					colsChanged->push_back(col->name());
			}
		
		if(colsRemoved)
			colsRemoved->clear();
		
		if(newColumns)
			(*newColumns) = false;
		
		if(rowCountChanged)
			(*rowCountChanged) = rowCountPrev != rowCount();

		return somethingChanged || (rowCountChanged && *rowCountChanged);
	}
}

const Columns & DataSet::computedColumns() const
{
	static Columns computedColumns;

	computedColumns.clear();

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
		Log::log() << "Old computed column: " << colJson.toStyledString() << std::endl;
		if (!colJson.isObject() || colJson["error"].asString().rfind("The engine crashed", 0) == 0) continue;

		const std::string name = colJson["name"].asString();

		Column * col = column(name);

		if(!col && !name.empty())
			col = newColumn(name);

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

void DataSet::setWorkspaceEmptyValues(const stringset &values)
{
	_emptyValues->setEmptyValues(values);
	for(Column * column : _columns)
		column->labelsTempReset();
	dbUpdate();
}

void DataSet::setDescription(const std::string &desc)
{
	_description = desc;
	dbUpdate();
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
	ColumnEncoder::columnEncoder()->encodeRScript(searchThis, &columnsWithTypeFound);
	
	//The found columns now also include the type, but we dont really care about that right now.
	//Instead we'll make use of the encode->decode not being symmetrical (for the results to be less ugly) and dropping the type
	
	for(const std::string & colPlusType : columnsWithTypeFound)
		columnsFound.insert(ColumnEncoder::columnEncoder()->decode(ColumnEncoder::columnEncoder()->encode(colPlusType)));
	
	return columnsFound;
}

bool DataSet::initColumnWithStrings(int colIndex, const std::string & newName, const stringvec &values, const stringvec & labels, const std::string & title, columnType desiredType, const stringset & emptyValues, int threshold, bool orderLabelsByValue)
{
	Column	*	column			=	columns()[colIndex];
				column			->	setHasCustomEmptyValues(emptyValues.size());
				column			->	setCustomEmptyValues(emptyValues);
				column			->	setName(newName);
				column			->	setTitle(title);
				column			->	beginBatchedLabelsDB();
	bool		anyChanges		=	title != column->title() || newName != column->name();
	columnType	prevType		=	column->type(),
				suggestedType	=	column->setValues(values, labels,	threshold, &anyChanges);  //If less unique integers than the thresholdScale then we think it must be ordinal: https://github.com/jasp-stats/INTERNAL-jasp/issues/270
				column			->	setType(column->type() != columnType::unknown ? column->type() : desiredType == columnType::unknown ? suggestedType : desiredType);
				column			->	endBatchedLabelsDB();

	if(orderLabelsByValue)
		column->labelsOrderByValue();

	return anyChanges || column->type() != prevType;
}

