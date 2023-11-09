#include "dataset.h"
#include "log.h"
#include <regex>
#include "databaseinterface.h"

DataSet::DataSet(int index)
	: DataSetBaseNode(dataSetBaseNodeType::dataSet, nullptr)
{
	_dataNode		= new DataSetBaseNode(dataSetBaseNodeType::data,	this);
	_filtersNode	= new DataSetBaseNode(dataSetBaseNodeType::filters, this);
	
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

	delete _dataNode;
	_dataNode = nullptr;
	
	delete _filter;
	_filter = nullptr;
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

void DataSet::endBatchedToDB(std::function<void(float)> progressCallback)
{
	assert(_writeBatchedToDB);
	_writeBatchedToDB = false;

	db().dataSetBatchedValuesUpdate(this, progressCallback);
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
			_columns.erase(col);

			incRevision();

			return;
		}
}

void DataSet::insertColumn(size_t index)
{

	assert(_dataSetID > 0);

	Column * newColumn = new Column(this, db().columnInsert(_dataSetID, index));

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

size_t DataSet::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	if(columnIndex >= columnCount())
		return 0;

	const Column * col = _columns[columnIndex];

	int extraPad = 4;

	switch(col->type())
	{
	case columnType::scale:
		return 11 + extraPad; //default precision of stringstream is 6 (and sstream is used in displaying scale values) + 3 because Im seeing some weird stuff with exp-notation  etc + some padding because of dots and whatnot

	case columnType::unknown:
		return 0;

	default:
	{
		int tempVal = 0;

		for(Label * label : col->labels())
			tempVal = std::max(tempVal, static_cast<int>(label->label(true).length()));

		return tempVal + extraPad;
	}
	}

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
	_dataSetID	= db().dataSetInsert(_dataFilePath, _description, _databaseJson, _emptyValues.toJson().toStyledString(), _dataFileSynch);
	_filter = new Filter(this);
	_filter->dbCreate();
	_columns.clear();

	db().transactionWriteEnd();

	_rowCount		= 0;
}

void DataSet::dbUpdate()
{
	assert(_dataSetID > 0);
	db().dataSetUpdate(_dataSetID, _description, _dataFilePath, _databaseJson, _emptyValues.toJson().toStyledString(), _dataFileSynch);
	incRevision();
}

void DataSet::dbLoad(int index, const Version& loadedJaspVersion, std::function<void(float)> progressCallback)
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

	db().dataSetLoad(_dataSetID, _description, _dataFilePath, _databaseJson, emptyVals, _revision, _dataFileSynch);
	progressCallback(0.1);

	if(!_filter)
		_filter = new Filter(this);
	_filter->dbLoad();
	progressCallback(0.2);

	int colCount = db().dataSetColCount(_dataSetID);
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
	setEmptyValuesJson(emptyValsJson, false);
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

	if(colCount > curCount)
		for(size_t i=curCount; i<colCount; i++)
			insertColumn(i);

	else if(colCount < curCount)
		for(size_t i=curCount-1; i>=colCount; i--)
			removeColumn(i);
	
	incRevision();

	db().transactionWriteEnd();
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

bool DataSet::checkForUpdates(stringvec * colsChanged)
{
	JASPTIMER_SCOPE(DataSet::checkForUpdates);

	if(_dataSetID == -1)
		return false;

	if(_revision != db().dataSetGetRevision(_dataSetID))
	{
		dbLoad();
		if(colsChanged)
		{
			colsChanged->clear();
			for(Column * col : _columns)
				colsChanged->push_back(col->name());
		}
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

		return somethingChanged;
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
	for(const Json::Value & colJson : json)
	{
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

std::map<std::string, intstrmap> DataSet::resetMissingData(const std::vector<Column*>& cols)
{
	std::map<std::string, intstrmap> colChanged;

	for (Column * col : cols)
	{
		intstrmap missingDataMap = _emptyValues.missingData(col->name());

		if (col->resetMissingData(missingDataMap))
			colChanged[col->name()] = missingDataMap;
	}

	incRevision();

	return colChanged;
}

void DataSet::setEmptyValuesJson(const Json::Value &emptyValues, bool updateDB)
{
	if (!emptyValues.isMember("workspaceEmptyValues"))
	{
		// For backword compatibility, if the workspaceEmptyValues are not specified, take the defdault ones
		Json::Value updatedEmptyValues = emptyValues;
		Json::Value emptyValuesJson(Json::arrayValue);
		for (const std::string& val : _defaultEmptyvalues)
			emptyValuesJson.append(val);
		updatedEmptyValues["workspaceEmptyValues"] = emptyValuesJson;
		_emptyValues.fromJson(updatedEmptyValues);
	}
	else
		_emptyValues.fromJson(emptyValues);

	if (updateDB)
		dbUpdate();
}

void DataSet::setWorkspaceEmptyValues(const stringset &values)
{
	_defaultEmptyvalues = values;
	_emptyValues.setWorkspaceEmptyValues(values);
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
	//sort of based on rbridge_encodeColumnNamesToBase64
	static std::regex nonNameChar("[^\\.A-Za-z0-9]");
	std::set<std::string> columnsFound;
	size_t foundPos = -1;

	for(Column * column : _columns)
	{
		const std::string & col = column->name();
		
		while((foundPos = searchThis.find(col, foundPos + 1)) != std::string::npos)
		{
			size_t foundPosEnd = foundPos + col.length();
			//First check if it is a "free columnname" aka is there some space or a kind in front of it. We would not want to replace a part of another term (Imagine what happens when you use a columname such as "E" and a filter that includes the term TRUE, it does not end well..)
			bool startIsFree	= foundPos == 0							|| std::regex_match(searchThis.substr(foundPos - 1, 1),	nonNameChar);
			bool endIsFree		= foundPosEnd == searchThis.length()	|| (std::regex_match(searchThis.substr(foundPosEnd, 1),	nonNameChar) && searchThis[foundPosEnd] != '('); //Check for "(" as well because maybe someone has a columnname such as rep or if or something weird like that

			if(startIsFree && endIsFree)
			{
				columnsFound.insert(col);
				searchThis.replace(foundPos, col.length(), ""); // remove the found entry
			}

		}
	}

	return columnsFound;
}
