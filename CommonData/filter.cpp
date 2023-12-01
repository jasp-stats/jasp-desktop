#include "filter.h"
#include "databaseinterface.h"
#include "dataset.h"

Filter::Filter(DataSet *data)
	: DataSetBaseNode(dataSetBaseNodeType::filter, data), _data(data)
{ }

void Filter::dbCreate()
{
	assert(_id == -1);
	_id = db().filterInsert(_data->id(), _rFilter, _generatedFilter, _constructorJson, _constructorR);
}

void Filter::dbUpdate()
{
	JASPTIMER_SCOPE(Filter::dbUpdate);

	assert(_id != -1);

	db().transactionWriteBegin();
	if(!_data->writeBatchedToDB())
		db().filterUpdate(_id, _rFilter, _generatedFilter, _constructorJson, _constructorR);

	incRevision();
	db().transactionWriteEnd();
}

void Filter::dbUpdateErrorMsg()
{
	assert(_id != -1);
	db().transactionWriteBegin();
	if(!_data->writeBatchedToDB())
		db().filterUpdateErrorMsg(_id, _errorMsg);
	incRevision();
	db().transactionWriteEnd();
}

void Filter::dbLoad()
{
	if(_id == -1)
		_id = db().filterGetId(_data->id());

	if(_id == -1)
		return;

	db().transactionReadBegin();
	
	db().filterLoad(_id, _rFilter, _generatedFilter, _constructorJson, _constructorR, _revision);

	_filteredRowCount	= 0;

	db().filterSelect(_id, _filtered);

	for(bool f : _filtered)
		if(f)
			_filteredRowCount++;

	db().transactionReadEnd();
}

bool Filter::setFilterVector(const boolvec & filterResult)
{
	bool changed = false;

	if(_filtered.size() == 0)
	{
		_filtered = filterResult;
		changed = true;
	}
	else
		for(size_t i=0; i<filterResult.size(); i++)
		{
			if(_filtered[i] != filterResult[i])
				changed = true;

			_filtered[i] = filterResult[i];
		}

	_filteredRowCount = 0;

	if(!_data->writeBatchedToDB())
		db().filterWrite(_id, _filtered);

	for(bool row : _filtered)
		if(row)
			_filteredRowCount++;

	if(changed)
		incRevision();

	return changed;
}

void Filter::setFilterValueNoDB(size_t row, bool val)
{
	_filtered[row] = val;
}

void Filter::setRowCount(size_t rows)
{
	_filtered.resize(rows);
}

bool Filter::dbLoadResultAndError()
{
	assert(_id != -1);
	
	_errorMsg = db().filterLoadErrorMsg(_id);
	 bool changed = db().filterSelect(_id, _filtered);

	 _filteredRowCount = 0;
	 for(bool f : _filtered)
		 if(f)
			 _filteredRowCount++;

	 return changed;
}

void Filter::dbDelete()
{
	assert(_id != -1);

	db().filterDelete(_id);
	_id = -1;
}

void Filter::incRevision()
{
	assert(_id != -1);
	
	if(!_data->writeBatchedToDB())
	{
		_revision = db().filterIncRevision(_id);
		checkForChanges();
	}
}

bool Filter::checkForUpdates()
{
	if(_id == -1)
	{
		_id = db().dataSetGetFilter(_data->id());
		
		if(_id == -1)
			return false;
	}
	else if(_revision == db().filterGetRevision(_id))
		return false;

	if(_data->id() != -1 && _id != -1)
	{
		dbLoad();
		return true;
	}
	else
		return false;
}

void Filter::reset()
{
	if(!_data->writeBatchedToDB())
		db().filterClear(_id);

	incRevision();
	_filtered = boolvec(_data->rowCount(), true);
}

DatabaseInterface		& Filter::db()			{ return *DatabaseInterface::singleton(); }
const DatabaseInterface & Filter::db() const	{ return *DatabaseInterface::singleton(); }

