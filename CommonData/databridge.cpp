//
// Copyright (C) 2013-2024 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "databridge.h"
#include "tempfiles.h"
#include "databaseinterface.h"
#include "columnencoder.h"
#include "rbridge.h"
#include "timers.h"

DataBridge::DataBridge(unsigned long sessionID, bool useMemory)
	: _extraEncodings(new ColumnEncoder(ExtraOptionsPrefix))
{
	JASPTIMER_START(TempFiles Attach);
	TempFiles::attach(sessionID);
	JASPTIMER_STOP(TempFiles Attach);

	if(sessionID != 0) //Otherwise we are just running to fix R packages
	{
		//singletonOrNull() and not singleton(): the latter *creates* an interface when there is none, so it
		//can never tell us whether a host already made one and we would always end up deleting somebody
		//else's. See resolveDb() below for why that matters.
		_db = DatabaseInterface::singletonOrNull();				//Borrowed: whoever created it destroys it.

		if(!_db)
		{
			_db		= new DatabaseInterface(false, useMemory);	//Nobody else made one (the engine process), so it is ours.
			_ownsDb	= true;
		}
	}
}

DataBridge::~DataBridge()
{
	if(_ownsWorkspace)
		delete _workspace;

	_workspace		= nullptr;
	_ownsWorkspace	= false;
	
	//Re-resolve before deciding: another owner may have replaced or destroyed it, and then it is not ours.
	DatabaseInterface * db = resolveDb();

	if(db && _ownsDb)
	{
		db->close();
		delete db;
	}

	_db		= nullptr;
	_ownsDb	= false;
}

void DataBridge::provideStateFileName(std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific("state", _analysisId, root, relativePath);
}

void DataBridge::provideJaspResultsFileName(std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific("jaspResults.json", _analysisId, root, relativePath);
}

void DataBridge::provideSpecificFileName(const std::string & specificName, std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific(specificName, _analysisId, root, relativePath);
}

void DataBridge::provideTempFileName(const std::string & extension, std::string & root, std::string & relativePath)
{
	TempFiles::create(extension, _analysisId, root, relativePath);
}

bool DataBridge::isColumnNameOk(const std::string & columnName)
{
	if(columnName == "" || !provideAndUpdateDataSet())
		return false;

	return provideAndUpdateDataSet()->column(columnName);
}

int DataBridge::getColumnType(const std::string &columnName)
{
	return int(!isColumnNameOk(columnName) ? columnType::unknown : provideAndUpdateDataSet()->column(columnName)->type());
}

int DataBridge::getColumnAnalysisId(const std::string &columnName)
{
	return	!isColumnNameOk(columnName)
		? -1
		: provideAndUpdateDataSet()->column(columnName)->analysisId();
}

int DataBridge::getColumnOriginalIndex(const std::string &columnName)
{
	return provideAndUpdateDataSet()->getColumnIndex(columnName);
}

Workspace * DataBridge::resolveWorkspace()
{
	//Workspace is a process-wide singleton. In the engine we are its only owner, but when we run inside
	//a host that already made one (SyntaxInterface via DataSetProvider, the desktop via DataSetPackage)
	//we must use that one: constructing a second Workspace replaces the singleton with an empty one and
	//cuts the R bridge off from the dataset that was actually loaded. There is then no shownDataSet, so
	//ColumnEncoder::setCurrentEncoder() never runs and every rbridge_* call falls back to the empty
	//default encoder. Resolve it on every use rather than caching, because an owner may swap it out
	//from under us (DataSetProvider::resetDataSet() deletes and recreates it on each loadDataSet).
	if(Workspace::singleton() != _workspace)
	{
		_workspace		= Workspace::singleton();	//Borrowed: whoever created it destroys it.
		_ownsWorkspace	= false;
	}

	if(!_workspace)
	{
		_workspace		= new Workspace();			//Nobody else made one (the engine process), so it is ours.
		_ownsWorkspace	= true;
	}

	return _workspace;
}

DatabaseInterface * DataBridge::resolveDb()
{
	//DatabaseInterface is a process-wide singleton and ownership of it is not ours to assume. In the engine
	//we are the one who makes it, but when we run inside a host that already made one (SyntaxInterface via
	//DataSetProvider, the desktop via DataSetPackage) it belongs to that host and must outlive us: deleting
	//it here leaves that owner deleting freed memory on its own way out. And because ~DatabaseInterface()
	//clears the singleton, the next DatabaseInterface::singleton() quietly opens a fresh interface instead
	//of complaining, so an in-memory db silently loses everything that was loaded into it. Resolve it on
	//every use rather than trusting the cached pointer, because an owner may swap it out from under us
	//(DataSetProvider::getProvider() destroys and recreates the provider, interface and all, whenever the
	//in-memory flag flips, and DatabaseInterface::closeInterfaces() deletes the singleton outright).
	if(DatabaseInterface::singletonOrNull() != _db)
	{
		_db		= DatabaseInterface::singletonOrNull();	//Somebody replaced or destroyed what we were pointing at, so it was never ours to free.
		_ownsDb	= false;
	}

	return _db;
}

DataSet * DataBridge::provideAndUpdateDataSet(int dataSetId, std::function<void(float)> progressCallback)
{
	JASPTIMER_RESUME(DataBridge::provideAndUpdateDataSet());
	
	resolveWorkspace();

	_workspace->checkForUpdates(progressCallback);
	
	if(dataSetId != -1)
		_workspace->setShownDataSet(dataSetId);
			
	if(_workspace->shownDataSet())
	{
		DataSet * ds = _workspace->shownDataSet();
		//Column-name encoding for the R bridge is scoped to the *current request's* shown dataset.
		//Every rbridge_* entry runs provideAndUpdateDataSet() first, and DataSet::setShownDataSet()
		//(desktop) / this re-point (engine) keep the indirection authoritative. Also see the
		//EngineBridgeCallbacks/DataSet guard in ColumnEncoder::setCurrentEncoder / ~DataSet.
		ColumnEncoder::setCurrentEncoder(&ds->encoder());
		ds->encoder().setCurrentNames(ds->getColumnTypesMap());
	}
	
	JASPTIMER_STOP(DataBridge::provideAndUpdateDataSet());

	return _workspace->shownDataSet();
}

std::string DataBridge::createColumn(const std::string &columnName, bool computed)
{
	if(columnName.empty() || isColumnNameOk(columnName))
		return "";

	DataSet * data = provideAndUpdateDataSet();
	Column  * col  = data->createColumn(columnName);

	col->setAnalysisId(_analysisId);
	col->setCodeType(computed ? computedColumnType::analysis : computedColumnType::analysisNotComputed);

	reloadColumnNames();

	return rbridge_encodeColumnName(columnName.c_str());
}

bool DataBridge::deleteColumn(const std::string &columnName)
{
	if(!isColumnNameOk(columnName))
		return false;

	DataSet * data = provideAndUpdateDataSet();
	Column  * col  = data->column(columnName);

	if(col->analysisId() != _analysisId)
		return false;

	data->removeColumn(columnName);

	reloadColumnNames();

	return true;
}

bool DataBridge::setColumnDataAndType(const std::string &columnName, const std::vector<std::string> &data, columnType colType, bool computed)
{
	if(!isColumnNameOk(columnName))
		return false;

	return provideAndUpdateDataSet()->column(columnName)->overwriteDataAndType(data, colType, computed);
}

bool DataBridge::setDataSet(const std::string & datasetName, const std::vector<std::string> & columnNames, const std::vector<columnType> & columnTypes, const std::vector<std::vector<std::string>> & columnData)
{
	DataSet * ds = resolveWorkspace()->dataSetByName(datasetName);

	if(!ds)
		return false;

	size_t	colCount	= columnNames.size(),
			rowCount	= 0;

	for(const auto & col : columnData)
		rowCount = std::max(rowCount, col.size());

	//Replace the current contents of the (computed) output dataset wholesale.
	while(ds->columnCount() > 0)
		ds->removeColumn(0);

	ds->setRowCount(rowCount);

	//insertColumns starts colIdx at 0 (createColumn would leave an off-by-one gap on an empty dataset).
	ds->insertColumns(size_t(0), colCount);

	for(size_t i=0; i<colCount; i++)
	{
		Column * col = ds->column(i);

		col->setName(columnNames[i]);
		col->setDefaultValues(columnTypes[i], false);
		col->setValues(columnData[i], columnData[i], 0);
		col->setType(columnTypes[i]);
	}

	ds->incRevision();

	return true;
}

void DataBridge::reloadColumnNames()
{
	DataSet * ds = provideAndUpdateDataSet();
	if(ds)
		ds->encoder().setCurrentNames(ds->getColumnTypesMap());
}

void DataBridge::updateOptionsAccordingToMeta(Json::Value & encodedOptions)
{
	JASPTIMER_SCOPE(DataBridge::updateOptionsAccordingToMeta);

	std::function<void(Json::Value&,Json::Value&)> recursiveUpdate;
	recursiveUpdate = [&recursiveUpdate, this](Json::Value & options, Json::Value & meta)
	{
		if(meta.isNull())
			return;

		Json::Value loadFilteredData = !meta.isObject() || !meta.isMember("loadFilteredData") ? Json::nullValue : meta["loadFilteredData"];

		switch(options.type())
		{
		case Json::arrayValue:
			for(int i=0; i<options.size() && i < meta.size(); i++)
				recursiveUpdate(options[i], meta.type() == Json::arrayValue ? meta[i] : meta);

			return;

		case Json::objectValue:
			for(const std::string & memberName : options.getMemberNames())
				if(memberName != ".meta" && meta.isMember(memberName))
					recursiveUpdate(options[memberName], meta[memberName]);

			if(loadFilteredData.isObject())
			{
				const std::string	colName = loadFilteredData["column"].asString(),
									filterN	= loadFilteredData["filter"].asString();
				DataSet			*	data	= provideAndUpdateDataSet();
				Column			*	col		= data->column(colName);

				if(!col)
					return;

				Filter			*	filter	= data->filter(filterN);

				if(col && filter)
				{
					Json::Value rowIndices	= Json::arrayValue,
								values		= Json::arrayValue;
					doublevec	dbls		= col->dataAsRDoubles({}); //We dont pass a filter because we need to know the rowindices.

					for(size_t r=0; r<dbls.size(); r++)
						if(filter->filtered()[r])
						{
							rowIndices	.append(int(r+1));
							values		.append(dbls[r]);
						}

					options["rowIndices"]	= rowIndices;
					options["values"]		= values;
				}
			}
			return;

		default:
			return;
		}
	};

	recursiveUpdate(encodedOptions, encodedOptions[".meta"]);


	//Log::log() << "After updating options according to their meta it is now:\n" << encodedOptions << std::endl;
}



