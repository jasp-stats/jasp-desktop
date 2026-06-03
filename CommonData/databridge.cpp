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
		_db = DatabaseInterface::singleton();
		if (!_db)
			_db = new DatabaseInterface(false, useMemory);
	}
}

DataBridge::~DataBridge()
{
	delete _dataSet;
	_dataSet = nullptr;
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

DataSet * DataBridge::provideAndUpdateDataSet()
{
	JASPTIMER_RESUME(DataBridge::provideAndUpdateDataSet());

	bool setColumnNames = !_dataSet;

	if(!_dataSet && _db->dataSetGetId() == 1 && _db->tableExists(_db->dataSetName(1)))
		_dataSet = new DataSet(_db->dataSetGetId());

	if(_dataSet)
		setColumnNames |= _dataSet->checkForUpdates();

	if(_dataSet && setColumnNames)
		ColumnEncoder::columnEncoder()->setCurrentNames(_dataSet->getColumnTypesMap());
	
	if(_dataSet && _datasetProvidedCallback)
		_datasetProvidedCallback();

	JASPTIMER_STOP(DataBridge::provideAndUpdateDataSet());

	return _dataSet;
}

std::string DataBridge::createColumn(const std::string &columnName, bool computed)
{
	if(columnName.empty() || isColumnNameOk(columnName))
		return "";

	DataSet * data = provideAndUpdateDataSet();
	Column  * col  = data->newColumn(columnName);

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

void DataBridge::reloadColumnNames()
{
	ColumnEncoder::columnEncoder()->setCurrentColumnNames(		provideAndUpdateDataSet() == nullptr ? std::map<std::string, columnType>({})			: provideAndUpdateDataSet()->getColumnTypesMap());
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

				Filter			*	filter	= new Filter(data, filterN, false);

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
				delete filter;
			}
			return;

		default:
			return;
		}
	};

	recursiveUpdate(encodedOptions, encodedOptions[".meta"]);


	//Log::log() << "After updating options according to their meta it is now:\n" << encodedOptions << std::endl;
}



