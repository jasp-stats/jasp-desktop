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

#include "enginebase.h"
#include "tempfiles.h"
#include "databaseinterface.h"
#include "columnencoder.h"
#include "rbridge.h"
#include "timers.h"

EngineBase::EngineBase(unsigned long sessionID, bool useMemory)
{
	JASPTIMER_START(TempFiles Attach);
	TempFiles::attach(sessionID);
	JASPTIMER_STOP(TempFiles Attach);

	if(sessionID != 0) //Otherwise we are just running to fix R packages
		_db = new DatabaseInterface(false, useMemory);
}

void EngineBase::provideStateFileName(std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific("state", _analysisId, root, relativePath);
}

void EngineBase::provideJaspResultsFileName(std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific("jaspResults.json", _analysisId, root, relativePath);
}

void EngineBase::provideSpecificFileName(const std::string & specificName, std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific(specificName, _analysisId, root, relativePath);
}

void EngineBase::provideTempFileName(const std::string & extension, std::string & root, std::string & relativePath)
{
	TempFiles::create(extension, _analysisId, root, relativePath);
}

bool EngineBase::isColumnNameOk(const std::string & columnName)
{
	if(columnName == "" || !provideAndUpdateDataSet())
		return false;

	return provideAndUpdateDataSet()->column(columnName);
}

int EngineBase::getColumnType(const std::string &columnName)
{
	return int(!isColumnNameOk(columnName) ? columnType::unknown : provideAndUpdateDataSet()->column(columnName)->type());
}

int EngineBase::getColumnAnalysisId(const std::string &columnName)
{
	return	!isColumnNameOk(columnName)
		? -1
		: provideAndUpdateDataSet()->column(columnName)->analysisId();
}

DataSet * EngineBase::provideAndUpdateDataSet()
{
	JASPTIMER_RESUME(EngineBase::provideAndUpdateDataSet());

	bool setColumnNames = !_dataSet;

	if(!_dataSet && _db->dataSetGetId() != -1)
		_dataSet = new DataSet(_db->dataSetGetId());

	if(_dataSet)
		setColumnNames |= _dataSet->checkForUpdates();

	if(_dataSet && setColumnNames)
		ColumnEncoder::columnEncoder()->setCurrentNames(_dataSet->getColumnNames(), true);

	JASPTIMER_STOP(EngineBase::provideAndUpdateDataSet());

	return _dataSet;
}

std::string EngineBase::createColumn(const std::string &columnName)
{
	if(columnName.empty() || isColumnNameOk(columnName))
		return "";

	DataSet * data = provideAndUpdateDataSet();
	Column  * col  = data->newColumn(columnName);

	col->setAnalysisId(_analysisId);
	col->setCodeType(computedColumnType::analysisNotComputed);

	reloadColumnNames();

	return rbridge_encodeColumnName(columnName.c_str());
}

bool EngineBase::deleteColumn(const std::string &columnName)
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

bool EngineBase::setColumnDataAndType(const std::string &columnName, const std::vector<std::string> &data, columnType colType)
{
	if(!isColumnNameOk(columnName))
		return false;

	return provideAndUpdateDataSet()->column(columnName)->overwriteDataAndType(data, colType);
}

void EngineBase::reloadColumnNames()
{
	ColumnEncoder::columnEncoder()->setCurrentColumnNames(provideAndUpdateDataSet() == nullptr ? std::vector<std::string>({}) : provideAndUpdateDataSet()->getColumnNames());
}





