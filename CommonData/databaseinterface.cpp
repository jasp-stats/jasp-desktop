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
#include "databaseinterface.h"
#include "columnutils.h"
#include "columntype.h"
#include "tempfiles.h"
#include <sqlite3.h>
#include "version.h"
#include "dataset.h"
#include "timers.h"
#include <thread>
#include "utils.h"
#include <cassert>
#include "log.h"

DatabaseInterface * DatabaseInterface::_singleton	= nullptr;

thread_local 	int			_transactionWriteDepth	= 0,
							_transactionReadDepth	= 0;


//#define SIR_LOG_A_LOT

const std::string DatabaseInterface::_dbConstructionSql =
// The actual definition can be found in "internalDbDefinition.sql"!
#include "internalDbDefinition.h"
;

const std::string DatabaseInterface::_dbIndexesSql =
#include "createIndexes.h"
;

void DatabaseInterface::upgradeDBFromVersion(Version originalVersion)
{
	transactionWriteBegin();

	if((originalVersion < "0.18.2") && !tableHasColumn("DataSets", "description"))
		runStatements("ALTER TABLE DataSets ADD COLUMN description     TEXT;");

	if((originalVersion < "0.19.0") && tableHasColumn("Columns", "isComputed"))
		runStatements("ALTER TABLE Columns  DROP 	COLUMN isComputed;");		// was removed in 0.18.3
	
	if(originalVersion <= "0.19.0")
	{
		if (!tableHasColumn("Columns", "emptyValuesJson"))
			runStatements("ALTER TABLE Columns  ADD 	COLUMN emptyValuesJson		TEXT;");

		if (!tableHasColumn("Columns", "forceSourceColType"))
			runStatements("ALTER TABLE Columns  ADD 	COLUMN forceSourceColType	INT NULL;");

		if (!tableHasColumn("Columns", "autoSortByValue"))
			runStatements("ALTER TABLE Columns  ADD 	COLUMN autoSortByValue		INT;");

		if (!tableHasColumn("DataSets", "dataFileTimestamp"))
			runStatements("ALTER TABLE DataSets  ADD 	COLUMN dataFileTimestamp	INT;");
	}
	
	if(originalVersion <= "0.19.2")
	{
		if (tableHasColumn("Columns", "forceSourceColType"))
			runStatements("ALTER TABLE Columns  DROP 	COLUMN forceSourceColType;");
	}

	if(originalVersion < "0.19.2" && !tableHasColumn("Filters", "name"))	
		runStatements("ALTER TABLE Filters  ADD COLUMN name		TEXT;");
	
	if(originalVersion <= "0.19.3")
	{
		if(!tableHasColumn("Columns", "dropLevels"))
		{
			runStatements("ALTER TABLE Columns  ADD COLUMN dropLevels		INT;");
			runStatements("UPDATE Columns SET dropLevels = 1;"); //Previously dropLevels was always on, so loading an older jasp-file should have this enabled
		}
		
		runStatements(std::string("UPDATE Filters SET name = '") + DEFAULT_FILTER_NAME + "' WHERE trim(name) = '' OR name IS NULL;"); //Previously the "default filter" didnt have a name, but this is actually not very practical for computeFilter, so lets set it to something on load. Filters will always have a name now.

		if(!tableHasColumn("Columns", "computeFilter"))
			runStatements("ALTER TABLE Columns  ADD COLUMN computeFilter		TEXT DEFAULT \"\";");
		
		if(!tableHasColumn("Labels", "userAdded"))
			runStatements("ALTER TABLE Labels  ADD COLUMN userAdded	INT DEFAULT 0;");
		
		//Create indexes cause they dont exist yet
		runStatements(_dbIndexesSql);
	}

	if(originalVersion <= "0.95.4")
	{
		if(!tableHasColumn("DataSets", "showRSyntax"))
			runStatements("ALTER TABLE DataSets  ADD COLUMN showRSyntax	INT;");
	}
	
	if(originalVersion < "0.97.0")
	{
		if(!tableHasColumn("Columns", "hasLabels"))
			runStatements(
				"ALTER TABLE Columns  ADD COLUMN hasLabels		INT DEFAULT 0;\n"
				"UPDATE Columns SET hasLabels=1;" //Make sure old columns all "hasLabels" enabled
			);
		
		
		if(!tableHasColumn("DataSets", "csvDelimiter"))
			runStatements("ALTER TABLE DataSets ADD COLUMN csvDelimiter INT DEFAULT 0;");
	}


	transactionWriteEnd();
}

DatabaseInterface::DatabaseInterface(bool createDb, bool inMemory)
	: _inMemory{inMemory}
{
	assert(!_singleton);
	_singleton = this;
	
	if(createDb)	create();
	else			load();
}

DatabaseInterface::~DatabaseInterface()
{
	close();

	_singleton = nullptr;
}


int DatabaseInterface::dataSetInsert(const std::string & dataFilePath, long dataFileTimestamp, const std::string & description, const std::string & databaseJson, const std::string & emptyValuesJson, bool dataSynch, bool showRSyntax, char csvDelimiter)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetInsert);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, dataFilePath.c_str(),	dataFilePath.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, dataFileTimestamp);
		sqlite3_bind_text(stmt, 3, description.c_str(),		description.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, databaseJson.c_str(),	databaseJson.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, emptyValuesJson.c_str(), emptyValuesJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	6, dataSynch);
		sqlite3_bind_int(stmt,	7, showRSyntax);
		sqlite3_bind_int(stmt,	8, csvDelimiter);
	};

	transactionWriteBegin();
	int id = runStatementsId("INSERT OR REPLACE INTO DataSets (dataFilePath, dataFileTimestamp, description, databaseJson, emptyValuesJson, dataFileSynch, showRSyntax, csvDelimiter, id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1) RETURNING id;", prepare);
	runStatements("CREATE TABLE " + dataSetName(id) + " (rowNumber INTEGER PRIMARY KEY);"); // Can be overwritten through dataSetCreateTable
	transactionWriteEnd();

	return id;
}

void DatabaseInterface::dataSetUpdate(int dataSetId,	const std::string & dataFilePath, long dataFileTimestamp, const std::string & description, const std::string & databaseJson, const std::string & emptyValuesJson, bool dataSynch, bool showRSyntax, char csvDelimiter)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetUpdate);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, dataFilePath.c_str(),	dataFilePath.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, dataFileTimestamp);
		sqlite3_bind_text(stmt, 3, description.c_str(),		description.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, databaseJson.c_str(),	databaseJson.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, emptyValuesJson.c_str(), emptyValuesJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	6, dataSynch);
		sqlite3_bind_int(stmt,	7, showRSyntax);
		sqlite3_bind_int(stmt,	8, csvDelimiter);
		sqlite3_bind_int(stmt,	9, dataSetId);
	};

	//Log::log() << "UPDATE DataSet " << dataSetId << " with Empty Values: " << emptyValuesJson << std::endl;

	runStatements("UPDATE DataSets SET dataFilePath=?, dataFileTimestamp=?, description=?, databaseJson=?, emptyValuesJson=?, dataFileSynch=?, showRSyntax=?, csvDelimiter=?, revision=revision+1 WHERE id = ?;", prepare);
}

void DatabaseInterface::dataSetLoad(int dataSetId, std::string & dataFilePath, long & dataFileTimestamp, std::string & description, std::string & databaseJson, std::string & emptyValuesJson, int & revision, bool & dataSynch, bool & showRSyntax, char & csvDelimiter)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetLoad);

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, dataSetId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 9);

		dataFilePath		= _wrap_sqlite3_column_text(stmt, 0);
		dataFileTimestamp	= sqlite3_column_int(		stmt, 1);
		description			= _wrap_sqlite3_column_text(stmt, 2);
		databaseJson		= _wrap_sqlite3_column_text(stmt, 3);
		emptyValuesJson		= _wrap_sqlite3_column_text(stmt, 4);
		revision			= sqlite3_column_int(		stmt, 5);
		dataSynch			= sqlite3_column_int(		stmt, 6);
		showRSyntax			= sqlite3_column_int(		stmt, 7);
		csvDelimiter		= static_cast<char>(sqlite3_column_int(stmt, 8));

		//Log::log() << "Output loadDataset(dataSetId="<<dataSetId<<") had (dataFilePath='"<<dataFilePath<<"', databaseJson='"<<databaseJson<<"', emptyValuesJson='"<<emptyValuesJson<<"')" << std::endl;
	};

	runStatements("SELECT dataFilePath, dataFileTimestamp, description, databaseJson, emptyValuesJson, revision, dataFileSynch, showRSyntax, csvDelimiter FROM DataSets WHERE id = ?;", prepare, processRow);
}

int DatabaseInterface::dataSetColCount(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetColCount);
	return singleton()->runStatementsId("SELECT COUNT(id) FROM Columns WHERE dataSet="+std::to_string(dataSetId));
}

int DatabaseInterface::dataSetRowCount(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetRowCount);
	return singleton()->runStatementsId("SELECT COUNT(*) FROM DataSet_"+std::to_string(dataSetId));
}

void DatabaseInterface::dataSetSetRowCount(int dataSetId, size_t rowCount)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetSetRowCount);
	transactionWriteBegin();

	int curCount = dataSetRowCount(dataSetId);

	const std::string DS = dataSetName(dataSetId);

	if(rowCount >= curCount)
	{
		const std::string & insertDefaultVals = "INSERT INTO "+DS+" DEFAULT VALUES;";

		_runStatementsRepeatedly(insertDefaultVals, 
			[&](bindParametersType ** bindParam, size_t row)
			{ 
				(*bindParam) = nullptr; 
				return curCount + row < rowCount; 
			});
	}
	else
		runStatements("DELETE FROM "+DS+" WHERE rowNumber > " + std::to_string(rowCount) + ";");
	
	transactionWriteEnd();
}

void DatabaseInterface::filterClear(int id)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterClear);
	int dataSet = filterGetDataSetId(id);

	runStatements("UPDATE " + dataSetName(dataSet) + " SET " + filterTableName(id) + " = 1;");
}

void DatabaseInterface::filterDelete(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterDelete);
	transactionWriteBegin();

	int dataSetId = filterGetDataSetId(filterIndex);

	if(dataSetId != -1)
		runStatements("ALTER TABLE " + dataSetName(dataSetId) + " DROP COLUMN " + filterTableName(filterIndex) + ";");
	runStatements("DELETE FROM Filters WHERE id = " + std::to_string(filterIndex) + ";");

	transactionWriteEnd();
}


int DatabaseInterface::filterInsert(int dataSetId, const std::string & rFilter, const std::string & generatedFilter, const std::string & constructorJson, const std::string & constructorR, const std::string & name)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterInsertDataSet);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int( stmt, 1, dataSetId);
		sqlite3_bind_text(stmt, 2, rFilter.c_str(),				rFilter.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, generatedFilter.c_str(),		generatedFilter.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, constructorJson.c_str(),		constructorJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, constructorR.c_str(),		constructorR.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 6, name.c_str(),				name.length(),				SQLITE_TRANSIENT);
	};

	transactionWriteBegin();
	
	int id = runStatementsId("INSERT INTO Filters (dataSet, rFilter, generatedFilter, constructorJson, constructorR, name) VALUES (?, ?, ?, ?, ?, ?) RETURNING rowid;", prepare);
	runStatements("ALTER TABLE " + dataSetName(dataSetId) + " ADD " + filterTableName(id) +" INT NOT NULL DEFAULT 1;");
	
	transactionWriteEnd();

	return id;
}


//This one only works when there is but 1 filter per dataset, this might change later
int DatabaseInterface::filterGetId(	int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetId);
	int filterId = -1;

	runStatements("SELECT id FROM Filters WHERE dataSet = ? AND name = ''",
		[&](sqlite3_stmt *stmt)				{ sqlite3_bind_int(stmt,	1, dataSetId); },
		[&](size_t row, sqlite3_stmt *stmt)	{ filterId = sqlite3_column_int(stmt, 0); }
	);

	return filterId;
}

int DatabaseInterface::filterGetId(const std::string &name)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetId);
	int filterId = -1;

	runStatements("SELECT id FROM Filters WHERE name = ?",
		[&](sqlite3_stmt *stmt)				{ sqlite3_bind_text(stmt, 1, name.c_str(), name.length(), SQLITE_TRANSIENT);	},
		[&](size_t row, sqlite3_stmt *stmt)	{ filterId = sqlite3_column_int(stmt, 0);										}
		);

	return filterId;
}

int DatabaseInterface::filterGetDataSetId(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetDataSetId);
	return runStatementsId("SELECT dataSet from Filters WHERE id=" + std::to_string(filterIndex));
}

std::string DatabaseInterface::filterGetName(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetName);
	std::string errorMsg;

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, filterIndex);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);
		errorMsg		= _wrap_sqlite3_column_text(stmt, 0);
	};

	runStatements("SELECT name FROM Filters WHERE id = ?;", prepare, processRow);

	return errorMsg;
}

bool DatabaseInterface::filterSelect(int filterIndex, boolvec & bools)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterSelect);
	bool changed = false;

	transactionReadBegin();

	int dataSet = filterGetDataSetId(filterIndex);

	if(dataSet != -1)
	{

		const size_t rows = dataSetRowCount(dataSet);

		changed = changed || rows != bools.size();

		bools.resize(rows);

		runStatements("SELECT " + filterTableName(filterIndex) + " FROM " + dataSetName(dataSet) + " ORDER BY rowNumber;",
		[&](sqlite3_stmt *){ }, [&](size_t row, sqlite3_stmt * stmt)
		{
			int val			= sqlite3_column_int(stmt, 0);
				changed		= changed || bools[row] != bool(val);
				bools[row]	= val;
		});
	}

	transactionReadEnd();

	return changed;
}

void DatabaseInterface::filterUpdate(int filterIndex, const std::string & rFilter, const std::string & generatedFilter, const std::string & constructorJson, const std::string & constructorR, const std::string & name)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterUpdate);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, rFilter.c_str(),			rFilter.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 2, generatedFilter.c_str(),	generatedFilter.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, constructorJson.c_str(),	constructorJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, constructorR.c_str(),	constructorR.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, name.c_str(),			name.length(),				SQLITE_TRANSIENT);
		sqlite3_bind_int (stmt,	6, filterIndex);
	};

	runStatements("UPDATE Filters SET rFilter=?, generatedFilter=?, constructorJson=?, constructorR=?, name=? WHERE id = ?;", prepare);
}

void DatabaseInterface::filterLoad(int filterIndex, std::string & rFilter, std::string & generatedFilter, std::string & constructorJson, std::string & constructorR, int & revision, std::string & name)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterLoad);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, filterIndex);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 6);
		rFilter			= _wrap_sqlite3_column_text(stmt, 0);
		generatedFilter	= _wrap_sqlite3_column_text(stmt, 1);
		constructorJson	= _wrap_sqlite3_column_text(stmt, 2);
		constructorR	= _wrap_sqlite3_column_text(stmt, 3);
		revision		= sqlite3_column_int(		stmt, 4);
		name			= _wrap_sqlite3_column_text(stmt, 5);
	};

	runStatements("SELECT rFilter, generatedFilter, constructorJson, constructorR, revision, name FROM Filters WHERE id = ?;", prepare, processRow);
}

std::string DatabaseInterface::filterLoadErrorMsg(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterLoadErrorMsg);
	std::string errorMsg;
	
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, filterIndex);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);
		errorMsg		= _wrap_sqlite3_column_text(stmt, 0);
	};

	runStatements("SELECT errorMsg FROM Filters WHERE id = ?;", prepare, processRow);
	
	return errorMsg;
}

void DatabaseInterface::filterUpdateErrorMsg(int filterIndex, const std::string & errorMsg)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterUpdateErrorMsg);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, errorMsg.c_str(),	errorMsg.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, filterIndex);
	};

	runStatements("UPDATE Filters SET errorMsg=? WHERE id = ?;", prepare);
}

int DatabaseInterface::filterIncRevision(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterIncRevision);
	transactionWriteBegin();

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, filterIndex);
	};

				runStatements(	"UPDATE Filters SET revision=revision+1	WHERE id=?;", prepare);
	int rev =	runStatementsId("SELECT revision FROM Filters			WHERE id=?;", prepare);

	transactionWriteEnd();

	return rev;
}

int DatabaseInterface::filterGetRevision(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetRevision);
	return runStatementsId("SELECT revision FROM Filters	WHERE id=?;", [&](sqlite3_stmt *stmt) { sqlite3_bind_int(stmt, 1, filterIndex); });
}

void DatabaseInterface::filterWrite(int filterIndex, const std::vector<bool> & values)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterWrite);

	transactionWriteBegin();
	
	int dataSet = filterGetDataSetId(filterIndex);

	const std::string updateFilterPrefix = "UPDATE " + dataSetName(dataSet) + " SET " + filterTableName(filterIndex) + "= ?  WHERE rowNumber = ?;" ;

	size_t rowOutside;
	
	bindParametersType _bindParams = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, values[rowOutside]);
		sqlite3_bind_int(stmt, 2, rowOutside+1);
	};
	
	_runStatementsRepeatedly(updateFilterPrefix, [&](bindParametersType ** bindParams, size_t row)
	{
		rowOutside = row;
		(*bindParams) = &_bindParams;
		
		return row < values.size();
	});

	filterIncRevision(filterIndex);

	transactionWriteEnd();
}

int DatabaseInterface::columnInsert(int dataSetId, int index, const std::string & name, columnType colType, bool alterTable)
{
	intvec ids = columnsInsert(dataSetId, 1, index, name, colType, alterTable);
	return ids.front();
}

intvec DatabaseInterface::columnsInsert(int dataSetId, int count, int index, const std::string & name, columnType colType, bool alterTable)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnInsert);
	transactionWriteBegin();
	
	if(index == -1)	index = columnLastFreeIndex(dataSetId);
	else			columnIndexIncrements(dataSetId, index, count);

#ifdef SIR_LOG_A_LOT
	Log::log() << "columnIndex for insert: " << index << " and dataSet: " << dataSetId << std::endl;
#endif
	
	intvec ids;
	int curCol = 0;
	
	bindParametersType _bindParams =  [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1, dataSetId);
		sqlite3_bind_text(stmt, 2, name.c_str(), name.length(), SQLITE_TRANSIENT);

		std::string colT = columnTypeToString(colType);
		sqlite3_bind_text(stmt, 3, colT.c_str(), colT.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	4, index+curCol++);
	};

	std::function<void(size_t,size_t, sqlite3_stmt*)> processRow = [&](size_t row, size_t rep, sqlite3_stmt * stmt)
	{
		assert(sqlite3_column_count(stmt) == 1);

		ids.push_back(sqlite3_column_int(stmt, 0));
	};
	
	_runStatementsRepeatedly("INSERT INTO Columns (dataSet, name, columnType, colIdx, analysisId) VALUES (?, ?, ?, ?, -1) RETURNING id;", [&](bindParametersType ** bindParams, size_t)
		{
			(*bindParams) = &_bindParams;
			
			return curCol < count;
		}, & processRow);
	

	if(alterTable) //If not then via dataSetCreateTable
	{
		std::stringstream statement;
		
		for(int id : ids)
			statement	<< "ALTER TABLE " << dataSetName(dataSetId)
						<< " ADD  "		<< columnBaseName(id)
						<< " NUM  NULL;\n";
		
		runStatements(statement.str());
	}
	
	//The labels will be added separately later

	transactionWriteEnd();
	return ids;
}


void DatabaseInterface::dataSetCreateTable(DataSet * dataSet)
{
	intset filters = dataSetGetFilters(dataSet->id());
	assert(filters.size() > 0);
	
	runStatements("DROP TABLE " + dataSetName(dataSet->id()) + ";");
		
	std::stringstream statements;
	statements		<<  "CREATE TABLE " + dataSetName(dataSet->id())
					<<	" (rowNumber INTEGER PRIMARY KEY";
	
	for(int id : filters)
		statements	<<	", "+ filterTableName(id) + " INT NOT NULL DEFAULT 1";
	
	for(Column * column : dataSet->columns())
		statements << ", " << columnBaseName(column->id()) << " NUM NULL";

	statements << ");";
	
	runStatements(statements.str());
}

int DatabaseInterface::columnGetDataSetId(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetDataSetId);
	return runStatementsId("SELECT dataSet from Columns WHERE id=" + std::to_string(columnId));
}

int	DatabaseInterface::columnLastFreeIndex(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnLastFreeIndex);
	return 1 + runStatementsId("SELECT MAX(colIdx) from Columns WHERE dataSet=" + std::to_string(dataSetId) + ";");
}

void DatabaseInterface::columnIndexIncrements(int dataSetId, int index, int count)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIndexIncrements);
	if(columnIdForIndex(dataSetId, index) != -1)
		runStatements("UPDATE Columns SET colIdx=colIdx+"+std::to_string(count)+ " WHERE dataSet=" + std::to_string(dataSetId) + " AND colIdx >= " + std::to_string(index) +";");
//Actually the following else is not necessary
//	else
//		throw std::runtime_error("columnIndexIncrements has a problem: index " + std::to_string(index) + " in dataSet " + std::to_string(dataSetId) + " already exists!");
}

void DatabaseInterface::columnIndexDecrements(int dataSetId, int index)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIndexDecrements);
	if(columnIdForIndex(dataSetId, index) == -1)
		runStatements("UPDATE Columns SET colIdx=colIdx-1 WHERE dataSet=" + std::to_string(dataSetId) + " AND colIdx > " + std::to_string(index) +";");
}

int DatabaseInterface::columnIdForIndex(int dataSetId, int index)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIdForIndex);
	return runStatementsId("SELECT id FROM Columns WHERE dataSet=? AND colIdx=?", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1, dataSetId);
		sqlite3_bind_int(stmt,	2, index);
	});
}

int DatabaseInterface::columnIndexForId(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIndexForId);
	return runStatementsId("SELECT colIdx FROM Columns WHERE id=?", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1, columnId);
	});
}

void DatabaseInterface::dataSetBatchedValuesUpdate(DataSet * data, Columns columns, std::function<void(float)> progressCallback)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetBatchedValuesUpdate);

	transactionWriteBegin();

	// But maybe we should update instead, maybe it speeds up the application?
	//As this data isnt synced anyway this shouldnt be a problem because it'd be invalidated after a single edit anyway
	runStatements("DELETE FROM " + dataSetName(data->id()) + " WHERE rowNumber > " + std::to_string(columns.size() == 0 ? 0 : columns.front()->rowCount()));

	std::stringstream statement;
	
	statement << "INSERT OR REPLACE INTO " << dataSetName(data->id()) << " (";

	//Add columnnames for data we want to insert
	for(Column * col : columns)
	{
		assert(col->data() == data); //Little sanity check
		statement << "Column_" << col->id() << ", ";
	}

	//And the filtername and rowNumber
        statement << filterTableName(data->filter()->id()) << ", " << "rowNumber) VALUES (";

	for(size_t i=0; i<columns.size(); i++)
		statement << "?, ";
	statement << "?, ?);"; //filter and rowNumber

	//We put a size_t outside the bindParamStore lambda to set it without having to change the signature
	size_t rowOutside=0;
	bindParametersType bindParamStore = [&](sqlite3_stmt * stmt)
	{
		size_t i=1;
		for(Column * col : columns)
			if(col->hasLabels())
			{
				sqlite3_bind_int(		stmt,	i++, col->ints()[rowOutside]);
			}
			else
			{
				const std::string & str = col->strs()[rowOutside];
				if(str.empty())
					_doubleTroubleBinder(stmt, i++, col->dbls()[rowOutside]);
				else
					sqlite3_bind_text(stmt, i++, str.c_str(), str.length(), SQLITE_TRANSIENT);
			}

		sqlite3_bind_int(stmt,	i++, data->filter()->filtered()[rowOutside]);
		sqlite3_bind_int(stmt,	i++, rowOutside+1);
	};

	const float rowsInverse		= 1.0 / float(data->rowCount());
	const int	updateInterval	= std::max(1, data->rowCount() / 100);
	int			prevUpdate		= 0;

	_runStatementsRepeatedly(
		statement.str(),
		[&](bindParametersType ** bindParameters, size_t row)
		{
			if(row >= data->rowCount())
				return false;

			rowOutside = row;

			if(prevUpdate + updateInterval <= rowOutside)
			{
				progressCallback(float(rowOutside) * rowsInverse);
				prevUpdate = rowOutside;
			}

			(*bindParameters) = &bindParamStore;

			return true;
		});
		
	transactionWriteEnd();
}

void DatabaseInterface::dataSetBatchedValuesLoad(DataSet *data, std::function<void(float)> progressCallback)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetBatchedValuesLoad);

	if(data->columns().size() == 0 && data->filter()->id() == -1)
		return;

	transactionReadBegin();
	
	//Set up some functions and such for concurrent loading:
	std::mutex	progressMutex;
	size_t 		totalRows	= data->rowCount(), // for progressbar
				progressRow = 0;
	
	
	std::function<void(float)> localProgressBar = [&progressMutex, &progressRow, &totalRows, &progressCallback](int rows)
	{
		progressMutex.lock();
		progressRow += rows;
		
		const size_t rowPercent = std::max(1, int(totalRows) / 100);
		static size_t lastRow = 0;
		
		if(progressRow - lastRow > rowPercent || progressRow >= totalRows - 1)
		{
			progressCallback(float(progressRow) / float(totalRows));
			lastRow = progressRow;
		}

		progressMutex.unlock();
	};
	
	auto loadBatchOfColumns = [this, data, &localProgressBar](Columns group, size_t groupNum)
	{
		std::stringstream statement;
	
		statement << "SELECT ";
	
		for(Column * col : group)
		{
			assert(col->id() > -1);
			statement << "Column_" << col->id() << ", ";
		}
	
		statement << filterTableName(data->filter()->id()) << " FROM " << dataSetName(data->id()) << " ORDER BY rowNumber";
	
		std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt) {};
	
		const int	rowCount	= std::max(0, dataSetRowCount(data->id()));
	
		for(Column * col : group)
			col->setRowCount(rowCount);
	
		if(groupNum == 0)
			data->filter()->setRowCount(rowCount);
	
		const size_t rowPercent = std::max(1, int(rowCount) / 100);
	
		size_t prevRowSent = 0;
		
		std::function<void(size_t, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
		{
			if(row % rowPercent == 0 || row == rowCount - 1)
			{
				localProgressBar(row-prevRowSent);
				prevRowSent = row;
			}
	
			int colCount = sqlite3_column_count(stmt);
	
			assert(colCount == group.size() + 1);
	
			for(size_t colI=0; colI<group.size(); colI++)
			{
				
				Column * col = group[colI];
				
				
				
				if(col->hasLabels())
				{
					int valueDb = !sqlite3_column_text(	stmt, colI) ? EmptyValues::missingValueInteger : sqlite3_column_int(stmt, colI);
					
					col->setValue(row, valueDb,		false);
				}
				else
				{
					
					
					
					std::string		dbStr;
					double			dbDbl = _doubleTroubleReader(stmt, colI, &dbStr);
		
					col->_strs[row] = dbStr;
					col->_dbls[row] = dbDbl;
				}
			}
	
			if(groupNum == 0)
				data->filter()->setFilterValueNoDB(row, sqlite3_column_int(stmt, colCount - 1));
		};
	
		runStatements(statement.str(), prepare, processRow);
	};
	
	//Ok, split up columns into some groups so we can use multiple threads
	
	size_t	groupCount	= std::max((unsigned int)1, std::thread::hardware_concurrency()),
			groupSize	= data->columns().size() / groupCount,
			groupSize0	= data->columns().size() - groupSize * (groupCount-1);
			totalRows	= data->rowCount() * groupCount; // for progressbar
			

	std::vector<std::thread>	threads;
	
	size_t	curCol	= 0,
			nextEnd = groupSize0;
	
	for(size_t group=0; group < groupCount; group++)
	{
		Columns cols;
		for(; curCol < nextEnd; curCol++)
			cols.push_back(data->column(curCol));
		
		
		threads.push_back(std::thread([cols, group, &loadBatchOfColumns,this]()
		{
			preloadInterfaceForThread();
			loadBatchOfColumns(cols, group);
		}));
		
		nextEnd += groupSize;
	}
	
	for(std::thread & t : threads)
		t.join();
		
	transactionReadEnd();
}

//Shouldve probably been more generic here
void DatabaseInterface::dataSetBatchedLabelsLoad(DataSet *data, std::function<void (float)> progressCallback)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetBatchedLabelsLoad);

	if(data->columns().size() == 0)
		return;

	transactionReadBegin();
	
	//Set up some functions and such for concurrent loading:
	std::mutex	progressMutex;
	size_t 		totalCols	= data->columnCount(), // for progressbar
				progressCol = 0;
	
	
	std::function<void(float)> localProgressBar = [&progressMutex, &progressCol, &totalCols, &progressCallback](int rows)
	{
		progressMutex.lock();
		progressCol += rows;
		
		const size_t rowPercent = std::max(1, int(totalCols) / 100);
		static size_t lastRow = 0;
		
		if(progressCol - lastRow > rowPercent || progressCol >= totalCols - 1)
		{
			progressCallback(float(progressCol) / float(totalCols));
			lastRow = progressCol;
		}
		progressMutex.unlock();
	};
	
	auto loadBatchOfColumns = [this, data, &localProgressBar](Columns group, size_t groupNum)
	{
		labelsLoad(group);
		localProgressBar(groupNum);
	};
	
	//Ok, split up columns into some groups so we can use multiple threads
	Columns columnsWithLabels;
	std::copy_if(data->columns().begin(), data->columns().end(), std::back_inserter(columnsWithLabels), 
				 [](Column * col){return col->hasLabels();});
	
	if(columnsWithLabels.size() == 0)
	{
		transactionReadEnd();
		return;
	}
		
	
	size_t	groupCount	= std::max((unsigned int)1, std::thread::hardware_concurrency()),
			groupSize	= columnsWithLabels.size() / groupCount,
			groupSize0	= columnsWithLabels.size() - groupSize * (groupCount-1);
			

	std::vector<std::thread>	threads;
	
	size_t	curCol	= 0,
			nextEnd = groupSize0;
	
	for(size_t group=0; group < groupCount; group++)
	{
		Columns cols;
		for(; curCol < nextEnd; curCol++)
			cols.push_back(columnsWithLabels[curCol]);
		
		
		threads.push_back(std::thread([cols, group, &loadBatchOfColumns]()
		{
			loadBatchOfColumns(cols, group);
		}));
		
		nextEnd += groupSize;
	}
	
	for(std::thread & t : threads)
		t.join();
		
	transactionReadEnd();
}

void DatabaseInterface::columnSetValues(int columnId, const intvec &ints)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetValues ints);
	transactionWriteBegin();
	
	const int			dataSetId = columnGetDataSetId(columnId);
	
	const std::string	updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "=? WHERE rowNumber=?";

	size_t				rowOutside;
	bindParametersType	bindParamStore;

	_runStatementsRepeatedly(
		updateStatement,
		[&](bindParametersType ** bindParameters, size_t row)
		{
			if(row == ints.size())
				return false;

			rowOutside = row;

			bindParamStore = [&](sqlite3_stmt * stmt)
			{
				sqlite3_bind_int(	stmt,	1, ints[rowOutside]);
				sqlite3_bind_int(	stmt,	2, rowOutside+1);
			};

			(*bindParameters) = &bindParamStore;

			return true;
		});

	transactionWriteEnd();
}

void DatabaseInterface::columnSetValues(int columnId, const doublevec &dbls, const stringvec &strs)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetValues ints);
	transactionWriteBegin();
	
	const int			dataSetId = columnGetDataSetId(columnId);
	
	const std::string	updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "=? WHERE rowNumber=?";

	size_t				rowOutside;
	bindParametersType	bindParamStore;

	_runStatementsRepeatedly(
		updateStatement,
		[&](bindParametersType ** bindParameters, size_t row)
		{
			if(row == dbls.size())
				return false;

			rowOutside = row;

			bindParamStore = [&](sqlite3_stmt * stmt)
			{
				if(!std::isnan(dbls[rowOutside]))
					_doubleTroubleBinder(	stmt,	1, dbls[rowOutside]);
				else
					sqlite3_bind_text(		stmt,	1, strs[rowOutside].c_str(), strs[rowOutside].size(), SQLITE_TRANSIENT);
				sqlite3_bind_int(			stmt,	2, rowOutside+1);
			};

			(*bindParameters) = &bindParamStore;

			return true;
		});

	transactionWriteEnd();
}

void DatabaseInterface::columnSetValue(int columnId, size_t row, int valueInt)
{
	const int dataSetId = columnGetDataSetId(columnId);
	
	const std::string updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "=? WHERE rowNumber=?";

	runStatements(updateStatement, [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(		stmt,	1, valueInt);
		sqlite3_bind_int(		stmt,	2, row+1);
	});
}

void DatabaseInterface::columnSetValue(int columnId, size_t row, double valueDbl, const std::string & valueStr)
{
	const int dataSetId = columnGetDataSetId(columnId);
	
	const std::string updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "=? WHERE rowNumber=?";

	runStatements(updateStatement, [&](sqlite3_stmt * stmt)
	{
		if(!std::isnan(valueDbl))
			_doubleTroubleBinder(	stmt,	1, valueDbl);
		else
			sqlite3_bind_text(		stmt,	1, valueStr.c_str(), valueStr.size(), SQLITE_TRANSIENT);
		sqlite3_bind_int(			stmt,	2, row+1);
	});
}

void DatabaseInterface::_doubleTroubleBinder(sqlite3_stmt * stmt, int param, double dbl)
{
	JASPTIMER_SCOPE(DatabaseInterface::_doubleTroubleBinder);
	
	if(std::isnan(dbl)	|| std::isinf(dbl))
	{
		const std::string storeThis = dbDblToString(std::isnan(dbl) ? dbDbl::nan : dbl < 0 ? dbDbl::neg_inf : dbDbl::inf);
		sqlite3_bind_text(stmt, param, storeThis.c_str(), storeThis.size(), SQLITE_TRANSIENT);
	}
	else
		sqlite3_bind_double(stmt, param, dbl);
}

double DatabaseInterface::_doubleTroubleReader(sqlite3_stmt * stmt, int colI, std::string * textReturn)
{	
	JASPTIMER_SCOPE(DatabaseInterface::_doubleTroubleReader);

	const std::string strVal = _wrap_sqlite3_column_text(stmt, colI);
	
	if(textReturn)
		*textReturn = "";
	
	if(!strVal.empty())
	{
		JASPTIMER_SCOPE(DatabaseInterface::_doubleTroubleReader-TRY);
		
		//Optimization:
		static const std::string _inf		= dbDblToString(dbDbl::inf);
		static const std::string _neg_inf	= dbDblToString(dbDbl::neg_inf);
		static const std::string _nan		= dbDblToString(dbDbl::nan);
		
		if(strVal == _inf)				return std::numeric_limits<double>::infinity();
		else if(strVal == _neg_inf)		return -1 * std::numeric_limits<double>::infinity();
		else if(strVal == _nan)			return EmptyValues::missingValueDouble;
		
	}
	
	double dbl = EmptyValues::missingValueDouble;
	
	if(ColumnUtils::getDoubleValue(strVal, dbl))
		return dbl;		// we can return the double because the textReturn is already set to empty
	
	if(textReturn)
		*textReturn = strVal;
	
	return EmptyValues::missingValueDouble;
}

size_t DatabaseInterface::columnGetLabelCount(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetLabelCount);
	
	size_t out;

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);
		out = sqlite3_column_int(stmt, 0);

	};

	runStatements("SELECT COUNT(id) FROM Labels WHERE columnId = ?;", prepare, processRow);

	return out;
}


void DatabaseInterface::columnGetValues(int columnId, intvec & ints,	const std::string & postFix)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetValues);
	transactionReadBegin();

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	int				dataSet		= columnGetDataSetId(columnId);
	const size_t	rowCount	= dataSetRowCount(dataSet);

	ints.resize(rowCount);

	std::function<void(size_t, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);

		ints[row] = sqlite3_column_int(		stmt, 0);
	};

	runStatements("SELECT " + columnBaseName(columnId, postFix) + " FROM " + dataSetName(dataSet) + " ORDER BY rowNumber;", prepare, processRow);

	transactionReadEnd();
}

void DatabaseInterface::columnGetValues(int columnId, doublevec &dbls, stringvec &strs,	const std::string & postFix)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetValues);
	transactionReadBegin();

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	int				dataSet		= columnGetDataSetId(columnId);
	const size_t	rowCount	= dataSetRowCount(dataSet);

	dbls.resize(rowCount);
	strs.resize(rowCount);

	std::function<void(size_t, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);
		
		double				dbl = _doubleTroubleReader(		stmt, 0);
		const std::string & str = _wrap_sqlite3_column_text(stmt, 0);

		strs[row] = !std::isnan(dbl) ? "" : str;
		dbls[row] = dbl;
	};

	runStatements("SELECT " + columnBaseName(columnId, postFix) + " FROM " + dataSetName(dataSet) + " ORDER BY rowNumber;", prepare, processRow);

	transactionReadEnd();
}


std::string DatabaseInterface::columnBaseName(int columnId,	const std::string & postFix) const
{
	return "Column_"  + std::to_string(columnId) + (postFix.empty() ? "" : "_" + postFix);
}

std::string DatabaseInterface::dataSetName(int dataSetId) const
{
	assert(dataSetId == 1); //yeah I know...
	return "DataSet_"  + std::to_string(dataSetId);
}

int DatabaseInterface::dataSetIncRevision(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetIncRevision);
	transactionWriteBegin();

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, dataSetId);
	};

				runStatements(	"UPDATE DataSets SET revision=revision+1	WHERE id=?;", prepare);
	int rev =	runStatementsId("SELECT revision FROM DataSets				WHERE id=?;", prepare);

	transactionWriteEnd();

	return rev;
}

int DatabaseInterface::dataSetGetRevision(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetGetRevision);
	return runStatementsId("SELECT revision FROM DataSets WHERE id=?;", [&](sqlite3_stmt *stmt) { sqlite3_bind_int(stmt, 1, dataSetId); });
}

intset DatabaseInterface::dataSetGetFilters(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetGetFilter);
	intset ints;
			
	runStatements(
				"SELECT id FROM Filters WHERE dataSet=?;", 
				[&](sqlite3_stmt *stmt) { sqlite3_bind_int(stmt, 1, dataSetId); },
				[&](size_t row, sqlite3_stmt * stmt){ ints.insert( sqlite3_column_int(stmt, 0)); });
	
	return ints;
}

std::string DatabaseInterface::filterTableName(int filterIndex) const
{
	JASPTIMER_SCOPE(DatabaseInterface::filterName);
	return "Filter_"  + std::to_string(filterIndex);
}


void DatabaseInterface::columnDelete(int columnId, bool cleanUpRest)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnDelete);
	transactionWriteBegin();

	//First lets drop the columns in the dataSet
	int dataSetId	= columnGetDataSetId(columnId),
		columnIndex	= columnIndexForId(columnId);

	if(cleanUpRest)
	{

		const std::string & alterDatasetPrefix = "ALTER TABLE Dataset_"  + std::to_string(dataSetId)	+ " ";
		const std::string & addColumnFragment  = "DROP COLUMN  " + columnBaseName(columnId);

		runStatements(alterDatasetPrefix + addColumnFragment + ";");
	}

	//Delete column entry
	runStatements("DELETE FROM Columns WHERE dataSet=? AND id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1, dataSetId);
		sqlite3_bind_int(stmt,	2, columnId);
	});
	
	if(cleanUpRest)
		columnIndexDecrements(dataSetId, columnIndex);

	transactionWriteEnd();
}

void DatabaseInterface::columnSetType(int columnId, columnType colType)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetType);
	std::string colT = columnTypeToString(colType);
	runStatements("UPDATE Columns SET columnType=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, colT.c_str(), colT.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetAutoSort(int columnId, bool sort)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetAutoSort);
	runStatements("UPDATE Columns SET autoSortByValue=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1,	sort);
		sqlite3_bind_int(stmt,	2,	columnId);
	});
}

void DatabaseInterface::columnSetHasLabels(int columnId, bool hasLabels)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetHasLabels);
	runStatements("UPDATE Columns SET hasLabels=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1,	hasLabels);
		sqlite3_bind_int(stmt,	2,	columnId);
	});
}

void DatabaseInterface::columnSetInvalidated(int columnId, bool invalidated)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetInvalidated);
	runStatements("UPDATE Columns SET invalidated=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1,	invalidated);
		sqlite3_bind_int(stmt,	2,	columnId);
	});
}

void DatabaseInterface::columnSetDropLevels(int columnId, int dropLevels)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetDropLevels);
	runStatements("UPDATE Columns SET dropLevels=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1,	dropLevels);
		sqlite3_bind_int(stmt,	2,	columnId);
	});
}

void DatabaseInterface::columnSetIndex(int columnId, int index)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetIndex);

	runStatements("UPDATE Columns SET colIdx=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1, index);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

int DatabaseInterface::columnIncRevision(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIncRevision);
	transactionWriteBegin();

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

				runStatements(	"UPDATE Columns SET revision=revision+1	WHERE id=?;", prepare);
	int rev =	runStatementsId("SELECT revision FROM Columns			WHERE id=?;", prepare);

	transactionWriteEnd();

	return rev;
}

int DatabaseInterface::columnGetRevision(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetRevision);
	return runStatementsId("SELECT revision FROM Columns WHERE id=?;", [&](sqlite3_stmt *stmt) { sqlite3_bind_int(stmt, 1, columnId); });
}


void DatabaseInterface::columnSetName(int columnId, const std::string &name)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetName);
	runStatements("UPDATE Columns SET name=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, name.c_str(), name.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetTitle(int columnId, const std::string & title)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetTitle);
	runStatements("UPDATE Columns SET title=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, title.c_str(), title.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetEmptyVals(int columnId, const std::string & emptyValsJson)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetTitle);
	runStatements("UPDATE Columns SET emptyValuesJson=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, emptyValsJson.c_str(), emptyValsJson.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetDescription(int columnId, const std::string & description)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetDescription);
	runStatements("UPDATE Columns SET description=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, description.c_str(), description.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetComputeFilter(int columnId, const std::string &filter)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetComputeFilter);
	runStatements("UPDATE Columns SET computeFilter=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, filter.c_str(), filter.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetComputedInfo(int columnId, int analysisId, bool invalidated, computedColumnType codeType, const std::string & rCode, const std::string & error, const std::string & constructorJsonStr, const std::string & computeFilter)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetComputedInfo);

	runStatements("UPDATE Columns SET invalidated=?, codeType=?, rCode=?, error=?, constructorJson=?, analysisId=?, computeFilter=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		std::string codeT = computedColumnTypeToString(codeType);

		sqlite3_bind_int(stmt,  1, int(invalidated));
		sqlite3_bind_text(stmt, 2, codeT.c_str(),				codeT.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, rCode.c_str(),				rCode.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, error.c_str(),				error.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, constructorJsonStr.c_str(),	constructorJsonStr.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,  6, analysisId);
		sqlite3_bind_text(stmt, 7, computeFilter.c_str(),		computeFilter.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,  8, columnId);
	});
}

void DatabaseInterface::columnGetBasicInfo(int columnId, std::string &name, std::string &title, std::string &description, columnType &colType, int & revision, Json::Value & emptyValuesJson, bool & autoSort, int & dropLevels, bool & hasLabels)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetBasicInfo);
	
	emptyValuesJson = Json::nullValue;
	
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};
	
	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 9);
					name			= _wrap_sqlite3_column_text(stmt, 0);
					title			= _wrap_sqlite3_column_text(stmt, 1);
					description		= _wrap_sqlite3_column_text(stmt, 2);
		std::string colTypeStr		= _wrap_sqlite3_column_text(stmt, 3);
					revision		= sqlite3_column_int(		stmt, 4);
		std::string	emptyValuesStr	= _wrap_sqlite3_column_text(stmt, 5);
					autoSort		= sqlite3_column_int(		stmt, 6);
					dropLevels		= sqlite3_column_int(		stmt, 7);
					hasLabels		= sqlite3_column_int(		stmt, 8);
					

		colType = colTypeStr.empty() ? columnType::unknown : columnTypeFromString(colTypeStr);
		
		if(colType == columnType::nominalText)
			colType = columnType::nominal;
		
		Json::Reader().parse(emptyValuesStr, emptyValuesJson);
	};

	runStatements("SELECT name, title, description, columnType, revision, emptyValuesJson, autoSortByValue, dropLevels, hasLabels FROM Columns WHERE id = ?;", prepare, processRow);
}


std::string DatabaseInterface::_wrap_sqlite3_column_text(sqlite3_stmt * stmt, int iCol)
{
	JASPTIMER_SCOPE(DatabaseInterface::_wrap_sqlite3_column_text);
	const unsigned char * col = sqlite3_column_text(stmt,	iCol);
	
	return !col ? "" : std::string(reinterpret_cast<const char*>(col));	
}

void DatabaseInterface::columnGetComputedInfo(int columnId, int &analysisId, bool &invalidated, computedColumnType &codeType, std::string &rCode, std::string &error, Json::Value &constructorJson, std::string & computeFilter)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetComputedInfo);

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 7);

					invalidated			= sqlite3_column_int(		stmt,	0);
		std::string codeTypeStr			= _wrap_sqlite3_column_text(stmt,	1);
					rCode				= _wrap_sqlite3_column_text(stmt,	2);
					error				= _wrap_sqlite3_column_text(stmt,	3);
		std::string constructorJsonStr	= _wrap_sqlite3_column_text(stmt,	4);
					analysisId			= sqlite3_column_int(		stmt,	5);
					computeFilter		= _wrap_sqlite3_column_text(stmt,	6);

		codeType = computedColumnType::notComputed;
		if (!codeTypeStr.empty())
		{
			try { codeType = computedColumnTypeFromString(codeTypeStr); }
			catch(...) {}
		}

		constructorJson = Json::objectValue;
		Json::Reader().parse(constructorJsonStr, constructorJson);
	};

	runStatements("SELECT invalidated, codeType, rCode, error, constructorJson, analysisId, computeFilter FROM Columns WHERE id = ?;", prepare, processRow);
}

void DatabaseInterface::labelsClear(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsClear);
	runStatements("DELETE FROM Labels WHERE columnId = ?;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt,	1, columnId);
	});
}

bool DatabaseInterface::labelExists(int	columnId, int intsId)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelExists);
	return -1 != runStatementsId("SELECT id FROM Labels WHERE columnId = ? and value = ?;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt,	1, columnId);
		sqlite3_bind_int(stmt,  2, intsId);
	});
}

intset DatabaseInterface::labelsExisting(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsExisting);
	intset ints;
			
	runStatements(
				"SELECT value FROM Labels WHERE columnId = ?;", 
				[&](sqlite3_stmt *stmt) { sqlite3_bind_int(stmt, 1, columnId); },
				[&](size_t row, sqlite3_stmt * stmt){ ints.insert( sqlite3_column_int(stmt, 0)); });
	
	return ints;
}

int DatabaseInterface::labelAdd(int columnId, int value, const std::string & label, bool filterAllows, const	std::string & description, const std::string & originalValueJson)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelAdd);
	return runStatementsId("INSERT INTO Labels (columnId, value, label, filterAllows, description, originalValueJson) "
						   "VALUES (?, ?, ?, ?, ?, ?) RETURNING rowid;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int( stmt,	1, columnId);
		sqlite3_bind_int( stmt,	2, value);
		sqlite3_bind_text(stmt, 3, label.c_str(),				label.length(),				SQLITE_TRANSIENT);
		sqlite3_bind_int( stmt,	4, filterAllows);
		sqlite3_bind_text(stmt, 5, description.c_str(),			description.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 6, originalValueJson.c_str(),	originalValueJson.length(),	SQLITE_TRANSIENT);
	});
}

void DatabaseInterface::labelSet(int id, int columnId, int value, const std::string & label, bool filterAllows, const	std::string & description, const std::string & originalValueJson, bool userAdded)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelSet);
	runStatements("UPDATE Labels SET columnId=?, value=?, label=?, filterAllows=?, description=?, originalValueJson=?, userAdded=? "
						   "WHERE id = ?;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int( stmt,	1, columnId);
		sqlite3_bind_int( stmt,	2, value);
		sqlite3_bind_text(stmt, 3, label.c_str(),				label.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_int( stmt,	4, filterAllows);
		sqlite3_bind_text(stmt, 5, description.c_str(),			description.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 6, originalValueJson.c_str(),	originalValueJson.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_int( stmt,	7, userAdded);
		sqlite3_bind_int( stmt,	8, id);
	});
}

void DatabaseInterface::labelDelete(int id)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelDelete);
	runStatements("DELETE FROM Labels WHERE id = ?;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int( stmt,	1, id);
	});
}

void DatabaseInterface::labelLoad(int id, int & columnId, int & value, std::string & label, bool & filterAllows, std::string & description, std::string & originalValueJson, int & order, bool & userAdded)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelLoad);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, id);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 8);

					columnId			= sqlite3_column_int(stmt,			0);
					value				= sqlite3_column_int(stmt,			1);
					label				= _wrap_sqlite3_column_text(stmt,	2);
					order				= sqlite3_column_int(stmt,			3);
					filterAllows		= sqlite3_column_int(stmt,			4);
					description			= _wrap_sqlite3_column_text(stmt,	5);
					originalValueJson	= _wrap_sqlite3_column_text(stmt,	6);
					userAdded			= sqlite3_column_int(stmt,			7);

	};

	runStatements("SELECT columnId, value, label, ordering, filterAllows, description, originalValueJson, userAdded FROM Labels WHERE id = ?;", prepare, processRow);
}

void DatabaseInterface::labelsSetOrder(const intintmap & orderPerDbId)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsSetOrder);
	
	auto iter = orderPerDbId.begin();
	if(iter == orderPerDbId.end())
		return;
	
	transactionWriteBegin();
	
	bindParametersType _bindParams = [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt, 1, iter->second);
		sqlite3_bind_int(stmt, 2, iter->first);
		iter++;
	};

	_runStatementsRepeatedly("UPDATE Labels SET ordering=? WHERE id=?", [&](bindParametersType ** bindParams, size_t)
	{
		(*bindParams) = &_bindParams;
				
		return iter != orderPerDbId.end();
	});


	transactionWriteEnd();
}

void DatabaseInterface::labelSetOrder(int id, int order)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelSetOrder);
	transactionWriteBegin();

	runStatements("UPDATE Labels SET ordering=? WHERE id=?", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt, 1, order);
		sqlite3_bind_int(stmt, 2, id);
	});

	transactionWriteEnd();
}


void DatabaseInterface::labelsLoad(Column * column)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsLoad);
	
	transactionReadBegin();
	
	column->beginBatchedLabelsDB();
	
	int labelsSize = 0;

	column->_resetLabelValueMap();
	
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, column->id());
	};
	
	Json::Reader reader;

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 7);

		int			id						= sqlite3_column_int(stmt,			0);
		int			value					= sqlite3_column_int(stmt,			1);
		std::string	label					= _wrap_sqlite3_column_text(stmt,	2);
		int			order					= sqlite3_column_int(stmt,			3);
		bool		filterAllows			= sqlite3_column_int(stmt,			4);
		std::string	description				= _wrap_sqlite3_column_text(stmt,	5);
		std::string	originalValueJsonStr	= _wrap_sqlite3_column_text(stmt,	6);
		
		Json::Value originalValueJson;
		
		reader.parse(originalValueJsonStr, originalValueJson);

		if (originalValueJson.isNull() && !originalValueJsonStr.empty())
			originalValueJson = originalValueJsonStr; // For backward compatibility: in some JASP files the originalValueJson is not a json string but just the original string.

		column->labelsSet(labelsSize++,	value, label, filterAllows, description, originalValueJson, order, id);
	};

	runStatements("SELECT id, value, label, ordering, filterAllows, description, originalValueJson FROM Labels WHERE columnId = ? ORDER BY ordering;", prepare, processRow);

	column->labelsShrinkOnlyToSize(labelsSize);
	 
	column->endBatchedLabelsDB(false);
	
	transactionReadEnd();
}

void DatabaseInterface::labelsLoad(const Columns &columns)//, std::function<void (float)> progressCallback)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsLoad);
	
	transactionReadBegin();
	
	std::map<int,Column*>	localColMap;
	intintmap				labelsPerCol;
	std::stringstream		statement;
	
	statement << "SELECT id, value, label, ordering, filterAllows, description, originalValueJson, columnId FROM Labels WHERE columnId IN (";
	
	for(Column * column : columns)
	{
		column->beginBatchedLabelsDB();
		
		if(localColMap.size() > 0)
			statement << ", ";
		statement << column->id();
		
		localColMap [column->id()] = column;
		labelsPerCol[column->id()] = 0;

		column->_resetLabelValueMap();
	}
	
	statement << ")  ORDER BY columnId, ordering;";

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
	};
	
	Json::Reader reader;

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 8);

		int			id						= sqlite3_column_int(stmt,			0);
		int			value					= sqlite3_column_int(stmt,			1);
		std::string	label					= _wrap_sqlite3_column_text(stmt,	2);
		int			order					= sqlite3_column_int(stmt,			3);
		bool		filterAllows			= sqlite3_column_int(stmt,			4);
		std::string	description				= _wrap_sqlite3_column_text(stmt,	5);
		std::string	originalValueJsonStr	= _wrap_sqlite3_column_text(stmt,	6);
		int			columnId				= sqlite3_column_int(stmt,			7);
		
		Json::Value originalValueJson;
		
		assert(id != -1);
		
		reader.parse(originalValueJsonStr, originalValueJson);

		if (originalValueJson.isNull() && !originalValueJsonStr.empty())
			originalValueJson = originalValueJsonStr; // For backward compatibility: in some JASP files the originalValueJson is not a json string but just the original string.

		if(localColMap.count(columnId))
		{
			//std::stringstream str;
			//str << "labelsLoad for column " << localColMap.at(columnId)->name() << " in thread " << std::this_thread::get_id() << " got "
			//	<< "labelsSet(row='"		<< order
			//	<< "', value='"				<< value
			//	<< "', label='"				<< label
			//	<< "', filterAllows='"		<< filterAllows
			//	<< "', description='"		<< description
			//	<< "', originalValueJson='"	<< originalValueJson
			//	<< "', order='"				<< order
			//	<< "', id='"				<< id
			//	<< "')\n";
			//Log::log() << str.str() << std::flush;

			localColMap.at(columnId)->labelsSet(labelsPerCol[columnId]++,	value, label, filterAllows, description, originalValueJson, order, id);
		}
	};

	runStatements(statement.str(), prepare, processRow);

	for(Column * column : columns)
	{
		column->labelsShrinkOnlyToSize(labelsPerCol[column->id()]);
		column->endBatchedLabelsDB(false);
	}
	
	transactionReadEnd();
}

void DatabaseInterface::labelsWrite(Column *column)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsWrite Column);
	transactionWriteBegin();

	runStatements("DELETE From Labels WHERE columnId=?", [&](sqlite3_stmt *stmt) { sqlite3_bind_int( stmt,	1, column->id()); });
	
	auto labelIter = column->labels().begin();
	
	if(labelIter != column->labels().end())
	{

		bindParametersType _bindParams =  [&](sqlite3_stmt *stmt)
		{
			const Label			*	label			= *labelIter;
			const std::string		labelDisplay	= label->label(),
									origValJson		= label->originalValue().toStyledString();
					
			sqlite3_bind_int( stmt,	1, column->id());
			sqlite3_bind_int( stmt,	2, label->intsId());
			sqlite3_bind_text(stmt, 3, labelDisplay.c_str(),			labelDisplay.length(),				SQLITE_TRANSIENT);
			sqlite3_bind_int( stmt,	4, label->filterAllows());
			sqlite3_bind_text(stmt, 5, label->description().c_str(),	label->description().length(),		SQLITE_TRANSIENT);
			sqlite3_bind_text(stmt, 6, origValJson.c_str(),				origValJson.length(),				SQLITE_TRANSIENT);
			sqlite3_bind_int( stmt,	7, label->order());
		};
	
		std::function<void(size_t,size_t, sqlite3_stmt*)> processRow = [&](size_t row, size_t rep, sqlite3_stmt * stmt)
		{
			assert(sqlite3_column_count(stmt) == 1);
	
			Label * label = *labelIter;

			label->setDbId(sqlite3_column_int(stmt, 0));
	
			labelIter++;
		};
		
		_runStatementsRepeatedly("INSERT INTO Labels (columnId, value, label, filterAllows, description, originalValueJson, ordering) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id;", [&](bindParametersType ** bindParams, size_t)
			{
				(*bindParams) = &_bindParams;
				
				return labelIter != column->labels().end();
			}, & processRow);
	}
	
	transactionWriteEnd();
}

void DatabaseInterface::labelsWrite(const Columns & columns, std::function<void(float)> progressCallback)
{
	 JASPTIMER_SCOPE(DatabaseInterface::labelsWrite Columns);
	 transactionWriteBegin();
	 
	 Labels allLabels;
	 
	 if(columns.size() > 0)
	 {
		 std::stringstream statement;
		 int count=0;
		 
		 statement << "(";
		 for(Column * column : columns)
		 {
			 if(count > 0)
				 statement << ", ";
			 statement << column->id();
			 
			 for(Label * label : column->labels())
				 allLabels.push_back(label);
			 
			 count++;
		 }
		 statement << ")";
		 
		 runStatements("DELETE From Labels WHERE columnId in " + statement.str());
	}
	 
	auto labelIter = allLabels.begin();
	
	if(labelIter != allLabels.end())
	{
		const size_t	totalRows	= allLabels.size();
		size_t			curRow		= 0,
						lastRow		= 0;
 
		 bindParametersType _bindParams =  [&](sqlite3_stmt *stmt)
		 {
			 const Label			*	label			= *labelIter;
			 const std::string			labelDisplay	= label->label(false),
										origValJson		= label->originalValue().toStyledString();
			 
			 Column					*	column			= static_cast<Column*>(label->parent());

			 sqlite3_bind_int( stmt,	1, column->id());
			 sqlite3_bind_int( stmt,	2, label->intsId());
			 sqlite3_bind_text(stmt,	3, labelDisplay.c_str(),			labelDisplay.length(),				SQLITE_TRANSIENT);
			 sqlite3_bind_int( stmt,	4, label->filterAllows());
			 sqlite3_bind_text(stmt,	5, label->description().c_str(),	label->description().length(),		SQLITE_TRANSIENT);
			 sqlite3_bind_text(stmt,	6, origValJson.c_str(),				origValJson.length(),				SQLITE_TRANSIENT);
			 sqlite3_bind_int( stmt,	7, label->order());
		 };
	 
		 std::function<void(size_t,size_t, sqlite3_stmt*)> processRow = [&](size_t row, size_t rep, sqlite3_stmt * stmt)
		 {
			 assert(sqlite3_column_count(stmt) == 1);
	 
			 Label * label = *labelIter;

			 label->setDbId(sqlite3_column_int(stmt, 0));
	 
			 labelIter++;
			 curRow++;
			 
			 if(curRow-lastRow > 100 || curRow-lastRow > totalRows / 100 || curRow >= totalRows-1)
			 {
				 progressCallback(float(curRow) / float(totalRows));
				 lastRow = curRow;
			}
		 };
		 
		 _runStatementsRepeatedly("INSERT INTO Labels (columnId, value, label, filterAllows, description, originalValueJson, ordering) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id;", [&](bindParametersType ** bindParams, size_t)
			 {
				 (*bindParams) = &_bindParams;
				 
				 return labelIter != allLabels.end();
			 }, & processRow);
	 }
	 
	 transactionWriteEnd();
}



std::string DatabaseInterface::dbFile(bool onlyName) const
{
	JASPTIMER_SCOPE(DatabaseInterface::dbFile);

	static std::string fileName = "internal.sqlite";
	static std::string memoryName = ":memory:";

	if (_inMemory)
		return memoryName;

	return onlyName ? fileName : std::filesystem::path(TempFiles::sessionDirName() + "/" + fileName).generic_string();
}

DatabaseInterface *DatabaseInterface::singleton() 
{ 
	if(!_singleton)
	{
		Log::log() << "No DatabaseInterface::singleton available here yet, creating an interface for internal.sqlite without recreating the dbstructure" << std::endl;
		_singleton = new DatabaseInterface(false);
	}
	
	return _singleton; 
}

void DatabaseInterface::closeInterfaces()
{
	delete _singleton;
}

void DatabaseInterface::runQuery(const std::string & query, std::function<void(sqlite3_stmt *stmt)> bindParameters, std::function<void(size_t row, sqlite3_stmt *stmt)> processRow)
{
	JASPTIMER_SCOPE(DatabaseInterface::runQuery);
	runStatements(query, bindParameters, processRow);
}

void DatabaseInterface::runStatements(	const std::string & statements, bool ignoreFails)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatements);
	_runStatements(statements, nullptr, nullptr, ignoreFails);
}

void DatabaseInterface::runStatements(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters, bool ignoreFails)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatements);
	_runStatements(statements, &bindParameters, nullptr, ignoreFails);
}

int DatabaseInterface::runStatementsId(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters, bool ignoreFails)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatementsId);
	int id = -1;
	std::function<void(size_t row, sqlite3_stmt *stmt)> processId = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		if(colCount)
			id = sqlite3_column_int(stmt, 0);

	};

	_runStatements(statements, &bindParameters, &processId, ignoreFails);

#ifdef SIR_LOG_A_LOT
	Log::log() << "Output for '" << statements << "' returns id:" << id << std::endl;
#endif
	return id;
}

int DatabaseInterface::runStatementsId(const std::string & statements, bool ignoreFails) 
{
	return runStatementsId(statements, [](sqlite3_stmt *stmt){}, ignoreFails);
}

void DatabaseInterface::runStatements(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters,	std::function<void(size_t row, sqlite3_stmt *stmt)>	processRow, bool ignoreFails)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatements);
	_runStatements(statements, &bindParameters, &processRow, ignoreFails);
}

int DatabaseInterface::dataSetGetId()
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetGetId);
	return runStatementsId("SELECT id FROM DataSets LIMIT 1");
}

bool DatabaseInterface::dataSetExists(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetExists);
	return -1 != runStatementsId("SELECT id FROM DataSets WHERE id = " + std::to_string(dataSetId) + ";");
}

void DatabaseInterface::dataSetDelete(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetDelete);
	transactionWriteBegin();
	runStatements("DELETE FROM DataSets WHERE id = " + std::to_string(dataSetId) + ";");
	runStatements("DROP TABLE " + dataSetName(dataSetId) + ";");
	transactionWriteEnd();
}

#define BUSY_MAX_SECS 90
#define BUSY_INTERVAL std::chrono::nanoseconds(1000000)

void DatabaseInterface::_runStatements(const std::string & statements, bindParametersType * bindParameters, std::function<void(size_t row, sqlite3_stmt *stmt)> * processRow, bool ignoreFails)
{
	JASPTIMER_SCOPE(DatabaseInterface::_runStatements);
#ifdef SIR_LOG_A_LOT
	Log::log() << "Running statements: '" << statements << "'" << std::endl;
#endif
	
	int64_t epochIdle = Utils::currentSeconds();

	sqlite3_stmt * dbStmt = nullptr;

	const char	*	start	= statements.c_str(),
				*	current	= start,
				*	tail;
	size_t			total	= statements.size(),
					remain,
					row;
	int				ret		= SQLITE_OK;

	do
	{
		JASPTIMER_RESUME(DatabaseInterface::_runStatements prepare);
		ret	= sqlite3_prepare_v2(_db(), current, total - (current - start), &dbStmt, &tail);
		JASPTIMER_STOP(DatabaseInterface::_runStatements prepare);
		row = 0;

		if(bindParameters)
			(*bindParameters)(dbStmt);

		JASPTIMER_RESUME(DatabaseInterface::_runStatements run statements);
		if(ret == SQLITE_OK && dbStmt)
		{
			do
			{
				ret = sqlite3_step(dbStmt);
				
				switch(ret)
				{
				case SQLITE_ERROR:
				{
					std::string errorMsg = "Running ```\n"+statements.substr(current - start)+"\n``` failed because of: `" + sqlite3_errmsg(_db());
					Log::log() << errorMsg << std::endl;
					if(!ignoreFails)
						throw std::runtime_error(errorMsg);
				}
				 
			   case SQLITE_ROW:
					epochIdle = Utils::currentSeconds();
					if(processRow)
						(*processRow)(row, dbStmt);
					row++;
					break;
					
				case SQLITE_BUSY:
					if(Utils::currentSeconds() - epochIdle > BUSY_MAX_SECS)
						throw std::runtime_error("Sqlite was busy for too long!");
					std::this_thread::sleep_for(BUSY_INTERVAL);
					break;
					
				case SQLITE_DONE:
					break;
				
				case SQLITE_NOTADB:
				case SQLITE_CORRUPT:
				{
					Log::log() << "Database is malformed..." << std::endl;
					throw dbMalformedException();
				}

					
				//case SQLITE_CORRUPT:
				//{
				//	std::string errorMsg = "Running ```\n"+statements.substr(current - start)+"\n``` with status "+std::to_string(ret)+" failed  because of: `" + sqlite3_errmsg(_db());
				//	Log::log() << errorMsg << std::endl;
				//	throw std::runtime_error(errorMsg);
				//}
				//	break;
					
				default:
				{
					std::string errorMsg = "Running ```\n"+statements.substr(current - start)+"\n``` had unchecked status "+std::to_string(ret)+" because of: `" + sqlite3_errmsg(_db());
					Log::log() << errorMsg << std::endl;
					break;
				}
			   }
				
				

			}
			while((ret == SQLITE_BUSY || ret == SQLITE_ROW) && ret != SQLITE_DONE);

			JASPTIMER_STOP(DatabaseInterface::_runStatements run statements);

			ret = sqlite3_finalize(dbStmt);
			dbStmt = nullptr;
		}

		remain	= total - (tail - start);
		//Log::log() << "Just ran `" + std::string(current, tail) + "` which returned " << ret << " and " << remain << " remaining." << std::endl;
		current	= tail;
	}
	while(remain > 1 && (ret == SQLITE_OK && ret != SQLITE_DONE));

	const int maxLenStatementError = 200;
	std::string shortStatements = statements.size() <= maxLenStatementError ? statements : statements.substr(0, maxLenStatementError);
	
	if(ret == SQLITE_ERROR)
	{
		Log::log() <<					"Running ```\n"+statements		+"\n``` failed because of: `" + sqlite3_errmsg(_db()) << std::endl;
		if(!ignoreFails)
			throw std::runtime_error(	"Running ```\n"+shortStatements	+"\n``` failed because of: `" + sqlite3_errmsg(_db()));
	}

	if(ret == SQLITE_READONLY)
	{
		Log::log() <<				"Running ```\n"+statements		+"\n``` failed because the database is readonly..." << std::endl;
		throw std::runtime_error(	"Running ```\n"+shortStatements	+"\n``` failed because the database is readonly...");
	}
}

void DatabaseInterface::_runStatementsRepeatedly(const std::string & statements, std::function<bool(bindParametersType ** bindParameters, size_t row)> bindParameterFactory, std::function<void(size_t row, size_t repetition, sqlite3_stmt *stmt)> * processRow, bool ignoreFails)
{
	JASPTIMER_SCOPE(DatabaseInterface::_runStatementsRepeatedly);
#ifdef SIR_LOG_A_LOT
	Log::log() << "Running statements repeatedly: '" << statements << "'" << std::endl;
#endif

	int64_t epochIdle = Utils::currentSeconds();
	
	sqlite3_stmt * dbStmt = nullptr;

	const char	*	start		= statements.c_str(),
				*	current		= start,
				*	tail;
	size_t			total		= statements.size(),
					remain,
					row			= 0,
					repetition	=  0;
	int				ret			= SQLITE_OK;

	std::function<void(sqlite3_stmt *stmt)> * bindParameters = nullptr;

	do
	{
		ret	= sqlite3_prepare_v2(_db(), current, total - (current - start), &dbStmt, &tail);

		row = 0;

		while((ret == SQLITE_OK || ret == SQLITE_DONE) && dbStmt && bindParameterFactory(&bindParameters, row))
		{
			if(bindParameters)
				(*bindParameters)(dbStmt);

			if((ret == SQLITE_OK || ret == SQLITE_DONE) && dbStmt)
			{
				do
				{
					ret = sqlite3_step(dbStmt);

					switch(ret)
					{
					case SQLITE_ERROR:
					{
						std::string errorMsg = "Running `\n"+statements.substr(current - start)+"\n` repeatedly failed because of: `" + sqlite3_errmsg(_db());
						Log::log() << errorMsg << std::endl;
						
						if(!ignoreFails)
							throw std::runtime_error(errorMsg);
					}

					case SQLITE_ROW:
						if(processRow)
							(*processRow)(row, repetition, dbStmt);
						row++;
						epochIdle = Utils::currentSeconds();
						break;
						
					case SQLITE_BUSY:
						if(Utils::currentSeconds() - epochIdle > BUSY_MAX_SECS)
							throw std::runtime_error("Sqlite was busy for too long!");
						std::this_thread::sleep_for(BUSY_INTERVAL);
						break;
						
					case SQLITE_DONE:
						row++;
						break;
					
					case SQLITE_NOTADB:
					case SQLITE_CORRUPT:
					{
						Log::log() << "Database is malformed..." << std::endl;
						throw dbMalformedException();
					}
						
					//case SQLITE_CORRUPT:
					//{
					//	std::string errorMsg = "Running ```\n"+statements.substr(current - start)+"\n``` with status "+std::to_string(ret)+" failed  because of: `" + sqlite3_errmsg(_db());
					//	Log::log() << errorMsg << std::endl;
					//	throw std::runtime_error(errorMsg);
					//}
						break;
						
					default:
					{
						std::string errorMsg = "Running ```\n"+statements.substr(current - start)+"\n``` had unchecked status "+std::to_string(ret)+" because of: `" + sqlite3_errmsg(_db());
						Log::log() << errorMsg << std::endl;
					}
					}
				
					
				}
				while((ret == SQLITE_BUSY || ret == SQLITE_ROW) && ret != SQLITE_DONE);
			}
			
			sqlite3_reset(dbStmt);
			repetition++;
		}
		
		if(ret == SQLITE_ERROR)
		{
			std::string errorMsg = "A problem occured trying to prepare statement `" + statements + "` and the error was: : `" + sqlite3_errmsg(_db());
			Log::log() << errorMsg << std::endl;
			throw std::runtime_error(errorMsg);
		}

		ret = sqlite3_finalize(dbStmt);
		dbStmt = nullptr;


		remain	= total - (tail - start);
		//Log::log() << "Just ran `" + std::string(current, tail) + "` which returned " << ret << " and " << remain << " remaining." << std::endl;
		current	= tail;
	}
	while(remain > 1 && (ret == SQLITE_OK && ret != SQLITE_DONE));

	if(ret == SQLITE_ERROR)
	{
		std::string errorMsg = "Running ```\n"+statements+"\n``` failed because of: `" + sqlite3_errmsg(_db());
		Log::log() << errorMsg << std::endl;

		if(!ignoreFails)
			throw std::runtime_error(errorMsg);
	}

	if(ret == SQLITE_READONLY)
	{
		std::string errorMsg = "Running ```\n"+statements+"\n``` failed because the database is readonly...";
		Log::log() << errorMsg << std::endl;
		throw std::runtime_error(errorMsg);
	}
}

sqlite3 * DatabaseInterface::_db()
{	
	const auto id = std::this_thread::get_id();

	if(_dbCreated && _dbCreator == id)
		return _dbCreated;

	_dbCheckMutex.lock();
	bool itsNotThereYet = !_dbs.count(id); //tip toe around the map
	_dbCheckMutex.unlock();

	if(itsNotThereYet)
		load();

	_dbCheckMutex.lock();
	sqlite3 * dbFound = _dbs.at(id);
	_dbCheckMutex.unlock();

	return dbFound;
}

void DatabaseInterface::create()
{
	JASPTIMER_SCOPE(DatabaseInterface::create);
	assert(!_dbCreated);

	if(!_inMemory && std::filesystem::exists(dbFile()))
	{
		Log::log() << "DatabaseInterface::create: Removing existing sqlite internal db at " << dbFile() << std::endl;
		std::filesystem::remove(dbFile());
	}
	
	int ret = sqlite3_open_v2(dbFile().c_str(), &_dbCreated, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_NOMUTEX, NULL);
	
	_transactionWriteDepth = 0;
	_transactionReadDepth = 0;

	if(ret != SQLITE_OK)
	{
		Log::log() << "Couldnt open sqlite internal db (" << dbFile() << "), because of: " << (_dbCreated ? sqlite3_errmsg(_dbCreated) : "not even a broken sqlite3 obj was returned..." ) << std::endl;
		throw std::runtime_error("JASP cannot run without an internal database and it cannot be created. Contact the JASP team for help.");
	}
	else
		Log::log() << "Opened internal sqlite database for creation at '" << dbFile() << "'.  This is for thread " << std::this_thread::get_id() << std::endl;
	
	//runStatements("pragma journal_mode=wal;");
	//runStatements("pragma synchronous=full;");
	
	sqlite3_busy_timeout(_dbCreated, 100);
	
	bool	constructionWorked	= false;
	size_t	constructionAttempt = 0; 
isItReallyALabel:
	
	try
	{
		transactionWriteBegin();
		runStatements(_dbConstructionSql);
		runStatements(_dbIndexesSql);
		transactionWriteEnd();
		
		constructionWorked = true;
	}
	catch(dbMalformedException & e)
	{
		//Unfortunate, but perhaps we were too quick?	
		constructionWorked = false;
		constructionAttempt++;
	}
	
	if(!constructionWorked)
	{
		if(constructionAttempt > 10)
			throw dbMalformedException();
		
		std::this_thread::sleep_for(std::chrono::nanoseconds(1000000));
		goto isItReallyALabel;
	}
	
	_dbCreator = std::this_thread::get_id();
}

void DatabaseInterface::doWalCheckPoint()
{
	/*int sizeWalLogInFrames, totalNumberOfFramesCheckpointed;
	
	sqlite3_wal_checkpoint_v2(_db(), NULL, SQLITE_CHECKPOINT_RESTART, &sizeWalLogInFrames, &totalNumberOfFramesCheckpointed);
	
	if(sizeWalLogInFrames || totalNumberOfFramesCheckpointed)
	{
		Log::log() << "DatabaseInterface::doWalCheckPoint: sizeWalLogInFrames=" << sizeWalLogInFrames << " and totalNumberOfFramesCheckpointed=" << totalNumberOfFramesCheckpointed << std::endl;
	}*/
}

void DatabaseInterface::preloadInterfaceForThread()
{
	//Load the interface by asking for it
	_db();
}

void DatabaseInterface::load()
{
	JASPTIMER_SCOPE(DatabaseInterface::load);
	assert(!_dbCreated || std::this_thread::get_id() != _dbCreator);

	_loadMutex.lock();
	if(_dbs.count(std::this_thread::get_id()))
	{
		sqlite3* connection = _dbs.at(std::this_thread::get_id());
		_loadMutex.unlock();
		return;
	}

	if(!std::filesystem::exists(dbFile()))
		throw std::runtime_error("Trying to load '" + dbFile() + "' but it doesn't exist!");

	bool        loadingWorked	= false;
	size_t      loadingAttempt	= 0;
	sqlite3 *   db              = nullptr;

	int ret = sqlite3_open_v2(dbFile().c_str(), &db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_NOMUTEX, NULL);

	if(ret != SQLITE_OK)
	{
		Log::log() << "Couldnt open sqlite internal db, because of: " << (db ? sqlite3_errmsg(db) : "not even a broken sqlite3 obj was returned..." ) << std::endl;
		throw std::runtime_error("JASP cannot run without an internal database and it cannot be created. Contact the JASP team for help.");
	}
	else
		Log::log() << "Opened internal sqlite database for loading at '" << dbFile() << "'. This is for thread " << std::this_thread::get_id() << std::endl;

	_dbCheckMutex.lock();
	_dbs[std::this_thread::get_id()] = db;
	_transactionWriteDepth = 0;
	_transactionReadDepth = 0;
	_dbCheckMutex.unlock();
	_loadMutex.unlock();

	for(bool loadingWorked = false; !loadingWorked; )
	{
		sqlite3_busy_timeout(db, 100);

		try
		{
			int tableCount = runStatementsId("SELECT COUNT(*) FROM sqlite_schema WHERE type ='table' AND name NOT LIKE 'sqlite_%';");
			if(tableCount < 0)
				throw dbMalformedException();

			Log::log() << "Loaded a database with #" << tableCount << " tables." << std::endl;
			loadingWorked = true;
		}
		catch(dbMalformedException & e)
		{
			//Unfortunate, but perhaps we were too quick?
			loadingWorked = false;
			loadingAttempt++;
		}

		if(!loadingWorked)
		{
			if(loadingAttempt > 10 * 60) //Timeout is 0.1 sec, so this lets the db try for 1 minute to connect...
				throw dbMalformedException();

			Log::log() << "There was a problem loading the database, retrying for the #" << loadingAttempt << " time" << std::endl;
			std::this_thread::sleep_for(std::chrono::nanoseconds(100000000));
		}
	}

	return;
}

void DatabaseInterface::close()
{
	JASPTIMER_SCOPE(DatabaseInterface::close);
	
	std::set<sqlite3*> waitingFor;
	
	_dbCheckMutex.lock();

	for(auto & idDb : _dbs)
		waitingFor.insert(idDb.second);
					
	do
	{
		for(auto & idDb : _dbs)
			if(waitingFor.count(idDb.second))
			{
				int ret = sqlite3_close(idDb.second);
				
				if(ret == SQLITE_OK)
					waitingFor.erase(idDb.second);
			}
		
		if(waitingFor.size() > 0)
			std::this_thread::sleep_for(std::chrono::nanoseconds(10000000));
	}
	while(waitingFor.size() > 0);
		
	_dbs.clear();

	_dbCheckMutex.unlock();
	
	while(_dbCreated && sqlite3_close(_dbCreated) != SQLITE_OK)
	{
		std::this_thread::sleep_for(std::chrono::nanoseconds(10000000));
	}
	_dbCreated = nullptr;
}

bool DatabaseInterface::tableHasColumn(const std::string &tableName, const std::string &columnName)
{
	return SQLITE_OK == sqlite3_table_column_metadata(
	  _db(),
	  NULL,     
	  tableName.c_str(), 
	  columnName.c_str(),
	  NULL,
	  NULL,
	  NULL,
	  NULL,
	  NULL
				);
}

bool DatabaseInterface::tableExists(const std::string &name)
{
	return runStatementsId("SELECT COUNT(name) FROM sqlite_master WHERE type='table' AND name='" + name + "';") > 0;
}

int DatabaseInterface::transactionWriteDepth()
{
	return _transactionWriteDepth;
}

int DatabaseInterface::transactionReadDepth()
{
	return _transactionReadDepth;
}

void DatabaseInterface::transactionWriteBegin()
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionWriteBegin);
	
	if(_transactionReadDepth > 0)
	{
		Log::log() << "A read transaction seems to be still going during DatabaseInterface::transactionWriteBegin, however, instead of crashing directly we will just try to unwind it." << std::endl;
		while(_transactionReadDepth > 0) transactionReadEnd();
	}
	
	if(_transactionWriteDepth++ == 0)
		runStatements("BEGIN EXCLUSIVE", true); //runStatements already has a while loop handling SQLITE_BUSY so this should work?
}

void DatabaseInterface::transactionReadBegin()
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionReadBegin);
	
	if(_transactionWriteDepth > 0)
	{
		Log::log() << "A write transaction seems to be still going during DatabaseInterface::transactionReadBegin, however, instead of crashing directly we will just try to unwind it." << std::endl;
		while(_transactionWriteDepth > 0) transactionWriteEnd();
	}
	
	if(_transactionReadDepth++ == 0)
		runStatements("BEGIN DEFERRED", true);
}

void DatabaseInterface::transactionWriteEnd(bool rollback)
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionWriteEnd);
	
	if(rollback)	
	{
		runStatements("ROLLBACK");
		_transactionWriteDepth = 0;
		throw std::runtime_error("Rollback!"); //Might be better to use a subclass of std::runtime_error but for now this isnt even used anyway.
	}	
	else if(_transactionWriteDepth == 0 || --_transactionWriteDepth == 0)
		runStatements("COMMIT", true);

}

void DatabaseInterface::transactionReadEnd()
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionReadEnd);
	
	if(_transactionReadDepth == 0 || --_transactionReadDepth == 0)
		runStatements("COMMIT", true); //ignore fails cause it fails sometimes, probably because no statements was done and so nothing to commit. this is ok
}

void DatabaseInterface::truncateAllTables()
{
	JASPTIMER_SCOPE(DatabaseInterface::truncateAllTables);
	runStatements(R"MultiPower(
	DELETE FROM Labels;
	DELETE FROM Columns;
	DELETE FROM Filters;
	DELETE FROM DataSets;
	VACUUM;
)MultiPower");
	
	
	
}


