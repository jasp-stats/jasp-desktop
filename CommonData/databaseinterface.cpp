#include "databaseinterface.h"
#include "utils.h"
#include "tempfiles.h"
#include "log.h"
#include "dataset.h"
#include "columntype.h"
#include "version"

DatabaseInterface * DatabaseInterface::_singleton = nullptr;

//#define SIR_LOG_A_LOT

const std::string DatabaseInterface::_dbConstructionSql =
//"PRAGMA foreign_keys=TRUE;\n"
"CREATE TABLE DataSets		( id INTEGER PRIMARY KEY, dataFilePath TEXT, description TEXT, databaseJson TEXT, emptyValuesJson TEXT, revision INT DEFAULT 0, dataFileSynch INT);\n"
"CREATE TABLE Filters		( id INTEGER PRIMARY KEY, dataSet INT, rFilter TEXT, generatedFilter TEXT, constructorJson TEXT, constructorR TEXT, errorMsg TEXT"
							", revision INT DEFAULT 0, FOREIGN KEY(dataSet) REFERENCES DataSets(id));\n"
"CREATE TABLE Columns		( id INTEGER PRIMARY KEY, dataSet INT, name TEXT, title TEXT, description TEXT, columnType TEXT, colIdx INT, isComputed INT, invalidated INT NULL, "
							"codeType TEXT NULL, rCode TEXT NULL, error TEXT NULL, constructorJson TEXT NULL, emptyValuesJson TEXT, "
							"analysisId INT NULL, revision INT DEFAULT 0, FOREIGN KEY(dataSet) REFERENCES DataSets(id));\n"
"CREATE TABLE Labels		( id INTEGER PRIMARY KEY, columnId INT, value INT, ordering INT, filterAllows INT, label TEXT, originalValueJson TEXT, description TEXT, FOREIGN KEY(columnId) REFERENCES Columns(id));\n";

DatabaseInterface::DatabaseInterface(bool createDb)
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


int DatabaseInterface::dataSetInsert(const std::string & dataFilePath, const std::string & description, const std::string & databaseJson, const std::string & emptyValuesJson, bool dataSynch)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetInsert);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, dataFilePath.c_str(),	dataFilePath.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 2, description.c_str(),		description.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, databaseJson.c_str(),	databaseJson.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, emptyValuesJson.c_str(), emptyValuesJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	5, dataSynch);
	};

	transactionWriteBegin();
	int id = runStatementsId("INSERT INTO DataSets (dataFilePath, description, databaseJson, emptyValuesJson, dataFileSynch) VALUES (?, ?, ?, ?, ?) RETURNING id;", prepare);
	runStatements("CREATE TABLE " + dataSetName(id) + " (rowNumber INTEGER PRIMARY KEY);");
	transactionWriteEnd();

	return id;
}

void DatabaseInterface::dataSetUpdate(int dataSetId,	const std::string & dataFilePath, const std::string & description, const std::string & databaseJson, const std::string & emptyValuesJson, bool dataSynch)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetUpdate);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, dataFilePath.c_str(),	dataFilePath.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 2, description.c_str(),		description.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, databaseJson.c_str(),	databaseJson.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, emptyValuesJson.c_str(), emptyValuesJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	5, dataSynch);
		sqlite3_bind_int(stmt,	6, dataSetId);
	};

	Log::log() << "UPDATE DataSet " << dataSetId << " with Empty Values: " << emptyValuesJson << std::endl;

	runStatements("UPDATE DataSets SET dataFilePath=?, description=?, databaseJson=?, emptyValuesJson=?, dataFileSynch=?, revision=revision+1 WHERE id = ?;", prepare);
}

void DatabaseInterface::dataSetLoad(int dataSetId, std::string & dataFilePath, std::string & description, std::string & databaseJson, std::string & emptyValuesJson, int & revision, bool & dataSynch)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetLoad);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, dataSetId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 6);

		dataFilePath	= _wrap_sqlite3_column_text(stmt, 0);
		description		= _wrap_sqlite3_column_text(stmt, 1);
		databaseJson	= _wrap_sqlite3_column_text(stmt, 2);
		emptyValuesJson = _wrap_sqlite3_column_text(stmt, 3);
		revision		= sqlite3_column_int(		stmt, 4);
		dataSynch		= sqlite3_column_int(		stmt, 5);

		Log::log() << "Output loadDataset(dataSetId="<<dataSetId<<") had (dataFilePath='"<<dataFilePath<<"', databaseJson='"<<databaseJson<<"', emptyValuesJson='"<<emptyValuesJson<<"')" << std::endl;
	};

	ensureCorrectDb();
	runStatements("SELECT dataFilePath, description, databaseJson, emptyValuesJson, revision, dataFileSynch FROM DataSets WHERE id = ?;", prepare, processRow);
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

	runStatements("UPDATE " + dataSetName(dataSet) + " SET " + filterName(id) + " = 1;");
}

void DatabaseInterface::filterDelete(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterDelete);
	transactionWriteBegin();

	int dataSetId = filterGetDataSetId(filterIndex);

	if(dataSetId != -1)
		runStatements("ALTER TABLE " + dataSetName(dataSetId) + " DROP COLUMN " + filterName(filterIndex) + ";");
	runStatements("DELETE FROM Filters WHERE id = " + std::to_string(filterIndex) + ";");

	transactionWriteEnd();
}


int DatabaseInterface::filterInsert(int dataSetId, const std::string & rFilter, const std::string & generatedFilter, const std::string & constructorJson, const std::string & constructorR)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterInsert);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int( stmt, 1, dataSetId);
		sqlite3_bind_text(stmt, 2, rFilter.c_str(),				rFilter.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, generatedFilter.c_str(),		generatedFilter.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, constructorJson.c_str(),		constructorJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, constructorR.c_str(),		constructorR.length(),		SQLITE_TRANSIENT);
	};

	transactionWriteBegin();
	
	int id = runStatementsId("INSERT INTO Filters (dataSet, rFilter, generatedFilter, constructorJson, constructorR) VALUES (?, ?, ?, ?, ?) RETURNING rowid;", prepare);
	runStatements("ALTER TABLE " + dataSetName(dataSetId) + " ADD " + filterName(id) +" INT NOT NULL DEFAULT 1;");
	
	transactionWriteEnd();

	return id;
}

//This one only works when there is but 1 filter per dataset, this might change later
int DatabaseInterface::filterGetId(	int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetId);
	int filterId = -1;

	runStatements("SELECT id FROM Filters WHERE dataSet = ?",
		[&](sqlite3_stmt *stmt)				{ sqlite3_bind_int(stmt,	1, dataSetId); },
		[&](size_t row, sqlite3_stmt *stmt)	{ filterId = sqlite3_column_int(stmt, 0); }
	);

	return filterId;
}

int DatabaseInterface::filterGetDataSetId(int filterIndex)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterGetDataSetId);
	return runStatementsId("SELECT dataSet from Filters WHERE id=" + std::to_string(filterIndex));
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

		runStatements("SELECT " + filterName(filterIndex) + " FROM " + dataSetName(dataSet) + " ORDER BY rowNumber;",
		[&](sqlite3_stmt *){ }, [&](size_t row, sqlite3_stmt * stmt)
		{
			int val			= sqlite3_column_int(stmt, 0);
				changed		= changed || bools[row] != val;
				bools[row]	= val;
		});
	}

	transactionReadEnd();

	return changed;
}

void DatabaseInterface::filterUpdate(int filterIndex, const std::string & rFilter, const std::string & generatedFilter, const std::string & constructorJson, const std::string & constructorR)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterUpdate);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_text(stmt, 1, rFilter.c_str(),			rFilter.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 2, generatedFilter.c_str(),	generatedFilter.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, constructorJson.c_str(),	constructorJson.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, constructorR.c_str(),	constructorR.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_int (stmt,	5, filterIndex);
	};

	runStatements("UPDATE Filters SET rFilter=?, generatedFilter=?, constructorJson=?, constructorR=? WHERE id = ?;", prepare);
}

void DatabaseInterface::filterLoad(int filterIndex, std::string & rFilter, std::string & generatedFilter, std::string & constructorJson, std::string & constructorR, int & revision)
{
	JASPTIMER_SCOPE(DatabaseInterface::filterLoad);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, filterIndex);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 5);
		rFilter			= _wrap_sqlite3_column_text(stmt, 0);
		generatedFilter	= _wrap_sqlite3_column_text(stmt, 1);
		constructorJson	= _wrap_sqlite3_column_text(stmt, 2);
		constructorR	= _wrap_sqlite3_column_text(stmt, 3);
		revision		= sqlite3_column_int(		stmt, 4);
	};

	runStatements("SELECT rFilter, generatedFilter, constructorJson, constructorR, revision FROM Filters WHERE id = ?;", prepare, processRow);
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

	const std::string updateFilterPrefix = "UPDATE " + dataSetName(dataSet) + " SET " + filterName(filterIndex) + "= ?  WHERE rowNumber = ?;" ;

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

int DatabaseInterface::columnInsert(int dataSetId, int index, const std::string & name, columnType colType)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnInsert);
	transactionWriteBegin();
	
	if(index == -1)	index = columnLastFreeIndex(dataSetId);
	else			columnIndexIncrements(dataSetId, index);

#ifdef SIR_LOG_A_LOT
	Log::log() << "columnIndex for insert: " << index << " and dataSet: " << dataSetId << std::endl;
#endif

	//Create column entry
	int columnId = runStatementsId("INSERT INTO Columns (dataSet, name, columnType, colIdx, isComputed, analysisId) VALUES (?, ?, ?, ?, 0, -1) RETURNING id;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1, dataSetId);
		sqlite3_bind_text(stmt, 2, name.c_str(), name.length(), SQLITE_TRANSIENT);

		std::string colT = columnTypeToString(colType);
		sqlite3_bind_text(stmt, 3, colT.c_str(), colT.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	4, index);
	});

#ifdef SIR_LOG_A_LOT
	if(columnId == -1)
		Log::log() << "Inserting column failed!" << std::endl;
#endif

	//Add a scalar and ordinal/nominal column to DataSet_# for the column
	const std::string alterDatasetPrefix = "ALTER TABLE " + dataSetName(dataSetId);
	const std::string addColumnFragment  = " ADD  " + columnBaseName(columnId);

	runStatements(alterDatasetPrefix + addColumnFragment + "_DBL REAL NULL;");
	runStatements(alterDatasetPrefix + addColumnFragment + "_INT INT  NULL;");

	//The labels will be added separately later? Probably

	transactionWriteEnd();
	return columnId;
}

int DatabaseInterface::columnGetDataSetId(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetDataSetId);
	return runStatementsId("SELECT dataSet from Columns WHERE id=" + std::to_string(columnId));
}

int	DatabaseInterface::columnLastFreeIndex(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnLastFreeIndex);
	return runStatementsId("SELECT MAX(colIdx) from Columns WHERE dataSet=" + std::to_string(dataSetId) + ";");
}

void DatabaseInterface::columnIndexIncrements(int dataSetId, int index)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIndexIncrements);
	if(columnIdForIndex(dataSetId, index) != -1)
		runStatements("UPDATE Columns SET colIdx=colIdx+1 WHERE dataSet=" + std::to_string(dataSetId) + " AND colIdx >= " + std::to_string(index) +";");
//Actually the following else is not necessary
//	else
//		throw std::runtime_error("columnIndexIncrements has a problem: index " + std::to_string(index) + " in dataSet " + std::to_string(dataSetId) + " already exists!");
}

void DatabaseInterface::columnIndexDecrements(int dataSetId, int index)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnIndexDecrements);
	if(columnIdForIndex(dataSetId, index) == -1)
		runStatements("UPDATE Columns SET colIdx=colIdx-1 WHERE dataSet=" + std::to_string(dataSetId) + " AND colIdx > " + std::to_string(index) +";");
//	else
//		throw std::runtime_error("columnIndexDecrements has a problem: colIdx " + std::to_string(index) + " in dataSet " + std::to_string(dataSetId) + " still exists!");
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

void DatabaseInterface::dataSetBatchedValuesUpdate(DataSet * data, std::function<void(float)> progressCallback)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetBatchedValuesUpdate);

	transactionWriteBegin();

	//Clear the entire dataset, then insert each row, including filter.
	//Bear in mind that this will also erase any scalar values that a column mightve converted from earlier.
	//As this data isnt synced anyway this shouldnt be a problem because it'd be invalidated after a single edit anyway
	runStatements("DELETE FROM " + dataSetName(data->id()));

	std::stringstream statement;

	statement << "INSERT INTO " << dataSetName(data->id()) << " (";

	//Add columnnames for data we want to insert
	for(Column * col : data->columns())
		statement << "Column_" << col->id() << (col->type() == columnType::scale ? "_DBL" : "_INT") << ", ";

	//And the filtername and rowNumber
	statement << filterName(data->filter()->id()) << ", " << "rowNumber) VALUES (";

	for(size_t i=0; i<data->columns().size(); i++)
		statement << "?, ";
	statement << "?, ?);"; //filter and rowNumber

	//We put a size_t outside the bindParamStore lambda to set it without having to change the signature
	size_t rowOutside=0;
	bindParametersType bindParamStore = [&](sqlite3_stmt * stmt)
	{
		size_t i=1;
		for(Column * col : data->columns())
			if(col->type() == columnType::scale)	_doubleTroubleBinder(stmt,	i++, col->dbls()[rowOutside]);
			else									sqlite3_bind_int(	stmt,	i++, col->ints()[rowOutside]);

		sqlite3_bind_int(stmt,	i++, data->filter()->filtered()[rowOutside]);
		sqlite3_bind_int(stmt,	i++, rowOutside+1);
	};

	const float rowsInverse		= 1.0 / float(data->rowCount());
	const int	updateInterval	= std::max(1, data->rowCount() / 100);

	_runStatementsRepeatedly(
		statement.str(),
		[&](bindParametersType ** bindParameters, size_t row)
		{
			if(row >= data->rowCount())
			{
				progressCallback(1);
				return false;
			}

			rowOutside = row;

			static int prevUpdate = 0;

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

	if(data->filter()->id() == -1)
		data->filter()->setId(dataSetGetFilter(data->id()));

	if(data->columns().size() == 0 && data->filter()->id() == -1)
		return;

	transactionReadBegin();

	std::stringstream statement;

	statement << "SELECT ";

	for(Column * col : data->columns())
		statement << "Column_" << col->id() << (col->type() == columnType::scale ? "_DBL" : "_INT") << ", ";

	statement << filterName(data->filter()->id()) << " FROM " << dataSetName(data->id()) << " ORDER BY rowNumber";

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt) {};

	const size_t	rowCount	= dataSetRowCount(data->id());

	for(Column * col : data->columns())
		col->setRowCount(rowCount);

	data->filter()->setRowCount(rowCount);

    size_t rowPercent = std::max(1, int(rowCount) / 100);

	std::function<void(size_t, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
        if(row % rowPercent == 0)
            progressCallback(float(row) / float(rowCount));

		int colCount = sqlite3_column_count(stmt);

		assert(colCount == data->columns().size() + 1);

		for(size_t colI=0; colI<data->columns().size(); colI++)
		{
			Column * col = data->columns()[colI];
			
			if(!sqlite3_column_text(	stmt, colI)) //If string is NULL then column value is NULL, so empty!
			{
				if(col->type() == columnType::scale)	col->setValue(row, NAN,									false);
				else									col->setValue(row, std::numeric_limits<int>::lowest(),	false);
			}
			else
			{
				if(col->type() == columnType::scale)	col->setValue(row, _doubleTroubleReader(	stmt, colI), false);
				else									col->setValue(row, sqlite3_column_int(		stmt, colI), false);
			}
		}

		data->filter()->setFilterValueNoDB(row, sqlite3_column_int(stmt, colCount - 1));
	};

	runStatements(statement.str(), prepare, processRow);

	transactionReadEnd();
}

void DatabaseInterface::columnSetValues(int columnId, const intvec &ints)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetValues);
	transactionWriteBegin();
	
	const int dataSetId = columnGetDataSetId(columnId);
	
	const std::string updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "_INT=? WHERE rowNumber=?";

	size_t rowOutside;
	bindParametersType bindParamStore;

	_runStatementsRepeatedly(
		updateStatement,
		[&](bindParametersType ** bindParameters, size_t row)
		{
			if(row == ints.size())
				return false;

			rowOutside = row;

			bindParamStore = [&](sqlite3_stmt * stmt)
			{
				sqlite3_bind_int(stmt,	1, ints[rowOutside]);
				sqlite3_bind_int(stmt,	2, rowOutside+1);
			};

			(*bindParameters) = &bindParamStore;

			return true;
		});

	transactionWriteEnd();
}

void DatabaseInterface::columnSetValues(int columnId, const doublevec &dbls)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetValues);
	transactionWriteBegin();
	
	const int dataSetId = columnGetDataSetId(columnId);
	
	const std::string updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "_DBL=? WHERE rowNumber=?";

	size_t rowOutside=0;
	bindParametersType bindParamStore = [&](sqlite3_stmt * stmt)
	{
		_doubleTroubleBinder(stmt,	1, dbls[rowOutside]);
		sqlite3_bind_int(stmt,		2, rowOutside+1);
	};

	_runStatementsRepeatedly(
		updateStatement,
		[&](bindParametersType ** bindParameters, size_t row)
		{
			if(row == dbls.size())
				return false;

			rowOutside = row;

			(*bindParameters) = &bindParamStore;

			return true;
		});

	transactionWriteEnd();
}

void DatabaseInterface::columnSetValue(int columnId, size_t row, int value)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetValue);
	const int dataSetId = columnGetDataSetId(columnId);
	
	const std::string updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "_INT=? WHERE rowNumber=?";

	runStatements(updateStatement, [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(	stmt,	1, value);
		sqlite3_bind_int(	stmt,	2, row+1);
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

double DatabaseInterface::_doubleTroubleReader(sqlite3_stmt * stmt, int colI)
{	
	JASPTIMER_SCOPE(DatabaseInterface::_doubleTroubleReader);

	const std::string strVal = _wrap_sqlite3_column_text(stmt, colI);
	
	if(!strVal.empty())
	{
		JASPTIMER_SCOPE(DatabaseInterface::_doubleTroubleReader-TRY);
		
		//Optimization:
		static const std::string _inf		= dbDblToString(dbDbl::inf);
		static const std::string _neg_inf	= dbDblToString(dbDbl::neg_inf);
		static const std::string _nan		= dbDblToString(dbDbl::nan);
		
		if(strVal == _inf)				return std::numeric_limits<double>::infinity();
		else if(strVal == _neg_inf)		return -1 * std::numeric_limits<double>::infinity();
		else if(strVal == _nan)			return NAN;
		
	}

	return sqlite3_column_double(stmt, colI);
}

void DatabaseInterface::columnSetValue(int columnId, size_t row, double value)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetValue);
	const int dataSetId = columnGetDataSetId(columnId);
	
	const std::string updateStatement = "UPDATE Dataset_" + std::to_string(dataSetId)	+ " SET Column_"  + std::to_string(columnId) + "_DBL=? WHERE rowNumber=?";

	runStatements(updateStatement, [&](sqlite3_stmt * stmt)
	{
		_doubleTroubleBinder(stmt,	1, value);
		sqlite3_bind_int(	stmt,	2, row+1);
	});
}



intvec DatabaseInterface::columnGetLabelIds(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetLabelIds);
	intvec out;

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);
		out.push_back(sqlite3_column_int(stmt, 0));

	};

	runStatements("SELECT id FROM Labels WHERE columnId = ? ORDER BY ordering;", prepare, processRow);

	return out;
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


void DatabaseInterface::columnGetValuesInts(int columnId, intvec & ints)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetValuesInts);
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

		ints[row] = sqlite3_column_int(stmt, 0);
	};

	runStatements("SELECT " + columnBaseName(columnId) + "_INT FROM " + dataSetName(dataSet) + " ORDER BY rowNumber;", prepare, processRow);

	transactionReadEnd();
}

void DatabaseInterface::columnGetValuesDbls(int columnId, doublevec & dbls)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetValuesDbls);
	transactionReadBegin();

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	int				dataSet		= columnGetDataSetId(columnId);
	const size_t	rowCount	= dataSetRowCount(dataSet);

	dbls.resize(rowCount);

	//Log::log() << "columnId " << columnId << " has " << rowCount << " values.\n";

	std::function<void(size_t, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 1);

		dbls[row] = _doubleTroubleReader(stmt, 0);
	};

	runStatements("SELECT " + columnBaseName(columnId) + "_DBL FROM " + dataSetName(dataSet) + " ORDER BY rowNumber;", prepare, processRow);

	transactionReadEnd();
}

std::string DatabaseInterface::columnBaseName(int columnId) const
{
	JASPTIMER_SCOPE(DatabaseInterface::columnBaseName);
	return "Column_"  + std::to_string(columnId);
}

std::string DatabaseInterface::dataSetName(int dataSetId) const
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetName);
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

int DatabaseInterface::dataSetGetFilter(int dataSetId)
{
	JASPTIMER_SCOPE(DatabaseInterface::dataSetGetFilter);
	return runStatementsId("SELECT id FROM Filters WHERE dataSet=? LIMIT 1;", [&](sqlite3_stmt *stmt) { sqlite3_bind_int(stmt, 1, dataSetId); });
}

std::string DatabaseInterface::filterName(int filterIndex) const
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

		runStatements(alterDatasetPrefix + addColumnFragment + "_DBL;");
		runStatements(alterDatasetPrefix + addColumnFragment + "_INT;");
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

void DatabaseInterface::columnSetInvalidated(int columnId, bool invalidated)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetInvalidated);
	runStatements("UPDATE Columns SET invalidated=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_int(stmt,	1,	invalidated);
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

void DatabaseInterface::columnSetDescription(int columnId, const std::string & description)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetDescription);
	runStatements("UPDATE Columns SET description=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		sqlite3_bind_text(stmt, 1, description.c_str(), description.length(), SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,	2, columnId);
	});
}

void DatabaseInterface::columnSetComputedInfo(int columnId, int analysisId, bool invalidated, computedColumnType codeType, const std::string & rCode, const std::string & error, const std::string & constructorJsonStr)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnSetComputedInfo);

	runStatements("UPDATE Columns SET isComputed=1, invalidated=?, codeType=?, rCode=?, error=?, constructorJson=?, analysisId=? WHERE id=?;", [&](sqlite3_stmt * stmt)
	{
		std::string codeT = computedColumnTypeToString(codeType);

		sqlite3_bind_int(stmt,  1, int(invalidated));
		sqlite3_bind_text(stmt, 2, codeT.c_str(),				codeT.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 3, rCode.c_str(),				rCode.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 4, error.c_str(),				error.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 5, constructorJsonStr.c_str(),	constructorJsonStr.length(),	SQLITE_TRANSIENT);
		sqlite3_bind_int(stmt,  6, analysisId);
		sqlite3_bind_int(stmt,  7, columnId);
	});
}

void DatabaseInterface::columnGetBasicInfo(int columnId, std::string &name, std::string &title, std::string &description, columnType &colType, int & revision)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetBasicInfo);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 5);
					name		= _wrap_sqlite3_column_text(stmt, 0);
					title		= _wrap_sqlite3_column_text(stmt, 1);
					description	= _wrap_sqlite3_column_text(stmt, 2);
		std::string colTypeStr	= _wrap_sqlite3_column_text(stmt, 3);
		revision				= sqlite3_column_int(		stmt, 4);

		colType = colTypeStr.empty() ? columnType::unknown : columnTypeFromString(colTypeStr);
	};

	runStatements("SELECT name, title, description, columnType, revision FROM Columns WHERE id = ?;", prepare, processRow);
}


std::string DatabaseInterface::_wrap_sqlite3_column_text(sqlite3_stmt * stmt, int iCol)
{
	JASPTIMER_SCOPE(DatabaseInterface::_wrap_sqlite3_column_text);
	const unsigned char * col = sqlite3_column_text(stmt,	iCol);
	
	return !col ? "" : std::string(reinterpret_cast<const char*>(col));	
}

bool DatabaseInterface::columnGetComputedInfo(int columnId, int &analysisId, bool &invalidated, computedColumnType &codeType, std::string &rCode, std::string &error, Json::Value &constructorJson)
{
	JASPTIMER_SCOPE(DatabaseInterface::columnGetComputedInfo);
	bool isComputed = false;

	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, columnId);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 7);

					isComputed			= sqlite3_column_int(		stmt,	0);
					invalidated			= sqlite3_column_int(		stmt,	1);
		std::string codeTypeStr			= _wrap_sqlite3_column_text(stmt,	2);
					rCode				= _wrap_sqlite3_column_text(stmt,	3);
					error				= _wrap_sqlite3_column_text(stmt,	4);
		std::string constructorJsonStr	= _wrap_sqlite3_column_text(stmt,	5);
					analysisId			= sqlite3_column_int(		stmt,	6);

		codeType = codeTypeStr.empty() ? computedColumnType::notComputed : computedColumnTypeFromString(codeTypeStr);

		constructorJson = Json::objectValue;
		Json::Reader().parse(constructorJsonStr, constructorJson);
	};

	runStatements("SELECT isComputed, invalidated, codeType, rCode, error, constructorJson, analysisId FROM Columns WHERE id = ?;", prepare, processRow);

	return isComputed;
}

void DatabaseInterface::labelsClear(int columnId)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsClear);
	runStatements("DELETE FROM Labels WHERE columnId = ?;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt,	1, columnId);
	});
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

void DatabaseInterface::labelSet(int id, int columnId, int value, const std::string & label, bool filterAllows, const	std::string & description, const std::string & originalValueJson)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelSet);
	runStatements("UPDATE Labels SET columnId=?, value=?, label=?, filterAllows=?, description=?, originalValueJson=? "
						   "WHERE id = ?;", [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int( stmt,	1, columnId);
		sqlite3_bind_int( stmt,	2, value);
		sqlite3_bind_text(stmt, 3, label.c_str(),				label.length(),					SQLITE_TRANSIENT);
		sqlite3_bind_int( stmt,	4, filterAllows);
		sqlite3_bind_text(stmt, 5, description.c_str(),			description.length(),			SQLITE_TRANSIENT);
		sqlite3_bind_text(stmt, 6, originalValueJson.c_str(),	originalValueJson.length(),		SQLITE_TRANSIENT);
		sqlite3_bind_int( stmt,	7, id);
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

void DatabaseInterface::labelLoad(int id, int & columnId, int & value, std::string & label, bool & filterAllows, std::string & description, std::string & originalValueJson, int & order)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelLoad);
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, id);
	};

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		assert(colCount == 7);

					columnId			= sqlite3_column_int(stmt,			0);
					value				= sqlite3_column_int(stmt,			1);
					label				= _wrap_sqlite3_column_text(stmt,	2);
					order				= sqlite3_column_int(stmt,			3);
					filterAllows		= sqlite3_column_int(stmt,			4);
					description			= _wrap_sqlite3_column_text(stmt,	5);
					originalValueJson	= _wrap_sqlite3_column_text(stmt,	6);

	};

	runStatements("SELECT columnId, value, label, ordering, filterAllows, description, originalValueJson FROM Labels WHERE id = ?;", prepare, processRow);
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
	
	size_t labelsSize = columnGetLabelCount(column->id());
	
	std::function<void(sqlite3_stmt *stmt)>  prepare = [&](sqlite3_stmt *stmt)
	{
		sqlite3_bind_int(stmt, 1, column->id());
	};
	
	Json::Reader reader;

	std::function<void(size_t row, sqlite3_stmt *stmt)> processRow = [&](size_t row, sqlite3_stmt *stmt)
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
		
		if(column->labels().size() == row)	column->labelsAdd(value, label, filterAllows, description, originalValueJson, order, id);
		else								column->labels()[row]->setInformation(column, id, order, label, value, filterAllows, description, originalValueJson);

	};

	runStatements("SELECT id, value, label, ordering, filterAllows, description, originalValueJson FROM Labels WHERE columnId = ? ORDER BY ordering;", prepare, processRow);
	
	column->labelsRemoveBeyond(labelsSize);
	 
	column->endBatchedLabelsDB(false);
	
	transactionReadEnd();
}

void DatabaseInterface::labelsWrite(Column *column)
{
	JASPTIMER_SCOPE(DatabaseInterface::labelsWrite);
	transactionWriteBegin();

	runStatements("DELETE From Labels WHERE columnId=?", [&](sqlite3_stmt *stmt) { sqlite3_bind_int( stmt,	1, column->id()); });
	
	auto labelIter = column->labels().begin();
	
	if(labelIter != column->labels().end())
	{

		bindParametersType _bindParams =  [&](sqlite3_stmt *stmt)
		{
			const Label			*	label			= *labelIter;
			const std::string		labelDisplay	= label->label(),
									origValJson		= label->originalValueAsString();
			
			
			sqlite3_bind_int( stmt,	1, column->id());
			sqlite3_bind_int( stmt,	2, label->value());
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
	
			label->setId(sqlite3_column_int(stmt, 0));
	
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

std::string DatabaseInterface::dbFile(bool onlyName) const
{
	JASPTIMER_SCOPE(DatabaseInterface::dbFile);

	return onlyName ? "internal.sqlite" : Utils::osPath(TempFiles::sessionDirName() + "/internal.sqlite").string();
}

void DatabaseInterface::runQuery(const std::string & query, std::function<void(sqlite3_stmt *stmt)> bindParameters, std::function<void(size_t row, sqlite3_stmt *stmt)> processRow)
{
	JASPTIMER_SCOPE(DatabaseInterface::runQuery);
	runStatements(query, bindParameters, processRow);
}

void DatabaseInterface::runStatements(	const std::string & statements)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatements);
	_runStatements(statements);
}

void DatabaseInterface::runStatements(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatements);
	_runStatements(statements, &bindParameters);
}

int DatabaseInterface::runStatementsId(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatementsId);
	int id = -1;
	std::function<void(size_t row, sqlite3_stmt *stmt)> processId = [&](size_t row, sqlite3_stmt *stmt)
	{
		int colCount = sqlite3_column_count(stmt);

		if(colCount)
			id = sqlite3_column_int(stmt, 0);

	};

	_runStatements(statements, &bindParameters, &processId);

#ifdef SIR_LOG_A_LOT
	Log::log() << "Output for '" << statements << "' returns id:" << id << std::endl;
#endif
	return id;
}

int DatabaseInterface::runStatementsId(const std::string & statements) 
{
	return runStatementsId(statements, [](sqlite3_stmt *stmt){});
}

void DatabaseInterface::runStatements(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters,	std::function<void(size_t row, sqlite3_stmt *stmt)>	processRow)
{
	JASPTIMER_SCOPE(DatabaseInterface::runStatements);
	_runStatements(statements, &bindParameters, &processRow);
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

void DatabaseInterface::_runStatements(const std::string & statements, bindParametersType * bindParameters, std::function<void(size_t row, sqlite3_stmt *stmt)> * processRow)
{
	JASPTIMER_SCOPE(DatabaseInterface::_runStatements);
#ifdef SIR_LOG_A_LOT
	Log::log() << "Running statements: '" << statements << "'" << std::endl;
#endif

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
		ret	= sqlite3_prepare_v2(_db, current, total - (current - start), &dbStmt, &tail);
		row = 0;

		if(bindParameters)
			(*bindParameters)(dbStmt);

		if(ret == SQLITE_OK && dbStmt)
		{
			do
			{
				ret = sqlite3_step(dbStmt);

				switch(ret)
				{
				case SQLITE_ERROR:
				{
					std::string errorMsg = "Running ```\n"+statements.substr(current - start)+"\n``` failed because of: `" + sqlite3_errmsg(_db);
					Log::log() << errorMsg << std::endl;
					throw std::runtime_error(errorMsg);
				}

				case SQLITE_ROW:
					if(processRow)
						(*processRow)(row, dbStmt);

					break;
				}

				row++;
			}
			while((ret == SQLITE_BUSY || ret == SQLITE_ROW) && ret != SQLITE_DONE);

			ret = sqlite3_finalize(dbStmt);
			dbStmt = nullptr;
		}

		remain	= total - (tail - start);
		//Log::log() << "Just ran `" + std::string(current, tail) + "` which returned " << ret << " and " << remain << " remaining." << std::endl;
		current	= tail;
	}
	while(remain > 1 && (ret == SQLITE_OK && ret != SQLITE_DONE));

	if(ret == SQLITE_ERROR)
	{
		std::string errorMsg = "Running ```\n"+statements+"\n``` failed because of: `" + sqlite3_errmsg(_db);
		Log::log() << errorMsg << std::endl;

		throw std::runtime_error(errorMsg);
	}

	if(ret == SQLITE_READONLY)
	{
		std::string errorMsg = "Running ```\n"+statements+"\n``` failed because the database is readonly...";
		Log::log() << errorMsg << std::endl;
		throw std::runtime_error(errorMsg);
	}
}

void DatabaseInterface::_runStatementsRepeatedly(const std::string & statements, std::function<bool(bindParametersType ** bindParameters, size_t row)> bindParameterFactory, std::function<void(size_t row, size_t repetition, sqlite3_stmt *stmt)> * processRow)
{
	JASPTIMER_SCOPE(DatabaseInterface::_runStatementsRepeatedly);
#ifdef SIR_LOG_A_LOT
	Log::log() << "Running statements repeatedly: '" << statements << "'" << std::endl;
#endif

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
		ret	= sqlite3_prepare_v2(_db, current, total - (current - start), &dbStmt, &tail);

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
						std::string errorMsg = "Running `\n"+statements.substr(current - start)+"\n` repeatedly failed because of: `" + sqlite3_errmsg(_db);
						Log::log() << errorMsg << std::endl;
						throw std::runtime_error(errorMsg);
					}

					case SQLITE_ROW:
						if(processRow)
							(*processRow)(row, repetition, dbStmt);

						break;
					}
				
					row++;
				}
				while((ret == SQLITE_BUSY || ret == SQLITE_ROW) && ret != SQLITE_DONE);
			}
			
			sqlite3_reset(dbStmt);
			repetition++;
		}
		
		if(ret == SQLITE_ERROR)
		{
			std::string errorMsg = "A problem occured trying to prepare statement `" + statements + "` and the error was: : `" + sqlite3_errmsg(_db);
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
		std::string errorMsg = "Running ```\n"+statements+"\n``` failed because of: `" + sqlite3_errmsg(_db);
		Log::log() << errorMsg << std::endl;

		throw std::runtime_error(errorMsg);
	}

	if(ret == SQLITE_READONLY)
	{
		std::string errorMsg = "Running ```\n"+statements+"\n``` failed because the database is readonly...";
		Log::log() << errorMsg << std::endl;
		throw std::runtime_error(errorMsg);
	}
}

void DatabaseInterface::create()
{
	JASPTIMER_SCOPE(DatabaseInterface::create);
	assert(!_db);

	if(std::filesystem::exists(dbFile()))
	{
		Log::log() << "DatabaseInterface::create: Removing existing sqlite internal db at " << dbFile() << std::endl;
		std::filesystem::remove(dbFile());
	}
	
	int ret = sqlite3_open_v2(dbFile().c_str(), &_db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_FULLMUTEX, NULL);

	if(ret != SQLITE_OK)
	{
		Log::log() << "Couldnt open sqlite internal db, because of: " << (_db ? sqlite3_errmsg(_db) : "not even a broken sqlite3 obj was returned..." ) << std::endl;
		throw std::runtime_error("JASP cannot run without an internal database and it cannot be created. Contact the JASP team for help.");
	}
	else
		Log::log() << "Opened internal sqlite database for creation at '" << dbFile() << "'." << std::endl;


	transactionWriteBegin();
	runStatements(_dbConstructionSql);
	transactionWriteEnd();
}

void DatabaseInterface::load()
{
	JASPTIMER_SCOPE(DatabaseInterface::load);
	assert(!_db);

	if(!std::filesystem::exists(dbFile()))
		throw std::runtime_error("Trying to load '" + dbFile() + "' but it doesn't exist!");

	int ret = sqlite3_open_v2(dbFile().c_str(), &_db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_FULLMUTEX, NULL);

	if(ret != SQLITE_OK)
	{
		Log::log() << "Couldnt open sqlite internal db, because of: " << (_db ? sqlite3_errmsg(_db) : "not even a broken sqlite3 obj was returned..." ) << std::endl;
		throw std::runtime_error("JASP cannot run without an internal database and it cannot be created. Contact the JASP team for help.");
	}
	else
		Log::log() << "Opened internal sqlite database for loading at '" << dbFile() << "'." << std::endl;

	ensureCorrectDb();
}

void DatabaseInterface::close()
{
	JASPTIMER_SCOPE(DatabaseInterface::);
	if(_db)
	{
		sqlite3_close(_db);
		_db = nullptr;
	}
}

void DatabaseInterface::ensureCorrectDb()
{
	transactionWriteBegin();

	// Check whether the description column exists in DataSets table
	int count = runStatementsId("SELECT COUNT(*) AS CNTREC FROM pragma_table_info('DataSets') WHERE name='description'");
	if (count == 0)
		runStatements("ALTER TABLE DataSets ADD COLUMN description TEXT");

	transactionWriteEnd();

}

void DatabaseInterface::transactionWriteBegin()
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionWriteBegin);
	assert(_transactionReadDepth == 0);
	
	if(_transactionWriteDepth++ == 0)
		runStatements("BEGIN EXCLUSIVE"); //runStatements already has a while loop handling SQLITE_BUSY so this should work?
}

void DatabaseInterface::transactionReadBegin()
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionReadBegin);
	assert(_transactionWriteDepth == 0);
	
	if(_transactionReadDepth++ == 0)
		runStatements("BEGIN DEFERRED");
}

void DatabaseInterface::transactionWriteEnd(bool rollback)
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionWriteEnd);
	assert(_transactionWriteDepth > 0);
	
	if(rollback)	
	{
		runStatements("ROLLBACK");
		_transactionWriteDepth = 0;
		throw std::runtime_error("Rollback!"); //Might be better to use a subclass of std::runtime_error but for now this isnt even used anyway.
	}	
	else if(--_transactionWriteDepth == 0)
		runStatements("COMMIT");
}

void DatabaseInterface::transactionReadEnd()
{
	JASPTIMER_SCOPE(DatabaseInterface::transactionReadEnd);
	assert(_transactionReadDepth > 0);
	
	if(--_transactionReadDepth == 0)
		runStatements("COMMIT");
}

