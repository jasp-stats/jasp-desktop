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
#ifndef DATABASEINTERFACE_H
#define DATABASEINTERFACE_H

#include "columntype.h"
#include <string>
#include "utils.h"
#include <json/json.h>
#include "version.h"
#include <functional>
#include <mutex>
#include <thread>

class DataSet;
class Column;
typedef std::vector<Column*> Columns;
class DatabaseInterface;
struct sqlite3_stmt;
struct sqlite3;

class dbMalformedException : public std::runtime_error
{
public:
	dbMalformedException() : std::runtime_error("Database file is malformed!") {}
	~dbMalformedException() {}
};

///Single point of interaction with sqlite, can later be turned into an interface for supporting other sql
///
/// This class represents the abstraction layer between DataSetPackage and the SQLite backend
/// Originally the intent was to also write it agnostic towards the actual data representation
/// So only using basic datatypes for interaction.
/// But performance considerations changed that equation, so to make bulk datatransfer possible
/// DataSet and Column are used directly. This could later also be done with other models of course.
///
/// The initial structure of a fresh database can be found in _dbConstructionSql and contains some general tables:
/// DataSets, Filters, Columns and Labels
/// Whenever a dataset is created it gets its own entry in DataSets describing it
/// and also a table named as for instance: DataSet_0 is created.
///
/// This DataSet_# table intially only contains a default filter column (for which an entry is made in Filters)
///
/// Then when columns are loaded/added each gets an entry in Columns describing it.
/// DataSet_# then also adds 2 columns for each actual column, both initially filled with NULLs
/// An integers column for ordinal and nominal(text) columns, and basically any future column with labels (those labels might be integers)
/// A double column for scalar columns, and perhaps later monetary or time related columns
///
/// As values are set they can be stored value for value (during manual editing) or bulked.
/// This is then represented in the linked column(s) in DataSet_#
/// 
/// The tables DataSets, Filters and Columns all have a field "revision"
/// This is incremented whenever a change is made. So if a single value in a column changes
/// its corresponding Column has "revision++". If a column is removed or added the same
/// "revision++" is done for DataSets. As for Filters, im sure you get the gist of it.
/// 
/// As each side (Desktop and Engine) both have datastructures that map to these tables,
/// they also have a "revision" field and so they can, and do, regurlarly check for it to synchronise
/// their loaded data.
/// 
/// General table structure (an example with a single dataset and support for a single filter
/// 
/// DataSets [ id, info... ] -> DataSet_1 [ row, Filter_1, Column_1_INT, Column_1_DBL, Column_2_int, ... ]
///		|---------------------> Filters [id, info...] 
///		|---------------------> Column  [id, info...] -> Labels [ id, columnId, info... ]
/// 
class DatabaseInterface
{
public:
	typedef std::function<void(sqlite3_stmt *stmt)> bindParametersType;

				DatabaseInterface(bool create = false, bool inMemory = false);									///< Creates or loads a sqlite database based on the argument
				~DatabaseInterface();

	std::string dbFile(bool onlyPostfix = false) const;									///< Convenience function for getting the filename where sqlite db should be

	static		DatabaseInterface * singleton();					///< There can be only one! https://www.youtube.com/watch?v=sqcLjcSloXs
	static		void				closeInterfaces();

	bool		hasConnection() { return _db(); }
	void		upgradeDBFromVersion(Version originalVersion);							///< Ensures that the database has all the fields configured as required for the current JASP version, useful when loading older sqlite-containing jasp-files

	void		runQuery(			const std::string & query,		std::function<void(sqlite3_stmt *stmt)>		bindParameters,				std::function<void(size_t row, sqlite3_stmt *stmt)>		processRow);	///< Runs a single query and then goes through the resultrows while calling processRow for each.
	void		runStatements(		const std::string & statements, bool ignoreFails=false);																																				///< Runs several sql statements without looking at the results.
	int			runStatementsId(	const std::string & statements, bool ignoreFails=false);																																				///< Runs several sql statements only looking for a single returned value from the results.
	intset		runStatementsIds(	const std::string & statements, bool ignoreFails=false);
	void		runStatements(		const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters, bool ignoreFails=false);																						///< Runs several sql statements without looking at the results. Arguments can be set by supplying bindParameters.
	int			runStatementsId(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters, bool ignoreFails=false);																						///< Runs (several) sql statements and only looks for a single value, this would usually be a id resulting from an insert
	intset		runStatementsIds(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters, bool ignoreFails=false);
	void		runStatements(		const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters,	std::function<void(size_t row, sqlite3_stmt *stmt)>	processRow, bool ignoreFails=false);						///< Runs several sql statements. Arguments can be set by supplying bindParameters and use processRow to read from the results.

	//Workspaces
	void		workspaceUpdate(		bool showRSyntax);	
	void		workspaceLoad(			bool & showRSyntax);
	
	//DataSets
	int			dataSetCount();
	intset		dataSetIds();
	bool		dataSetExists(			int dataSetId);
	void		dataSetDelete(			int dataSetId);
int			dataSetInsert(							const std::string & dataFilePath = "", long dataFileTimestamp = 0, const std::string & description = "", const std::string & databaseJson = "", const std::string & emptyValuesJson = "", bool dataSynch = false, char csvDelimiter = '\0');		///< Inserts a new DataSet row into DataSets and creates an empty DataSet_#id. returns id
	void		dataSetUpdate(			int dataSetId,	const std::string & title = "", const std::string & dataFilePath = "", long dataFileTimestamp = 0, const std::string & description = "", const std::string & databaseJson = "", const std::string & emptyValuesJson = "", bool dataSynch = false, char csvDelimiter = '\0');		///< Updates an existing DataSet row in DataSets
	void		dataSetLoad(			int dataSetId,		  std::string & title,		  std::string & dataFilePath,		long & dataFileTimestamp,		 std::string & description,			   std::string & databaseJson,			   std::string & emptyValuesJson, int & revision, bool & dataSynch, char & csvDelimiter);	///< Loads an existing DataSet row into arguments
	void		dataSetSetComputedInfo(	int dataSetId, bool invalidated, computedColumnType codeType, const std::string & rCode, const std::string & error, int defaultInputFilter);
	void		dataSetGetComputedInfo(	int dataSetId, bool & invalidated, computedColumnType & codeType, std::string & rCode, std::string & error, int & defaultInputFilter);
	static int	dataSetColCount(		int dataSetId);
	static int	dataSetRowCount(		int dataSetId);
	void		dataSetSetRowCount(		int dataSetId, size_t rowCount);
	std::string dataSetName(			int dataSetId) const;
	int			dataSetIncRevision(		int dataSetId);
	int			dataSetGetRevision(		int dataSetId);
	intset		dataSetGetFilters(		int dataSetId);
	int			dataSetGetDefaultFilter(int dataSetId);
	void		dataSetInsertEmptyRow(	int dataSetId, size_t row);
	stringset	dataSetFilterNames(		int dataSetId);
	void		dataSetCreateTable(		DataSet * dataSet); ///< Assumes you are importing fresh data and havent created any DataSet_? table yet

	void		dataSetBatchedValuesUpdate(DataSet * data, Columns columns, std::function<void(float)> progressCallback = [](float){});
	void		dataSetBatchedValuesUpdate(DataSet * data, std::function<void(float)> progressCallback = [](float){});

	//Filters
	std::string filterTableName(		int filterId) const;
	int			filterGetId(			int dataSetId);
	int			filterGetId(			int dataSetId,		const std::string & name);
	bool		filterSelect(			int filterId,				  boolvec & bools);																	///< Loads result and errorMsg and returns whether there was a change in either of those.
	void		filterWrite(			int filterId,		const	  boolvec & values);																	///< Overwrites the current filter values, no checks are done on the size. If too few the rest is TRUE nd superfluous bools are ignored.
	int			filterInsert(			int dataSetId,		const std::string & rFilter = "", const std::string & generatedFilter = "", const std::string & constructorJson = "", const std::string & constructorR = "", const std::string & name = "");		///< Inserts a new Filter row into Filters and creates an empty FilterValues_#id. It returns id
	void		filterUpdate(			int filterId,		const std::string & rFilter = "", const std::string & generatedFilter = "", const std::string & constructorJson = "", const std::string & constructorR = "", const std::string & name = "", bool   invalidated = false);		///< Updates an existing Filter row in Filters
	void		filterLoad(				int filterId,			  std::string & rFilter,			std::string & generatedFilter,			  std::string & constructorJson,			std::string & constructorR, int & revision, std::string & name, bool & invalidated);			///< Loads an existing Filter row into arguments
	void		filterClear(			int filterId);																					///< Clears all values in Filter
	void		filterDelete(			int filterId);
	int			filterGetDataSetId(		int filterId);
	std::string	filterGetName(			int filterId);
	std::string	filterLoadErrorMsg(		int filterId);
	void		filterUpdateErrorMsg(	int filterId,		const std::string & errorMsg);
	int			filterIncRevision(		int filterId);
	int			filterGetRevision(		int filterId);

	//Columns & Data/Values
	//Index stuff:
	int			columnInsert(			int dataSetId, int index = -1, const std::string & name = "", columnType colType = columnType::unknown, bool alterTable=true);	///< Insert a row into Columns and create the corresponding columns in DataSet_? Also makes sure the indices are correct
	intvec		columnsInsert(			int dataSetId, int count, int index, const std::string &name, columnType colType, bool alterTable);
	int			columnLastFreeIndex(	int dataSetId);
	void		columnIndexIncrements(	int dataSetId, int index, int count);																			///< If index already is in use that column and all after are incremented by 1
	void		columnIndexDecrements(	int dataSetId, int index);																			///< Indices bigger than index are decremented, assumption is that the previous one using it has been removed already
	int			columnIdForIndex(		int dataSetId, int index);
	int			columnIndexForId(		int columnId);
	void		columnSetIndex(			int columnId, int index);		///< If this is used by JASP and changes the index the assumption is all will be brought in order. By setting the indices correct for all columns.
	int			columnIncRevision(		int columnId);
	int			columnGetRevision(		int columnId);

	//id stuff:
	int			columnGetDataSetId(			int columnId);
	void		columnDelete(				int columnId, bool					cleanUpRest = true);			///< Also makes sure indices stay as contiguous and correct as before. disable cleanUpRest to just clear from Columns
	void		columnSetType(				int columnId, columnType			colType);
	void		columnSetInvalidated(		int columnId, bool					invalidated);
	void		columnSetAutoSort(			int columnId, bool					sort);
	void		columnSetHasLabels(			int columnId, bool					hasLabels);
	void		columnSetDropLevels(		int columnId, int					dropLevels);
	void		columnSetName(				int columnId, const std::string &	name);
	void		columnSetTitle(				int columnId, const std::string &	title);
	void		columnSetEmptyVals(			int columnId, const std::string &	emptyValsJson);
	void		columnSetDescription(		int columnId, const std::string &	description);
	void		columnSetComputeFilter(		int columnId, const std::string &	computeFilter);
	void		columnGetBasicInfo(			int columnId,		std::string &	name, std::string & title, std::string & description, columnType & colType, int & revision, Json::Value & emptyValuesJson, bool & autoSort, int & dropLevels, bool & hasLabels);
	void		columnSetComputedInfo(		int columnId, int analysisId,  bool   invalidated, computedColumnType   codeType, const	std::string & rCode, const	std::string & error, const	std::string & constructorJson, const std::string & computeFilter);
	void		columnGetComputedInfo(		int columnId, int &analysisId, bool & invalidated, computedColumnType & codeType,		std::string & rCode,		std::string & error,		Json::Value & constructorJson, std::string & computeFilter);
	void		columnSetValues(			int columnId, const intvec	  & ints);
	void		columnSetValues(			int columnId, const doublevec & dbls, const stringvec & strs);
	void		columnSetValue(				int columnId, size_t row, int valueInt);
	void		columnSetValue(				int columnId, size_t row, double dbl, const std::string & str);
	size_t		columnGetLabelCount(		int columnId);
	void		columnGetValues(			int columnId,	intvec		& ints,						const std::string & postFix = "");
	void		columnGetValues(			int columnId,	doublevec	& dbls, stringvec & strs,	const std::string & postFix = "");
	std::string columnBaseName(				int columnId,	const std::string & postFix = "") const;
	
	void		dataSetBatchedValuesLoad(	DataSet * data, std::function<void(float)> progressCallback = [](float){});
	void		dataSetBatchedLabelsLoad(	DataSet * data, std::function<void(float)> progressCallback = [](float){});

	//Labels
	void		labelsClear(			int columnId);
	int			labelAdd(				int columnId,	int value, const std::string & label, bool filterAllows, const	std::string & description = "", const	std::string & originalValueJson = "");
	void		labelSet(		int id,	int columnId,	int value, const std::string & label, bool filterAllows, const	std::string & description = "", const	std::string & originalValueJson = "", bool userAdded = false);
	void		labelDelete(	int id);
	void		labelLoad(		int id,	int & columnId,	int & value,	 std::string & label, bool & filterAllows,		std::string & description,				std::string & originalValueJson,	int & order, bool & userAdded);
	void		labelSetOrder(	int id, int order);
	bool		labelExists(	int	columnId, int intsId);
	intset		labelsExisting(	int columnId);
	void		labelsLoad(			Column  * column);
	void		labelsLoad(	const	Columns & columns);//, std::function<void(float)> progressCallback);
	void		labelsWrite(const	Columns & columns, std::function<void(float)> progressCallback);
	void		labelsWrite(		Column  * column);
	void		labelsSetOrder(	const intintmap & orderPerDbId);

	//Transactions
	void		transactionWriteBegin();						///< runs BEGIN EXCLUSIVE and waits for sqlite to not be busy anymore if some other process is writing. Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	void		transactionWriteEnd(bool rollback = false);		///< runs COMMIT or ROLLBACK based on rollback and ends the transaction.  Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	void		transactionReadBegin();							///< runs BEGIN DEFERRED and waits for sqlite to not be busy anymore if some other process is writing  Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	void		transactionReadEnd();							///< runs COMMIT and ends the transaction. Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	
	//Miscellaneous
	void		truncateAllTables();
	bool		tableHasColumn(const std::string & tableName, const std::string & columnName);
	bool		tableExists(const std::string & name);
	int			transactionWriteDepth();
	int			transactionReadDepth();

    void        preloadInterfaceForThread();
	void		close();					///< Closes the loaded database and disconnects
	void		load();						///< Loads a sqlite database from sessiondir (after loading a jaspfile)

	
	
private:
	sqlite3	*	_db();
	void		_upgradeDBStatements(Version originalVersion);							///< The bare migration ALTER statements; upgradeDBFromVersion() wraps these in a transaction with rollback-on-failure.
	void		_doubleTroubleBinder(sqlite3_stmt *stmt, int param, double dbl);	///< Needed to work around the lack of support for NAN, INF and NEG_INF in sqlite, converts those to string to make use of sqlite flexibility
	double		_doubleTroubleReader(sqlite3_stmt *stmt, int colI, std::string * textReturn = nullptr);					///< The reading counterpart to _doubleTroubleBinder to convert string representations of NAN, INF and NEG_INF back to double
	void		_runStatements(				const std::string & statements,						std::function<void(sqlite3_stmt *stmt)> *	bindParameters = nullptr,	std::function<void(size_t row, sqlite3_stmt *stmt)> *	processRow = nullptr, bool ignoreFails = false);	///< Runs several sql statements without looking at the results. Unless processRow is not NULL, then this is called for each row.
	void		_runStatementsRepeatedly(	const std::string & statements, std::function<bool(	std::function<void(sqlite3_stmt *stmt)> **	bindParameters, size_t row)> bindParameterFactory, std::function<void(size_t row, size_t repetition, sqlite3_stmt *stmt)> * processRow = nullptr, bool ignoreFails = false);

	void		create();					///< Creates a new sqlite database in sessiondir and loads it

	std::map<std::thread::id, sqlite3*>		_dbs;
	std::thread::id							_dbCreator;
	sqlite3*								_dbCreated = nullptr;
	bool									_inMemory;
	std::mutex                              _loadMutex,
											_dbCheckMutex;

	static				std::string _wrap_sqlite3_column_text(sqlite3_stmt * stmt, int iCol);
	static const	std::string _dbConstructionSql;
	static const	std::string _dbIndexesSql;
	
	static DatabaseInterface * _singleton;

	friend class DataSetPackage;
	
	
	//chunk
	static const int CHUNK_SIZE = 1000;		// Number of rows contained in each chunks
	
	// chunk serialization/deserialization
	static std::vector<unsigned char>			serializeIntChunk(const intvec &ints,						size_t startRow,					size_t count);
	static std::vector<unsigned char>			serializeDoubleChunk(const doublevec &dbls,			const stringvec &strs,		size_t startRow,	size_t count);
	static void														deserializeIntChunk(const unsigned char *data,	size_t dataSize,					intvec &ints,			size_t startRow, size_t expectedCount);
	static void														deserializeDoubleChunk(const unsigned char *data, size_t dataSize,				doublevec &dbls, stringvec &strs, size_t startRow, size_t expectedCount);
	
	// read/write column chunks
	void					writeColumnChunks		(	int columnId,		const intvec		&ints);
	void					writeColumnChunks		(	int columnId,		const doublevec &dbls,	const stringvec &strs);
	void					readColumnChunks		(	int columnId,		intvec &ints);
	void					readColumnChunks		(	int columnId,		doublevec &dbls,				stringvec &strs);
	void					deleteColumnChunks	(	int columnId);
	
	// update single value
	void					setIntChunkValue		(	int columnId,		size_t row,			int value													);
	void					setDoubleChunkValue	(	int columnId,		size_t row,			double val, const std::string &str);
	
	// adjust column row count
	void					adjustIntColumnRowCount		(	int columnId, size_t oldRowCount, size_t newRowCount);
	void					adjustDoubleColumnRowCount(	int columnId, size_t oldRowCount, size_t newRowCount);
	
	// helper: get IDs and types of all columns in a dataset
	std::vector<std::pair<int, bool>> getColumnsForDataSet(int dataSetId);
	
	bool				readChunkBlob				(	int columnId,			int chunkId,				std::vector<unsigned char> &blob);
	void				writeChunkBlob				(	int columnId,			int chunkId,				const std::vector<unsigned char> &blob);
	void				growIntColumnChunks	(	int columnId,			size_t oldRowCount, size_t newRowCount);
	void				shrinkIntColumnChunks(	int columnId,			size_t newRowCount);
	
	//	migrate these DataSet_# to DataChunks table
	bool				hasDataChunksTable();
	void				migrateLegacyWideTableToDataChunks();
	
	
};

#endif // DATABASEINTERFACE_H
