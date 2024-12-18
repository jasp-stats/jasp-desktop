#ifndef DATABASEINTERFACE_H
#define DATABASEINTERFACE_H

#include "columntype.h"
#include <sqlite3.h>
#include <string>
#include "utils.h"
#include <json/json.h>
#include "version.h"


class DataSet;
class Column;
class DatabaseInterface;

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

	static		DatabaseInterface * singleton() { return _singleton; }					///< There can be only one! https://www.youtube.com/watch?v=sqcLjcSloXs

	bool		hasConnection() { return _db; }
	void		upgradeDBFromVersion(Version originalVersion);							///< Ensures that the database has all the fields configured as required for the current JASP version, useful when loading older sqlite-containing jasp-files

	void		runQuery(		const std::string & query,		std::function<void(sqlite3_stmt *stmt)>		bindParameters,				std::function<void(size_t row, sqlite3_stmt *stmt)>		processRow);	///< Runs a single query and then goes through the resultrows while calling processRow for each.
	void		runStatements(	const std::string & statements);																																				///< Runs several sql statements without looking at the results.
	int			runStatementsId(const std::string & statements);																																				///< Runs several sql statements only looking for a single returned value from the results.
	void		runStatements(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters);																						///< Runs several sql statements without looking at the results. Arguments can be set by supplying bindParameters.
	int			runStatementsId(const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters);																						///< Runs (several) sql statements and only looks for a single value, this would usually be a id resulting from an insert
	void		runStatements(	const std::string & statements, std::function<void(sqlite3_stmt *stmt)>	bindParameters,	std::function<void(size_t row, sqlite3_stmt *stmt)>	processRow);						///< Runs several sql statements. Arguments can be set by supplying bindParameters and use processRow to read from the results.

	//DataSets
	int			dataSetGetId();
	bool		dataSetExists(			int dataSetId);
	void		dataSetDelete(			int dataSetId);
	int			dataSetInsert(							const std::string & dataFilePath = "", long dataFileTimestamp = 0, const std::string & description = "", const std::string & databaseJson = "", const std::string & emptyValuesJson = "", bool dataSynch = false);		///< Inserts a new DataSet row into DataSets and creates an empty DataSet_#id. returns id
	void		dataSetUpdate(			int dataSetId,	const std::string & dataFilePath = "", long dataFileTimestamp = 0, const std::string & description = "", const std::string & databaseJson = "", const std::string & emptyValuesJson = "", bool dataSynch = false);		///< Updates an existing DataSet row in DataSets
	void		dataSetLoad(			int dataSetId,		  std::string & dataFilePath,	long & dataFileTimestamp,		 std::string & description,			   std::string & databaseJson,			  std::string & emptyValuesJson, int & revision, bool & dataSynch);	///< Loads an existing DataSet row into arguments
	static int	dataSetColCount(		int dataSetId);
	static int	dataSetRowCount(		int dataSetId);
	void		dataSetSetRowCount(		int dataSetId, size_t rowCount);
	std::string dataSetName(			int dataSetId) const;
	int			dataSetIncRevision(		int dataSetId);
	int			dataSetGetRevision(		int dataSetId);
	int			dataSetGetFilter(		int dataSetId);
	void		dataSetInsertEmptyRow(	int dataSetId, size_t row);
	void		dataSetCreateTable(		DataSet * dataSet); ///< Assumes you are importing fresh data and havent created any DataSet_? table yet

	void		dataSetBatchedValuesUpdate(DataSet * data, std::vector<Column*> columns, std::function<void(float)> progressCallback = [](float){});
	void		dataSetBatchedValuesUpdate(DataSet * data, std::function<void(float)> progressCallback = [](float){});

	//Filters
	std::string filterTableName(		int filterIndex) const;
	int			filterGetId(			int dataSetId);
	int			filterGetId(			const std::string & name);
	bool		filterSelect(			int filterIndex,			boolvec & bools);																	///< Loads result and errorMsg and returns whether there was a change in either of those.
	void		filterWrite(			int filterIndex,	const	boolvec & values);																	///< Overwrites the current filter values, no checks are done on the size. If too few the rest is TRUE nd superfluous bools are ignored.
	int			filterInsert(			int dataSetId,		const std::string & rFilter = "", const std::string & generatedFilter = "", const std::string & constructorJson = "", const std::string & constructorR = "", const std::string & name = "");		///< Inserts a new Filter row into Filters and creates an empty FilterValues_#id. It returns id
	void		filterUpdate(			int filterIndex,	const std::string & rFilter = "", const std::string & generatedFilter = "", const std::string & constructorJson = "", const std::string & constructorR = "", const std::string & name = "");		///< Updates an existing Filter row in Filters
	void		filterLoad(				int filterIndex,		  std::string & rFilter,			std::string & generatedFilter,			  std::string & constructorJson,			std::string & constructorR, int & revision, std::string & name);			///< Loads an existing Filter row into arguments
	void		filterClear(			int filterIndex);																					///< Clears all values in Filter
	void		filterDelete(			int filterIndex);
	int			filterGetDataSetId(		int filterIndex);
	std::string	filterGetName(			int filterIndex);
	std::string	filterLoadErrorMsg(		int filterIndex);
	void		filterUpdateErrorMsg(	int filterIndex, const	std::string & errorMsg);
	int			filterIncRevision(		int filterIndex);
	int			filterGetRevision(		int filterIndex);

	//Columns & Data/Values
	//Index stuff:
	int			columnInsert(			int dataSetId, int index = -1, const std::string & name = "", columnType colType = columnType::unknown, bool alterTable=true);	///< Insert a row into Columns and create the corresponding columns in DataSet_? Also makes sure the indices are correct
	int			columnLastFreeIndex(	int dataSetId);
	void		columnIndexIncrements(	int dataSetId, int index);																			///< If index already is in use that column and all after are incremented by 1
	void		columnIndexDecrements(	int dataSetId, int index);																			///< Indices bigger than index are decremented, assumption is that the previous one using it has been removed already
	int			columnIdForIndex(		int dataSetId, int index);
	int			columnIndexForId(		int columnId);
	void		columnSetIndex(			int columnId, int index);		///< If this is used by JASP and changes the index the assumption is all will be brought in order. By setting the indices correct for all columns.
	int			columnIncRevision(		int columnId);
	int			columnGetRevision(		int columnId);

	//id stuff:
	int			columnGetDataSetId(			int columnId);
	void		columnDelete(				int columnId, bool cleanUpRest = true);			///< Also makes sure indices stay as contiguous and correct as before. disable cleanUpRest to just clear from Columns
	void		columnSetType(				int columnId, columnType colType);
	void		columnSetAutoSort(			int columnId, bool sort);
	void		columnSetInvalidated(		int columnId, bool invalidated);
	void		columnSetName(				int columnId, const std::string & name);
	void		columnSetTitle(				int columnId, const std::string & title);
	void		columnSetEmptyVals(			int columnId, const std::string & emptyValsJson);
	void		columnSetDescription(		int columnId, const std::string & description);
	void		columnGetBasicInfo(			int columnId,		std::string & name, std::string & title, std::string & description, columnType & colType, int & revision, Json::Value & emptyValuesJson, bool & autoSort);
	void		columnSetComputedInfo(		int columnId, int analysisId,  bool   invalidated, computedColumnType   codeType, const	std::string & rCode, const	std::string & error, const	std::string & constructorJson);
	void		columnGetComputedInfo(		int columnId, int &analysisId, bool & invalidated, computedColumnType & codeType,		std::string & rCode,		std::string & error,		Json::Value & constructorJson);
	void		columnSetValues(			int columnId, const intvec	  & ints, const doublevec & dbls);
	void		columnSetValue(				int columnId, size_t row, int valueInt, double valueDbl);
	intvec		columnGetLabelIds(			int columnId);
	size_t		columnGetLabelCount(		int columnId);
	void		columnGetValues(			int columnId,	intvec		& ints, doublevec & dbls);
	std::string columnBaseName(				int columnId) const;
	void		dataSetBatchedValuesLoad(	DataSet * data, std::function<void(float)> progressCallback = [](float){});

	//Labels
	void		labelsClear(			int columnId);
	int			labelAdd(				int columnId,	int value, const std::string & label, bool filterAllows, const	std::string & description = "", const	std::string & originalValueJson = "");
	void		labelSet(		int id,	int columnId,	int value, const std::string & label, bool filterAllows, const	std::string & description = "", const	std::string & originalValueJson = "");
	void		labelDelete(	int id);
	void		labelLoad(		int id,	int & columnId,	int & value,	 std::string & label, bool & filterAllows,		std::string & description,				std::string & originalValueJson,	int & order);
	void		labelSetOrder(	int id, int order);
	void		labelsLoad(		Column * column);
	void		labelsWrite(	Column * column);
	void		labelsSetOrder(	const intintmap & orderPerDbId);

	//Transactions
	void		transactionWriteBegin();						///< runs BEGIN EXCLUSIVE and waits for sqlite to not be busy anymore if some other process is writing. Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	void		transactionWriteEnd(bool rollback = false);		///< runs COMMIT or ROLLBACK based on rollback and ends the transaction.  Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	void		transactionReadBegin();							///< runs BEGIN DEFERRED and waits for sqlite to not be busy anymore if some other process is writing  Tracks whether nested and only does BEGIN+COMMIT at lowest depth
	void		transactionReadEnd();							///< runs COMMIT and ends the transaction. Tracks whether nested and only does BEGIN+COMMIT at lowest depth
		
private:
	void		_doubleTroubleBinder(sqlite3_stmt *stmt, int param, double dbl);	///< Needed to work around the lack of support for NAN, INF and NEG_INF in sqlite, converts those to string to make use of sqlite flexibility
	double		_doubleTroubleReader(sqlite3_stmt *stmt, int colI);					///< The reading counterpart to _doubleTroubleBinder to convert string representations of NAN, INF and NEG_INF back to double
	void		_runStatements(				const std::string & statements,						std::function<void(sqlite3_stmt *stmt)> *	bindParameters = nullptr,	std::function<void(size_t row, sqlite3_stmt *stmt)> *	processRow = nullptr);	///< Runs several sql statements without looking at the results. Unless processRow is not NULL, then this is called for each row.
	void		_runStatementsRepeatedly(	const std::string & statements, std::function<bool(	std::function<void(sqlite3_stmt *stmt)> **	bindParameters, size_t row)> bindParameterFactory, std::function<void(size_t row, size_t repetition, sqlite3_stmt *stmt)> * processRow = nullptr);

	void		create();					///< Creates a new sqlite database in sessiondir and loads it
	void		load();						///< Loads a sqlite database from sessiondir (after loading a jaspfile)
	void		close();										///< Closes the loaded database and disconnects
	bool		tableHasColumn(const std::string & tableName, const std::string & columnName);

	int			_transactionWriteDepth	= 0,
				_transactionReadDepth	= 0;

	sqlite3	*	_db = nullptr;
	bool		_inMemory = false;

	static			std::string _wrap_sqlite3_column_text(sqlite3_stmt * stmt, int iCol);
	static const	std::string _dbConstructionSql;


	static DatabaseInterface * _singleton;

	friend class DataSetPackage;
	
};

#endif // DATABASEINTERFACE_H
