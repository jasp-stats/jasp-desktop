#ifndef DATASET_H
#define DATASET_H

#include "datasetbasenode.h"
#include "column.h"
#include "filter.h"
#include "emptyvalues.h"
#include "version.h"

class DataSet : public DataSetBaseNode
{
public:
	typedef 	std::map<std::string,columnType> colTypeMap;
	
							DataSet(int index = -1); ///< index==-1: create a new dataSet, >0: load that dataSet, 0: do nothing
							~DataSet();
	
			Filter		*	filter()						{ return	_filter;	}
			Columns		&	columns()			const		{ return	const_cast<Columns&>(_columns);	}
    const	EmptyValues *	emptyValues()       const		{ return	_emptyValues; }
			EmptyValues *	emptyValues()					{ return	_emptyValues; }

			Column		*	column(		const std::string & name);
			Column		*	column(		size_t				columnIndex);

			Column		*	operator[](	size_t				columnIndex)	{ return column(columnIndex); }
			Column		*	operator[](	const std::string &	columnName)		{ return column(columnName); }
	
			int				id()					const { return _dataSetID;				}
			int				columnCount()			const ;
			int				rowCount()				const ;
			bool			dataFileSynch()			const { return _dataFileSynch;			}
			bool			showRSyntax()			const { return _showRSyntax;			}
	const	std::string &	dataFilePath()			const { return _dataFilePath;			}
			int				dataFileTimestamp()		const { return _dataFileTimestamp;		}
	const	std::string &	databaseJson()			const { return _databaseJson;			}
			bool			writeBatchedToDB()		const { return _writeBatchedToDBDepth;		}
			void			batchColumnHadChange(Column *col);

			void			dbCreate();
			void			dbUpdate();
			void			dbLoad(int index = -1, std::function<void(float)> progressCallback = [](float){}, Version doUpgradeFrom = Version());
			void			dbDelete();

			void			beginBatchedToDB();
			void			endBatchedToDB(std::function<void(float)> progressCallback = [](float){}, Columns columns={});
			void			endBatchedToDB(Columns columns) { endBatchedToDB([](float){}, columns); }
			
			void			removeColumn(	const	std::string &	name	);
			void			removeColumn(			size_t			index	);
			void			removeColumnById(		size_t			id		);
			void			insertColumn(			size_t			index,	bool alterDataSetTable = true);
			Column		*	newColumn(		const	std::string &	name);
			int				getColumnIndex(	const	std::string &	name	) const;
			int				columnIndex(	const	Column		*	col		) const;
			void			columnsReorder(			stringvec		order	); ///< Expects a sane order vector, with or without computed columns

			bool			allColumnsPassFilter()					const;

			size_t			getMaximumColumnWidthInCharacters(size_t columnIndex) const;
			stringvec		getColumnNames();
			colTypeMap		getColumnTypesMap();

			void			setDataFile( const std::string & dataFilePath, long timestamp)	{ _dataFilePath	= dataFilePath;	_dataFileTimestamp = timestamp; dbUpdate(); }
			void			setDatabaseJson(	const std::string & databaseJson)	{ _databaseJson		= databaseJson;			dbUpdate(); }
			void			setDataFileSynch(	bool synchronizing)					{ _dataFileSynch	= synchronizing;		dbUpdate(); }
			void			setShowRSyntax(		bool showRSyntax)					{ _showRSyntax		= showRSyntax;			dbUpdate(); }
			char			csvDelimiter()		const								{ return _csvDelimiter; }
			void			setCsvDelimiter(	char delimiter)						{ _csvDelimiter		= delimiter;			dbUpdate(); }

			void			setColumnCount(	size_t colCount);
			void			setRowCount(	size_t rowCount, bool alsoLoadData = true);

			void			incRevision() override;
			bool			checkForUpdates(stringvec * colsChanged = nullptr, stringvec * colsRemoved = nullptr, bool * newColumns = nullptr, bool * rowCountChanged = nullptr);

			const Columns &	computedColumns() const;
			
			void			loadOldComputedColumnsJson(const Json::Value & json); ///< Should act the same as the old ComputedColumns::fromJson() to allow loading "older jaspfiles"
			stringset		findUsedColumnNames(std::string searchThis);

			DatabaseInterface	 &	db();
	const	DatabaseInterface	 &	db() const;
	
			DataSetBaseNode		 *	dataNode()		const { return _dataNode; }
			DataSetBaseNode		 *	filtersNode()	const { return _filtersNode; }

			void					setEmptyValuesJson(			const Json::Value & emptyValues, bool updateDB = true);
			
	const	stringset			&	workspaceEmptyValues()															const	{ return _emptyValues->emptyStrings();								}
			void					setWorkspaceEmptyValues(	const stringset& values);
	const	std::string			&	description()																	const	{ return _description; }
			void					setDescription(				const std::string& desc);
			Json::Value				jsonForCompare() const;
			
private:
			void					upgradeEmptyValsFrom018To019(const Json::Value & emptyVals);
			void					setEmptyValuesJsonOldStuff(	const Json::Value & emptyValues);
			
			
private:
	DataSetBaseNode			*	_dataNode				= nullptr, //To make sure we have a pointer to flesh out the node hierarchy we add a "data" node, so we can place it next to the "filters" node in the tree
							*	_filtersNode			= nullptr;
	Columns						_columns;
	Filter					*	_filter					= nullptr;
	EmptyValues				*	_emptyValues			= nullptr;
	int							_dataSetID				= -1,
								_rowCount				= -1,
								_writeBatchedToDBDepth	= 0;
	ColumnSet					_changedDuringBatch		= {};
	long						_dataFileTimestamp		= 0;
	std::string					_dataFilePath,
								_databaseJson;
	
	bool						_dataFileSynch			= false,
								_showRSyntax			= false;
	char						_csvDelimiter			= '\0';
	static stringset			_defaultEmptyvalues;	// Default empty values if workspace do not have its own empty values (used for backward compatibility)
	std::string					_description;
};

#endif // DATASET_H
