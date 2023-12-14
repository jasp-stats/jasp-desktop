#ifndef DATASET_H
#define DATASET_H

#include "datasetbasenode.h"
#include "column.h"
#include "filter.h"
#include "emptyvalues.h"
#include "version.h"

typedef std::vector<Column*> Columns;


class DataSet : public DataSetBaseNode
{
public:
							DataSet(int index = -1);
							~DataSet();
	
			Filter		*	filter()				{ return	_filter;	}
			Columns		&	columns()	const		{ return	const_cast<Columns&>(_columns);	}

			Column		*	column(		const std::string & name);
			Column		*	column(		size_t				columnIndex);

			Column		*	operator[](	size_t				columnIndex)	{ return column(columnIndex); }
			Column		*	operator[](	const std::string &	columnName)		{ return column(columnName); }
	
			int				id()					const { return _dataSetID;				}
			int				columnCount()			const ;
			int				rowCount()				const ;
			bool			dataFileSynch()			const { return _dataFileSynch;			}
	const	std::string &	dataFilePath()			const { return _dataFilePath;			}
	const	std::string &	databaseJson()			const { return _databaseJson;			}
			bool			writeBatchedToDB()		const { return _writeBatchedToDB;		}

			void			dbCreate();
			void			dbUpdate();
			void			dbLoad(int index = -1, std::function<void(float)> progressCallback = [](float){});
			void			dbDelete();

			void			beginBatchedToDB();
			void			endBatchedToDB(std::function<void(float)> progressCallback = [](float){});

			void			removeColumn(	const	std::string &	name	);
			void			removeColumn(			size_t			index	);
			void			removeColumnById(		size_t			id		);
			void			insertColumn(			size_t			index	);
			Column		*	newColumn(		const	std::string &	name);
			int				getColumnIndex(	const	std::string &	name	) const;
			int				columnIndex(	const	Column		*	col		) const;

			bool			allColumnsPassFilter()					const;

			size_t			getMaximumColumnWidthInCharacters(size_t columnIndex) const;
			stringvec		getColumnNames();

			void			setDataFilePath(	const std::string & dataFilePath)	{ _dataFilePath		= dataFilePath;			dbUpdate(); }
			void			setDatabaseJson(	const std::string & databaseJson)	{ _databaseJson		= databaseJson;			dbUpdate(); }
			void			setDataFileSynch(	bool synchronizing)					{ _dataFileSynch	= synchronizing;		dbUpdate(); }

			void			setColumnCount(	size_t colCount);
			void			setRowCount(	size_t rowCount);

			void			incRevision() override;
			bool			checkForUpdates(stringvec * colsChanged = nullptr);

			const Columns &	computedColumns() const;
			
			void			loadOldComputedColumnsJson(const Json::Value & json); ///< Should act the same as the old ComputedColumns::fromJson() to allow loading "older jaspfiles"
			stringset		findUsedColumnNames(std::string searchThis);

			DatabaseInterface	 &	db();
	const	DatabaseInterface	 &	db() const;
	
			DataSetBaseNode		 *	dataNode()		const { return _dataNode; }
			DataSetBaseNode		 *	filtersNode()	const { return _filtersNode; }

			std::map<std::string, intstrmap > resetMissingData(const std::vector<Column*>& columns);
			void					setMissingData(const std::string & columnName, const intstrmap & missingData)			{ _emptyValues.setMissingData(columnName, missingData);			dbUpdate();	}
			void					setEmptyValuesJson(			const Json::Value & emptyValues, bool updateDB = true);
	const	stringset			&	emptyValues(				const std::string& colName)							const	{ return _emptyValues.emptyValues(colName);									}
	const	doubleset			&	doubleEmptyValues(			const std::string& colName)							const	{ return _emptyValues.doubleEmptyValues(colName);							}
	const	intstrmap			&	missingData(				const std::string& colName)							const	{ return _emptyValues.missingData(colName);									}
	const	stringset			&	workspaceEmptyValues()															const	{ return _emptyValues.workspaceEmptyValues();								}
			void					setCustomEmptyValues(		const std::string& colName, const stringset& values)		{ _emptyValues.setCustomEmptyValues(colName, values);			dbUpdate();	}
			bool					hasCustomEmptyValues(		const std::string& colName)							const	{ return _emptyValues.hasCutomEmptyValues(colName);							}
			void					setHasCustomEmptyValues(	const std::string& colName, bool hasCustom)					{ _emptyValues.setHasCustomEmptyValues(colName, hasCustom);		dbUpdate();	}
			void					setWorkspaceEmptyValues(	const stringset& values);
	const	std::string			&	description()																	const	{ return _description; }
			void					setDescription(				const std::string& desc);

private:
	DataSetBaseNode			*	_dataNode				= nullptr, //To make sure we have a pointer to flesh out the node hierarchy we add a "data" node, so we can place it next to the "filters" node in the tree
							*	_filtersNode			= nullptr;
	Columns						_columns;
	Filter					*	_filter					= nullptr;
	int							_dataSetID				= -1,
								_rowCount				= -1;
	std::string					_dataFilePath,
								_databaseJson;
	EmptyValues					_emptyValues;
	bool						_writeBatchedToDB		= false,
								_dataFileSynch			= false;
	static stringset			_defaultEmptyvalues;	// Default empty values if workspace do not have its own empty values (used for backward compatibility)
	std::string					_description;
};

#endif // DATASET_H
