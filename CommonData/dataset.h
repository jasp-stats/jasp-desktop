#ifndef DATASET_H
#define DATASET_H

#include "datasetbasenode.h"
#include "column.h"
#include "filter.h"
#include "emptyvalues.h"

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
			Json::Value		emptyValuesJson()		const { return _emptyValues.toJson();	}
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

			void			setEmptyValuesJson(	const Json::Value & emptyValues)	{ _emptyValues.fromJson(emptyValues);		dbUpdate(); }
			void			setDataFilePath(	const std::string & dataFilePath)	{ _dataFilePath		= dataFilePath;			dbUpdate(); }
			void			setDatabaseJson(	const std::string & databaseJson)	{ _databaseJson		= databaseJson;			dbUpdate(); }
			void			setDataFileSynch(	bool synchronizing)					{ _dataFileSynch	= synchronizing;		dbUpdate(); }

			void			setColumnCount(	size_t colCount);
			void			setRowCount(	size_t rowCount);

			void			incRevision() override;
			bool			checkForUpdates();

			const Columns &	computedColumns() const;
			
			void			loadOldComputedColumnsJson(const Json::Value & json); ///< Should act the same as the old ComputedColumns::fromJson() to allow loading "older jaspfiles"
			stringset		findUsedColumnNames(std::string searchThis);

			void								storeInEmptyValues(const std::string & columnName, const intstrmap & emptyValues) { _emptyValues.storeInEmptyValues(columnName, emptyValues); }
			std::map<std::string, intstrmap >	resetEmptyValues();
	
			DatabaseInterface	 &	db();
	const	DatabaseInterface	 &	db() const;
	
			DataSetBaseNode		 *	dataNode()		const { return _dataNode; }
			DataSetBaseNode		 *	filtersNode()	const { return _filtersNode; }

	
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

};

#endif // DATASET_H
