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
#ifndef DATASET_H
#define DATASET_H

#include "databaseconnectioninfo.h"
#include "datasetbasenode.h"
#include "emptyvalues.h"
#include "column.h"
#include "filter.h"
#include "version.h"
#include "columnencoder.h"
#include "datasetsyncer.h"
#include "qutils.h"

class Workspace;
class UndoStack;

class DataSet : public DataSetBaseNode
{
	Q_OBJECT
	
	//Would be nice to have EmptyValuesQ also and make it available as a property here
	Q_PROPERTY(QString				description					READ descriptionQ				WRITE setDescriptionQ			NOTIFY descriptionChanged				)
	Q_PROPERTY(QString				dataFile					READ dataFileQ					WRITE setDataFileQ				NOTIFY dataFileChanged					)
	//Q_PROPERTY(QJsonValue			databaseJson				READ databaseJsonQ				WRITE setDatabaseJsonQ			NOTIFY databaseJsonChanged				)
	Q_PROPERTY(bool					dataFileSynch				READ dataFileSynch				WRITE setDataFileSynch			NOTIFY dataFileSynchChanged				)
	Q_PROPERTY(long					dataFileTimestamp			READ dataFileTimestamp			WRITE setDataTimestamp			NOTIFY dataTimestampChanged				)
	Q_PROPERTY(int					columnsLabelFilteredCount	READ columnsLabelFilteredCount									NOTIFY columnsLabelFilteredCountChanged	)
	Q_PROPERTY(Filter	*			shownFilter					READ shownFilter												NOTIFY shownFilterChanged				)
	Q_PROPERTY(Column	*			shownColumn					READ shownColumn				WRITE setShownColumn			NOTIFY shownColumnChanged				)
	Q_PROPERTY(QString				name						READ name														CONSTANT								)
	Q_PROPERTY(QString				title						READ title						WRITE setTitle					NOTIFY titleChanged						)
	Q_PROPERTY(QString				rCode						READ rCodeQ						WRITE setRCodeQ					NOTIFY rCodeChanged						)
	Q_PROPERTY(computedColumnType	codeType					READ codeType					WRITE setCodeType				NOTIFY codeTypeChanged					)
	Q_PROPERTY(bool					invalidated					READ invalidated												NOTIFY invalidatedChanged				)
	Q_PROPERTY(QString				error						READ errorQ						WRITE setErrorQ					NOTIFY errorChanged						)
	Q_PROPERTY(int					defaultInputFilterId		READ defaultInputFilterId		WRITE setDefaultInputFilterId	NOTIFY defaultInputFilterChanged		)
	// Emit signals also in refresh
	
	friend Column;
	
public:
	typedef 	std::map<std::string,columnType>	colTypeMap;
	typedef		DatabaseInterface					DBIF;
	typedef		DatabaseConnectionInfo				DBCIF;
	
							DataSet(Workspace * workspace, int index = -1); ///< index==-1: create a new dataSet, >0: load that dataSet, 0: do nothing
							~DataSet();
	
			Workspace	*	workspace()			const		{ return	_workspace; }
			Filter		*	defaultFilter()		const 		{ return	_defaultFilter;	}
			Filter		*	shownFilter()		const 		{ return	!_shownFilter ? _defaultFilter : _shownFilter;	}
	const	Filters		&	filters()			const		{ return	_filters;		}
			void			deleteShownFilter();
			void			addFilter();
			void			showFilter(Filter * filter);
			Filter *		showFilter(const std::string & filterName);
			Filter *		showFilter(const QString & filterName);
			Columns		&	columns()			const		{ return	const_cast<Columns&>(_columns);	}
    const	EmptyValues *	emptyValues()       const		{ return	_emptyValues; }
			EmptyValues *	emptyValues()					{ return	_emptyValues; }
			QString			name()				const;
			QString			title()				const;

			Column		*	column(		const std::string & name);
			Column		*	column(		const char * name) { return column(std::string(name));}
			Column		*	column(		const QString & name);
			Column		*	column(		int					columnIndex);

			Column		*	operator[](	size_t				columnIndex)	{ return column(columnIndex); }
			Column		*	operator[](	const std::string &	columnName)		{ return column(columnName); }
	
			int				id()					const { return _dataSetId;				}
			bool			dataFileSynch()			const { return _dataFileSynch;			}
			
	const	std::string &	dataFilePath()			const { return _dataFilePath;			}
			bool			dataFileCanHaveLabels() const;
			qint64			dataFileTimestamp()		const { return _dataFileTimestamp;		}
	const	Json::Value &	databaseJson()			const { return _database;				}
			bool			writeBatchedToDB()		const { return _writeBatchedToDBDepth;		}
			bool			filterExists(const std::string & name) { return filter(name); }
			Filter *		filter(const std::string & name);
			Filter *		filter(int id);
			void			batchColumnHadChange(Column *col);
			
			int				rowCount(		const QModelIndex &parent = QModelIndex())										const	override;
			int				columnCount(	const QModelIndex &parent = QModelIndex())										const	override;
			QVariant		data(			const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
			bool			setData(		const QModelIndex &index, const QVariant &value, int role)								override;
			QVariant		headerData(		int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const	override;
			Qt::ItemFlags	flags(			const QModelIndex &index)														const	override;
			
			bool			insertRows(		int row,		int count, const QModelIndex & aparent = QModelIndex())					override;
			bool			insertColumns(	int column,		int count, const QModelIndex & aparent = QModelIndex())					override;
			bool			removeRows(		int row,		int count, const QModelIndex & aparent = QModelIndex())					override;
			bool			removeColumns(	int column,		int count, const QModelIndex & aparent = QModelIndex())					override;

			int				columnsLabelFilteredCount()	const;

			void			dbCreate();
			void			dbUpdate();
			void			dbLoad(int id = -1, std::function<void(float)> progressCallback = [](float){}, Version doUpgradeFrom = Version());
			void			dbDelete();

			void			beginBatchedToDB();
			void			endBatchedToDB(std::function<void(float)> progressCallback = [](float){}, Columns columns={});
			void			endBatchedToDB(Columns columns) { endBatchedToDB([](float){}, columns); }
			
			void			removeColumn(	const	std::string &	name	);
			void			removeColumn(			size_t			index	);
			void			removeColumnById(		size_t			id		);
			void			insertColumns(			size_t			index,	size_t count, bool alterDataSetTable = true);
			void			insertColumn(			size_t			index,	bool alterDataSetTable = true);
			Column		*	createColumn(			const std::string &	name, columnType columnType = columnType::unknown);
			Column		*	createComputedColumn(	const std::string & name, columnType type		= columnType::unknown, computedColumnType desiredType = computedColumnType::analysis, int analysisId = -1);
			ColumnEncoder	&	encoder()			{ return *_encoder; }
	const	ColumnEncoder	&	encoder()	const	{ return *_encoder; }
			DataSetSyncer	&	syncer()			{ return *_syncer; }
	const	DataSetSyncer	&	syncer()	const	{ return *_syncer; }
			int				getColumnIndex(	const	std::string &	name	) const;
			int				columnIndex(	const	Column		*	col		) const;
			void			columnsReorder(			stringvec		order	); ///< Expects a sane order vector, with or without computed columns
			void			columnRefreshed(Column * column);
			void			columnsSetAutoSortForColumns(std::map<std::string,bool> sortPerColumn);
			void			columnsReverseValues(stringset columnIndexes);
			void			invalidateAllComputedColumns();

			bool			allColumnsPassFilter()					const;
			
			std::string		freeNewColumnName(size_t startHere)																const;
			bool			isColumnNameFree(const std::string & name)														const;
			
			QString			dataFileQ()			const;
			QString			descriptionQ()		const;
			long			dataTimestamp()		const;
			bool			isDatabase()						const	{ return _database != Json::nullValue;				}
			
			Column		*	shownColumn() const;
			void			setShownColumn(Column *newShownColumn);
			
			
			void			setTitle(				const QString & title);
			void			setDataFileQ(			const QString &	newDataFile);
			void			setDescriptionQ(		const QString &	newDescription);
			//void			setDatabaseJsonQ(		const QString &	newDatabaseJson);
			void			setDataFileAndTimeStamp(const std::string &dataFilePath, long timestamp);
			
			void			resetAllFilters();
			void			resetFilterCounters();
			void			resetVariableTypes(int thresholdScale);
			

			size_t			getMaximumColumnWidthInCharacters(size_t columnIndex) const;
			stringvec		getColumnNames();
			colTypeMap		getColumnTypesMap();
			void			setupEncoderPrefix();
			

			void			setDataFile(		const std::string & dataFilePath);
			void			setDataTimestamp(	long timestamp);
			void			setDatabaseJson(	const Json::Value & databaseJson);
			void			setDataFileSynch(	bool synchronizing);
			bool			synchingData()		const { return _synchingDataNow; }
			void			startSynching(		bool synchImmediately = true);
			
			void			emitColumnChanged(		const QString		& name);

			void			setDataFile( const std::string & dataFilePath, long timestamp)	{ _dataFilePath	= dataFilePath;	_dataFileTimestamp = timestamp; dbUpdate(); }
			void			setDatabaseJson(	const std::string & databaseJson)	{ Json::Reader().parse(databaseJson, _database); dbUpdate(); }
			char			csvDelimiter()		const								{ return _csvDelimiter; }
			void			setCsvDelimiter(	char delimiter)						{ _csvDelimiter		= delimiter;			dbUpdate(); }

			void			setColumnCount(	size_t colCount);
			void			setRowCount(	size_t rowCount, bool alsoLoadData = true);

			void			incRevision() override;
			bool			checkForUpdates(std::function<void(float)> progressCallback = [](float){});
			void			runComputedColumn(QString columnName, QString code, enum columnType columnType);
			void			runComputedDataset(QString code, int defaultInputFilterId);

			Columns				computedColumns() const;

			//Computed-dataset state (a whole DataSet generated from R code), mirroring the per-column state.
			bool					isComputed()				const	{ return _codeType != computedColumnType::notComputed;									}
			bool					isComputedRCode()			const	{ return _codeType == computedColumnType::rCode;								}
			computedColumnType		codeType()					const	{ return _codeType;															}
			bool					invalidated()				const	{ return _invalidated;														}
			void					invalidate()						{ setInvalidated(true);														}
			void					validate()							{ setInvalidated(false);													}
			std::string				rCode()						const	{ return _rCode;															}
			QString					rCodeQ()					const	{ return tq(_rCode);														}
			std::string				rCodeStripped()				const;
			QString					errorQ()					const	{ return tq(_error);														}
			std::string				error()						const	{ return _error;															}
			int						defaultInputFilterId()		const	{ return _defaultInputFilterId;											}
			Filter				*	defaultInputFilter()		const;
			DataSet				*	defaultInputDataSet()		const;
			bool					setRCode(				const std::string	& rCode);
			bool					setRCodeQ(				const QString		& rCode)	{ return setRCode(fq(rCode));						}
			void					setCodeType(			computedColumnType codeType);
			void					setInvalidated(			bool invalidated);
			bool					setError(				const std::string	& error);
			bool					setErrorQ(				const QString		& error)				{ return setError(fq(error));			}
			bool					setDefaultInputFilterId(int defaultInputFilterId);
			bool					tryAndRunComputedDataset();
			bool					iShouldBeSentAgain();
			void					checkForDependentDatasetsToBeSent(bool refreshMe = false);
			void					dbUpdateComputedDatasetStuff();
			
			void			loadOldComputedColumnsJson(const Json::Value & json); ///< Should act the same as the old ComputedColumns::fromJson() to allow loading "older jaspfiles"
			stringset		findUsedColumnNames(std::string searchThis);

			DBIF		&	db();
	const	DBIF		&	db() const;
	
			void			setEmptyValuesJson(			const Json::Value & emptyValues, bool updateDB = true);
			
	const	std::string	&	description()																	const	{ return _description; }
	const	stringset	&	emptyValuesAsStrings()															const	{ return _emptyValues->emptyStrings();		}
			void			setEmptyValuesFromStrings(	const stringset& values);
			void			setDescription(				const std::string& desc);
			Json::Value		jsonForCompare() const;
			void			writeToOStream(std::ostream & out, bool includeComputed);

signals:
			void			manualEditMade(); 
			void			datasetChanged(				int						dataSetId,
															QStringList				changedColumns,
															QStringList				missingColumns,
															QMap<QString, QString>	changeNameColumns,
															bool					rowCountChanged,
															bool					hasNewColumns); 
			void			labelsReordered(			QString columnName);
			void			labelFilterChanged();
			
			void			allFiltersReset();
			void			showWarning(						QString title, QString msg);
			void			descriptionChanged();
			void			titleChanged();
			void			dataFileChanged();
			void			databaseJsonChanged();
			void			dataFileSynchChanged();
			void			dataTimestampChanged();
			void			columnsLabelFilteredCountChanged();
			void			refreshAllAnalyses(Filter * f);
			void			refreshAllCompCols(Filter * f);
			void			synchingIntervalPassed();
			void			columnTypeChanged(QString name);
			void			sendFilter(int dataSetID, const QString & generatedFilter, const QString & filter);
			void			sendFilterByName(int dataSetID, const QString & name, const QString & module = "*");
			void			filtersCountChanged();
			void			shownFilterChanged(DataSet * data);
			void			filterRemoved(Filter * f);
			void			syncRequired(DataSet * dataSet, QString locator, QString extension, QString databaseJson);
			void			labelChanged(		const Column * column, QString originalLabel, QString newLabel);
			QString			askPassword(	QString title, QString message);
			bool			showYesNo(		QString title, QString message);
			void			shownColumnChanged();
			void			emptyValuesChanged();
			void			rCodeChanged();
			void			codeTypeChanged();
			void			invalidatedChanged();
			void			errorChanged();
			void			defaultInputFilterChanged();
			
public slots:
			void			refresh(bool doColumnsToo = true);
			void			runFilters();
			void			handleColumnChanged(		const Column * column);
			void			handleLabelsReordered(		const Column * column);
			bool			setColumnTypes(stringset columnIndexes, columnType newColumnType);
			void			filterByNameDone(int dataSetID, const QString & name, const QString & error);
			

public:
			Filter		*	createFilter(const std::string & name, bool createIfMissing = true) { return new Filter(this, name, createIfMissing); }
			void			registerFilter(Filter * f);
			void			removeFilter(Filter * f);
			
private:
			void			upgradeEmptyValsFrom018To019(const Json::Value & emptyVals);
			void			setEmptyValuesJsonOldStuff(	const Json::Value & emptyValues);
			void			columnsApply(intset		columnIndxs, std::function<bool (Column *)>			applyThis);
			void			columnsApply(stringset	columnNames, std::function<bool (Column *)>			applyThis);
			void			columnsApply(intset		columnIndxs, std::function<bool (Column *, int)>	applyThis);
			void			columnsApply(stringset	columnNames, std::function<bool (Column *, int)>	applyThis);

			
private slots:
			void			handleDataSetChanged(	int						dataSetID,
													QStringList				changedColumns,
													QStringList				missingColumns,
													QMap<QString, QString>	changeNameColumns,
													bool					rowCountChanged,
													bool					hasNewColumns);

public:
	static QVariant			getDataSetViewLines(bool up, bool left, bool down, bool right)									;
			
	
	UndoStack			*	undoStack()				const	{ return _undoStack; }
	QString					insertColumnSpecial(int columnIndex, const QMap<QString, QVariant> &props);
	void					pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> &  labels, const intvec & coltypes = std::vector<int>(), const QStringList & colNames = {}, const std::vector<boolvec> & selected = {});
	
protected:
	bool					getRowFilter(int row)																			const;
	bool					getColumnInDragNDropShownFilter(int columnIndex)												const;
	bool					getColumnInDragNDropShownFilter(Column * column)												const;
		
private:
	Workspace			*	_workspace				= nullptr;
	Columns					_columns;
	Column				*	_shownColumn			= nullptr;
	ColumnEncoder		*	_encoder				= nullptr;
	DataSetSyncer		*	_syncer					= nullptr;
	Filter				*	_defaultFilter			= nullptr,
						*	_shownFilter			= nullptr;
	Filters					_filters;
	EmptyValues			*	_emptyValues			= nullptr;
	int						_dataSetId				= -1,
							_rowCount				= -1,
							_writeBatchedToDBDepth	= 0;
	ColumnSet				_changedDuringBatch		= {};
	long					_dataFileTimestamp		= 0;
	std::string				_dataFilePath,
							_title;
	bool					_dataFileSynch			= false,
							_synchingDataNow		= false;
	char					_csvDelimiter			= '\0';
	Json::Value				_database				= Json::nullValue;
	static stringset		_defaultEmptyvalues;	// Default empty values if workspace do not have its own empty values (used for backward compatibility)
	std::string				_description;
	UndoStack			*	_undoStack				= nullptr;
	computedColumnType		_codeType				= computedColumnType::notComputed;
	bool					_invalidated			= false;
	int						_defaultInputFilterId	= -1;
	std::string				_rCode,
							_error;
};

typedef std::vector<DataSet*> DataSets;

#endif // DATASET_H
