//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef FILEPACKAGE_H
#define FILEPACKAGE_H

#include <cstddef>
#include <QAbstractItemModel>
#include <QFileInfo>
#include <QUrl>
#include "common.h"
#include "version.h"
#include <map>
#include <json/json.h>
#include <QTimer>
#include "databaseinterface.h"
#include "dataset.h"
#include "datasetpackageenums.h"

class EngineSync;
class DataSetPackageSubNodeModel;

///
/// This class is meant as the single bottleneck between the main application and Qt and the data stored in sqlite.
/// To this end a clean separation has been attempted between any access of the data so that it can easily be controlled.
///
/// In order to have all that data available through this class a tree-model has been chosen here.
///
/// The top layer of this tree are a parIdxType::dataRoot, ::filterRoot and several ::labelRoot "nodes" (one per column) stored in _internalPointers (together with column, just for label*)
/// Each of these is used a the rootnode for a DataSetPackageSubNodeModel, which basically drops everything not belonging to that rootnode.
/// And then makes sure it looks like a table to whatever model connects to that (like DataSetTableProxy which can then be used for filtering and sorting)
/// The _internalPointers structure also contains a node for parIdxType::data and parIdxType::filter and several for parIdxType::label (again one per column)
/// _internalPointers is formed by pairs of the nodetype and a column, the column is only used for the label and labelRoot nodes though.
/// by storing a reference to one of these elements in QModelIndices for DataSetPackage the correct parent can be found and proper filtering can be achieved.
///
/// For the filter it can, now because we only support a single filter, return a list of booleans. But this might be expanded later on.
/// The label is a special case and some support has been hacked in to also specify which particular column is referenced. 
/// Those labels are returned as columns: [filter-value, value-value, label-text]
///
/// These subnodes (at dataSubModel, filterSubModel and labelSubModel) are then used by proxy-classes like DataSetTable(Proxy)Model to make them easily available to QML.
///
/// It can use cleaning up though.
class DataSetPackage : public QAbstractItemModel //Not QAbstractTableModel because of: https://stackoverflow.com/a/38999940 (And this being a tree model)
{
	Q_OBJECT
	Q_PROPERTY(int			columnsFilteredCount	READ columnsFilteredCount								NOTIFY columnsFilteredCountChanged	)
	Q_PROPERTY(QString		name					READ name												NOTIFY nameChanged					)
	Q_PROPERTY(QString		folder					READ folder					WRITE setFolder				NOTIFY folderChanged				)
	Q_PROPERTY(QString		windowTitle				READ windowTitle										NOTIFY windowTitleChanged			)
	Q_PROPERTY(bool			modified				READ isModified				WRITE setModified			NOTIFY isModifiedChanged			)
	Q_PROPERTY(bool			loaded					READ isLoaded				WRITE setLoaded				NOTIFY loadedChanged				)
	Q_PROPERTY(QString		currentFile				READ currentFile			WRITE setCurrentFile		NOTIFY currentFileChanged			)
	Q_PROPERTY(bool			dataMode				READ dataMode											NOTIFY dataModeChanged				)
	Q_PROPERTY(bool			synchingExternally		READ synchingExternally		WRITE setSynchingExternally NOTIFY synchingExternallyChanged	)

	typedef DataSetPackageSubNodeModel							SubNodeModel;

public:
	typedef dataPkgRoles specialRoles;

	static DataSetPackage *	pkg() { return _singleton; }

							DataSetPackage(QObject * parent);
							~DataSetPackage();
		static Filter	*	filter();
		DataSet			*	dataSet() { return _dataSet; }
		void				setEngineSync(EngineSync * engineSync);
		void				reset(bool newDataSet = true);
		void				resizeData(size_t rowCount, size_t columnCount); //Should do reset and such things unlike setDataSetSize
		void				setDataSetSize(size_t columnCount, size_t rowCount);
		void				setDataSetColumnCount(size_t columnCount)			{ setDataSetSize(columnCount,			dataRowCount()); }
		void				setDataSetRowCount(size_t rowCount)					{ setDataSetSize(dataColumnCount(),		rowCount); }
		void				increaseDataSetColCount(size_t rowCount)			{ setDataSetSize(dataColumnCount() + 1,	rowCount); }

		void				createDataSet();	///< Creates *OR* recreates a dataset in database
        void                loadDataSet(std::function<void(float)> progressCallback = [](float){});      ///< Assumes internal.sqlite has just been loaded from a JASPFile and will init DataSet etc with it.
		void				deleteDataSet();	///< Deletes dataset from memory but not from database
		bool				hasDataSet() { return _dataSet; }

		void				pauseEngines();
		void				resumeEngines();
		void				enginesPrepareForData();
		void				enginesReceiveNewData();
		bool				enginesInitializing()	{ return emit enginesInitializingSignal();	}

		SubNodeModel	*	dataSubModel	() { return _dataSubModel;	 }
		SubNodeModel	*	filterSubModel	() { return _filterSubModel; }
		SubNodeModel	*	labelsSubModel	() { return _labelsSubModel; }
		
		DataSetBaseNode *	indexPointerToNode(			const QModelIndex	& index	) const;
		bool				dataSetBaseNodeStillExists(	DataSetBaseNode		* node	) const;
		
		void				waitForExportResultsReady();
		


		void				beginLoadingData(	bool informEngines = true);
		void				endLoadingData(		bool informEngines = true);
		void				beginSynchingData(	bool informEngines = true);
		void				endSynchingDataChangedColumns(stringvec	&	changedColumns,		bool hasNewColumns = false, bool informEngines = true);
		void				endSynchingData(stringvec				&	changedColumns,
											stringvec				&	missingColumns,
											std::map<std::string, std::string>		&	changeNameColumns,  //origname -> newname
											bool										rowCountChanged,
											bool										hasNewColumns,		bool informEngines = true);

		
		

		QHash<int, QByteArray>		roleNames()																						const	override;
				int					rowCount(		const QModelIndex &parent = QModelIndex())										const	override;
				int					columnCount(	const QModelIndex &parent = QModelIndex())										const	override;
				QVariant			data(			const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
				QVariant			headerData(		int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const	override;
				bool				setData(		const QModelIndex &index, const QVariant &value, int role)								override;
				Qt::ItemFlags		flags(			const QModelIndex &index)														const	override;
				QModelIndex			parent(			const QModelIndex & index)														const	override;
				QModelIndex			index(			int row,		int column, const QModelIndex & parent = QModelIndex())			const	override;
				bool				insertRows(		int row,		int count, const QModelIndex & aparent = QModelIndex())					override;
				bool				insertColumns(	int column,		int count, const QModelIndex & aparent = QModelIndex())					override;
				bool				removeRows(		int row,		int count, const QModelIndex & aparent = QModelIndex())					override;
				bool				removeColumns(	int column,		int count, const QModelIndex & aparent = QModelIndex())					override;
				QString				insertColumnSpecial(int column, bool computed, bool R);
				QString				appendColumnSpecial(			bool computed, bool R);

				QModelIndex			indexForSubNode(DataSetBaseNode * node)														const;
				int					filteredRowCount()																			const { return _dataSet->filter()->filteredRowCount(); }
				QVariant			getDataSetViewLines(bool up=false, bool left=false, bool down=true, bool right=true)		const;

				int					dataRowCount()		const;
				int					dataColumnCount()	const;


				std::string			id()								const	{ return _id;							}
				QString				name()								const;
				QString				folder()							const	{ return _folder;						}
				bool				dataMode()							const;
				
				bool				isReady()							const	{ return _analysesHTMLReady;			}
				bool				isLoaded()							const	{ return _isLoaded;						 }
				bool				isArchive()							const	{ return _isArchive;					  }
				bool				isJaspFile()						const	{ return _isArchive;					  } ///< for readability
				bool				isModified()						const	{ return _isModified;					   }
				std::string			initialMD5()						const	{ return _initialMD5;						 }
				QString				windowTitle()						const;
				QString				currentFile()						const	{ return _currentFile;						 }
				bool				hasAnalyses()						const	{ return _analysesData.size() > 0;			  }
				bool				synchingData()						const	{ return _synchingData;						  }
				std::string			dataFilePath()						const	{ return _dataSet ? _dataSet->dataFilePath() : "";  }
				bool				isDatabase()						const	{ return _database != Json::nullValue;			}
		const	Json::Value		&	databaseJson()						const	{ return _database;								}
		const	QString			&	analysesHTML()						const	{ return _analysesHTML;							}
		const	Json::Value		&	analysesData()						const	{ return _analysesData;							 }
		const	std::string		&	warningMessage()					const	{ return _warningMessage;						  }
		const	Version			&	archiveVersion()					const	{ return _archiveVersion;						   }
				Json::Value			emptyValuesJson()					const	{ return _dataSet->emptyValuesJson(); 				}

				bool				dataFileReadOnly()					const	{ return _dataFileReadOnly;						     }
				uint				dataFileTimestamp()					const	{ return _dataFileTimestamp;					      }
				bool				isDatabaseSynching()				const	{ return _databaseIntervalSyncher.isActive();	}
				bool				filterShouldRunInit()				const	{ return _filterShouldRunInit;							}


				void				setFilterShouldRunInit(bool shouldIt)				{ _filterShouldRunInit			= shouldIt;			}
				void				setAnalysesData(const Json::Value & analysesData)	{ _analysesData					= analysesData;		}
				void				setArchiveVersion(Version archiveVersion)			{ _archiveVersion				= archiveVersion;	}
				void				setEmptyValues(Json::Value & emptyValues);
				void				setWarningMessage(std::string message)				{ _warningMessage				= message;			}
				void				setDataFilePath(std::string filePath);
				void				setDatabaseJson(const Json::Value & dbInfo);
				void				setInitialMD5(std::string initialMD5)				{ _initialMD5					= initialMD5;		}
				void				setDataFileTimestamp(uint timestamp)				{ _dataFileTimestamp			= timestamp;		}
				void				setDataFileReadOnly(bool readOnly)					{ _dataFileReadOnly				= readOnly;			}
				void				setAnalysesHTML(const QString & html)				{ _analysesHTML					= html;				}
				void				setIsArchive(bool isArchive)						{ _isArchive					= isArchive;		}
				void				setHasAnalysesWithoutData()							{ _hasAnalysesWithoutData		= true;				}
				void				setModified(bool value);
				void				setAnalysesHTMLReady()								{ _analysesHTMLReady			= true;				}
				void				setId(std::string id)								{ _id							= id;				}
				void				setWaitingForReady()								{ _analysesHTMLReady			= false;			}
				void				setLoaded(bool loaded = true);

				bool						initColumnAsScale(				size_t				colNo,		const std::string & newName, const doublevec	& values);
				bool						initColumnAsScale(				const std::string & colName,	const std::string & newName, const doublevec	& values)	{ return initColumnAsScale(_dataSet->getColumnIndex(colName), newName, values); }
				bool						initColumnAsScale(				QVariant			colID,		const std::string & newName, const doublevec	& values);
				bool						initColumnAsNominalOrOrdinal(	size_t				colNo,		const std::string & newName, const intvec		& values,	const intstrmap &uniqueValues,	bool is_ordinal = false);
				bool						initColumnAsNominalOrOrdinal(	const std::string & colName,	const std::string & newName, const intvec		& values,	const intstrmap &uniqueValues,	bool is_ordinal = false) { return initColumnAsNominalOrOrdinal(_dataSet->getColumnIndex(colName), newName, values, uniqueValues, is_ordinal); }
				bool						initColumnAsNominalOrOrdinal(	QVariant			colID,		const std::string & newName, const intvec		& values,	const intstrmap &uniqueValues,	bool is_ordinal = false);
				bool						initColumnAsNominalOrOrdinal(	size_t				colNo,		const std::string & newName, const intvec		& values,									bool is_ordinal = false);
				bool						initColumnAsNominalOrOrdinal(	const std::string & colName,	const std::string & newName, const intvec		& values,									bool is_ordinal = false) { return initColumnAsNominalOrOrdinal(_dataSet->getColumnIndex(colName), newName, values, is_ordinal); }
				bool						initColumnAsNominalOrOrdinal(	QVariant			colID,		const std::string & newName, const intvec		& values,									bool is_ordinal = false);
				intstrmap					initColumnAsNominalText(		size_t				colNo,		const std::string & newName, const stringvec	& values,	const strstrmap & labels = strstrmap());
				intstrmap					initColumnAsNominalText(		const std::string & colName,	const std::string & newName, const stringvec	& values,	const strstrmap & labels = strstrmap())	{ return initColumnAsNominalText(_dataSet->getColumnIndex(colName), newName, values, labels); }
				intstrmap					initColumnAsNominalText(		QVariant			colID,		const std::string & newName, const stringvec	& values,	const strstrmap & labels = strstrmap());
				void						initColumnWithStrings(			QVariant			colId,		const std::string & newName, const stringvec	& values);
				void						initializeComputedColumns();
				
				void						pasteSpreadsheet(size_t row, size_t column, const std::vector<std::vector<QString>> & cells, QStringList newColNames = QStringList());

				void						storeInEmptyValues(		const std::string	& columnName, const intstrmap & emptyValues);

				void						columnSetDefaultValues(	const std::string	& columnName, columnType colType = columnType::unknown, bool emitSignals = true);
				Column *					createColumn(			const std::string	& name,		columnType colType);
				Column *					createComputedColumn(	const std::string	& name,		columnType type, computedColumnType desiredType, Analysis * analysis = nullptr);
				void						renameColumn(			const std::string	& oldColumnName, const std::string & newColumnName);
				void						removeColumn(			const std::string	& name);
				bool						columnExists(			Column				* column);

				stringvec					getColumnNames();
				bool						isColumnDifferentFromStringValues(const std::string & columnName, const stringvec & strVals);
				int							findIndexByName(const std::string & name)	const;

				bool						getRowFilter(				int						row)		const;
				QVariant					getColumnTypesWithIcons()										const;
				std::string					getComputedColumnError(		size_t					colIndex)	const;

				bool						isColumnUsedInEasyFilter(	const std::string	&	name)		const;
				bool						isColumnNameFree(			const std::string	&	name)		const;
				bool						isColumnNameFree(			const QString		&	name)		const	{ return isColumnNameFree(name.toStdString()); }
				bool						isColumnComputed(			size_t					colIndex)	const;
				bool						isColumnComputed(			const std::string	&	name)		const;
				bool						isColumnInvalidated(		size_t					colIndex)	const;

				int							setColumnTypeFromQML(	int columnIndex, int		newColumnType);
				bool						setColumnType(			int columnIndex, columnType newColumnType, bool emitHeaderChanged = true);

				int							columnsFilteredCount();

				std::string					getColumnTypeNameForJASPFile(columnType columnType);
				void						writeDataSetToOStream(std::ostream & out, bool includeComputed);
				columnType					parseColumnTypeForJASPFile(const std::string & name);
				Json::Value					columnToJsonForJASPFile(size_t columnIndex, Json::Value & labelsData, size_t & dataSize);
				void						columnLabelsFromJsonForJASPFile(Json::Value xData, Json::Value columnDesc, size_t columnIndex, std::map<std::string, intintmap > & mapNominalTextValues);

				int							getColumnIndex(			const std::string	& name)			const	{ return !_dataSet ? -1 : _dataSet->getColumnIndex(name); }
				int							getColumnIndex(			const QString		& name)			const	{ return getColumnIndex(name.toStdString()); }
				enum columnType				getColumnType(			size_t columnIndex)					const;
				std::string					getColumnName(			size_t columnIndex)					const;
				intvec						getColumnDataInts(		size_t columnIndex);
				doublevec					getColumnDataDbls(		size_t columnIndex);
				stringvec					getColumnDataStrs(		size_t columnIndex);
				void						setColumnName(			size_t columnIndex, const std::string	& newName,			bool resetModel = true);
				void						setColumnTitle(			size_t columnIndex, const std::string	& newTitle,			bool resetModel = true);
				void						setColumnDescription(	size_t columnIndex, const std::string	& newDescription,	bool resetModel = true);
				void						setColumnDataInts(		size_t columnIndex, const intvec		& ints);
				void						setColumnDataDbls(		size_t columnIndex, const doublevec		& dbls);
				size_t						getMaximumColumnWidthInCharacters(int columnIndex)			const;
				QStringList					getColumnLabelsAsStringList(const std::string & columnName,	bool dropUnused = false)	const;
				QStringList					getColumnLabelsAsStringList(size_t columnIndex,				bool dropUnused = false)	const;
				QStringList					getColumnValuesAsStringList(size_t columnIndex)				const;
				QList<QVariant>				getColumnValuesAsDoubleList(size_t columnIndex)				const;
				void						unicifyColumnNames();

				bool						setFilterData(const std::string & filter, const boolvec & filterResult);
				void						resetFilterAllows(size_t columnIndex);
				int							filteredOut(size_t column)									const;
				void						resetAllFilters();

				void						labelMoveRows(size_t column, std::vector<size_t> rows, bool up);
				void						labelReverse(size_t column);

				std::vector<bool>			filterVector();
				void						setFilterVectorWithoutModelUpdate(std::vector<bool> newFilterVector) { if(_dataSet) _dataSet->filter()->setFilterVector(newFilterVector); }


				void						databaseStartSynching(bool syncImmediately);
				void						databaseStopSynching();
				bool						synchingExternally() const;
				void						checkComputedColumnDependenciesForAnalysis(Analysis * analysis);
				std::string					freeNewColumnName(size_t startHere);
				void						dbDelete();


signals:
				void				datasetChanged(	QStringList				changedColumns,
													QStringList				missingColumns,
													QMap<QString, QString>	changeNameColumns,
													bool					rowCountChanged,
													bool					hasNewColumns);
				void				columnsFilteredCountChanged();
				void				badDataEntered(const QModelIndex index);
				void				allFiltersReset();
				void				labelFilterChanged();
				void				labelChanged(			QString columnName, QString originalLabel, QString newLabel);
				void				dataSetChanged();
				void				columnDataTypeChanged(	QString columnName);
				void				labelsReordered(		QString columnName);
				void				isModifiedChanged();
				void				enginesPrepareForDataSignal();
				void				enginesReceiveNewDataSignal();
				bool				enginesInitializingSignal();
				void				filteredOutChanged(int column);
				bool				checkDoSync();
				void				modelInit();
				void				nameChanged();
				void				folderChanged();
				void				windowTitleChanged();
				void				loadedChanged();
				void				currentFileChanged();
				void				synchingIntervalPassed();
				void				newDataLoaded();
				void				dataModeChanged(bool dataMode);
				void				synchingExternallyChanged();
				void				askUserForExternalDataFile();
				void				checkForDependentColumnsToBeSent(QString columnName);
				void				showWarning(QString title, QString msg);


public slots:
				void				refresh()																		{ beginResetModel(); endResetModel(); }
				void				refreshColumn(						QString columnName);
				void				columnWasOverwritten(				const std::string & columnName, const std::string & possibleError);
				void				notifyColumnFilterStatusChanged(	int columnIndex);
				void				setColumnsUsedInEasyFilter(			stringset usedColumns);
				void				emptyValuesChangedHandler();
				void				setCurrentFile(						QString currentFile);
				void				setFolder(							QString folder);
				void				generateEmptyData();
				void				logDataModeChanged(					bool dataMode);
				void				setSynchingExternally(				bool synchingExternally);
				Column			 *	requestComputedColumnCreation(		const std::string & columnName, Analysis * analysis);
				void				requestColumnCreation(				const std::string & columnName, Analysis * analysis, columnType type);
				void				requestComputedColumnDestruction(	const std::string & columnName);

private:
				bool				isThisTheSameThreadAsEngineSync();
				bool				setAllowFilterOnLabel(const QModelIndex & index, bool newAllowValue);
				bool				setDescriptionOnLabel(const QModelIndex & index, const QString & newDescription);
				QModelIndex			lastCurrentCell();
				void				resetModelOneCell();


private:
	static DataSetPackage	*	_singleton;
	DatabaseInterface		*	_db							= nullptr;
	DataSet					*	_dataSet					= nullptr;
	EngineSync				*	_engineSync					= nullptr;

	QString						_currentFile,
								_folder,
								_analysesHTML;
	std::string					_id,
								_warningMessage,
								_initialMD5;

	bool						_isArchive					= false,
								_dataFileReadOnly,
								_isModified					= false,
								_isLoaded					= false,
								_hasAnalysesWithoutData		= false,
								_analysesHTMLReady			= false,
								_filterShouldRunInit		= false,
								_dataMode					= false;

	Json::Value					_analysesData,
								_database					= Json::nullValue;
	Version						_archiveVersion;

	uint						_dataFileTimestamp;

	bool						_synchingData				= false;
	std::map<std::string, bool> _columnNameUsedInEasyFilter;

	SubNodeModel			*	_dataSubModel,
							*	_filterSubModel,
							*	_labelsSubModel;
	
	QTimer						_databaseIntervalSyncher;

};

#endif // FILEPACKAGE_H
