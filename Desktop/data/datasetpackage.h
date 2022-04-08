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
#include "dataset.h"
#include "version.h"
#include <map>
#include <json/json.h>
#include "computedcolumns.h"
#include "datasetdefinitions.h"
#include <QTimer>

class EngineSync;
class DataSetPackageSubNodeModel;

///
/// This class is meant as the single bottleneck between the main application and Qt and the data stored in shared memory.
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
	Q_PROPERTY(int			columnsFilteredCount	READ columnsFilteredCount							NOTIFY columnsFilteredCountChanged	)
	Q_PROPERTY(QString		name					READ name											NOTIFY nameChanged					)
	Q_PROPERTY(QString		folder					READ folder					WRITE setFolder			NOTIFY folderChanged				)
	Q_PROPERTY(QString		windowTitle				READ windowTitle									NOTIFY windowTitleChanged			)
	Q_PROPERTY(bool			modified				READ isModified				WRITE setModified		NOTIFY isModifiedChanged			)
	Q_PROPERTY(bool			loaded					READ isLoaded				WRITE setLoaded			NOTIFY loadedChanged				)
	Q_PROPERTY(QString		currentFile				READ currentFile			WRITE setCurrentFile	NOTIFY currentFileChanged			)

	typedef std::map<std::string, std::map<int, std::string>>	emptyValsType;
	typedef std::pair<parIdxType, int>							intnlPntPair; //first value is what kind of data the index is for and the int is for parIdxType::label only, to know which column is selected.
	typedef std::vector<intnlPntPair>							internalPointerType;
	typedef DataSetPackageSubNodeModel							SubNodeModel;

public:
	///Special roles for the different submodels of DataSetPackage. If both maxColString and columnWidthFallback are defined by a model DataSetView will only use maxColString. selected is now only used in LabelModel, but defined here for convenience.
	enum class	specialRoles { filter = Qt::UserRole, lines, maxColString, maxRowHeaderString, columnIsComputed, computedColumnIsInvalidated, labelsHasFilter, computedColumnError, value, columnType, columnWidthFallback, label, selected };

	static DataSetPackage *	pkg() { return _singleton; }

							DataSetPackage(QObject * parent);
							~DataSetPackage();
		void				setEngineSync(EngineSync * engineSync);
		void				reset();
		void				setDataSetSize(size_t columnCount, size_t rowCount);
		void				setDataSetColumnCount(size_t columnCount)			{ setDataSetSize(columnCount,			dataRowCount()); }
		void				setDataSetRowCount(size_t rowCount)					{ setDataSetSize(dataColumnCount(),		rowCount); }
		void				increaseDataSetColCount(size_t rowCount)			{ setDataSetSize(dataColumnCount() + 1,	rowCount); }

		void				createDataSet();
		void				freeDataSet();
		bool				hasDataSet() { return _dataSet; }

		void				pauseEngines();
		void				resumeEngines();
		bool				enginesInitializing()	{ return emit enginesInitializingSignal();	}

		SubNodeModel	*	dataSubModel	() { return _dataSubModel;	 }
		SubNodeModel	*	filterSubModel	() { return _filterSubModel; }
		SubNodeModel	*	labelsSubModel	() { return _labelsSubModel; }
		
		void				waitForExportResultsReady();
		
		void				beginLoadingData();
		void				endLoadingData();
		void				beginSynchingData();
		void				endSynchingDataChangedColumns(std::vector<std::string>	&	changedColumns);
		void				endSynchingData(std::vector<std::string>				&	changedColumns,
											std::vector<std::string>				&	missingColumns,
											std::map<std::string, std::string>		&	changeNameColumns,  //origname -> newname
											bool										rowCountChanged,
											bool										hasNewColumns);


		QHash<int, QByteArray>		roleNames()																					const	override;
				int					rowCount(	const QModelIndex &parent = QModelIndex())										const	override;
				int					columnCount(const QModelIndex &parent = QModelIndex())										const	override;
				QVariant			data(		const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
				QVariant			headerData(	int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const	override;
				bool				setData(	const QModelIndex &index, const QVariant &value, int role)								override;
				Qt::ItemFlags		flags(		const QModelIndex &index)														const	override;
				QModelIndex			parent(		const QModelIndex & index)														const	override;
				QModelIndex			index(int row, int column, const QModelIndex &parent)										const	override;
				parIdxType			parIdxTypeIs(const QModelIndex &index)														const;
		const	intnlPntPair *		getInternalPointerPairFromIndex(const QModelIndex & index)									const;
				void				regenerateInternalPointers();
				QModelIndex			parentModelForType(parIdxType type, int column = 0)											const;
				QModelIndex			rootModelIndexForType(parIdxType type, int column = 0)										const;
		static	parIdxType			parIdxTypeParentForChild(parIdxType type);
		static	parIdxType			parIdxTypeChildForParent(parIdxType type);
				int					filteredRowCount()																			const { return _dataSet ? _dataSet->filteredRowCount() : 0; }
				QVariant			getDataSetViewLines(bool up=false, bool left=false, bool down=true, bool right=true)		const;

				int					dataRowCount()		const { return rowCount(parentModelForType(parIdxType::data));		}
				int					dataColumnCount()	const { return columnCount(parentModelForType(parIdxType::data));	}

				void				storeInEmptyValues(std::string columnName, std::map<int, std::string> emptyValues)	{ _emptyValuesMap[columnName] = emptyValues;	}
				void				resetEmptyValues()																	{ _emptyValuesMap.clear();											}

				std::string			id()								const	{ return _id;							}
				QString				name()								const;
				QString				folder()							const	{ return _folder;						}
				bool				isReady()							const	{ return _analysesHTMLReady;			}
				bool				isLoaded()							const	{ return _isLoaded;						 }
				bool				isArchive()							const	{ return _isArchive;					  }
				bool				isModified()						const	{ return _isModified;					   }
				std::string			dataFilter()						const	{ return _dataFilter;						}
				std::string			initialMD5()						const	{ return _initialMD5;						 }
				QString				windowTitle()						const;
				QString				currentFile()						const	{ return _currentFile;						 }
				bool				hasAnalyses()						const	{ return _analysesData.size() > 0;			  }
				bool				synchingData()						const	{ return _synchingData;						  }
				std::string			dataFilePath()						const	{ return _dataFilePath;						   }
		const	Json::Value		&	databaseJson()						const	{ return _database;								}
		const	std::string		&	analysesHTML()						const	{ return _analysesHTML;							}
		const	Json::Value		&	analysesData()						const	{ return _analysesData;							 }
		const	std::string		&	warningMessage()					const	{ return _warningMessage;						  }
		const	Version			&	archiveVersion()					const	{ return _archiveVersion;						   }
		const	emptyValsType	&	emptyValuesMap()					const	{ return _emptyValuesMap;   						}

				bool				dataFileReadOnly()					const	{ return _dataFileReadOnly;						     }
				uint				dataFileTimestamp()					const	{ return _dataFileTimestamp;					      }
		const	Version			&	dataArchiveVersion()				const	{ return _dataArchiveVersion;						   }
				bool				filterShouldRunInit()				const	{ return _filterShouldRunInit;							}
		const	std::string		&	filterConstructorJson()				const	{ return _filterConstructorJSON;					    }


				void				setDataArchiveVersion(Version archiveVersion)		{ _dataArchiveVersion			= archiveVersion;	}
				void				setFilterShouldRunInit(bool shouldIt)				{ _filterShouldRunInit			= shouldIt;			}
				void				setFilterConstructorJson(std::string json)			{ _filterConstructorJSON		= json;				}
				void				setAnalysesData(const Json::Value & analysesData)	{ _analysesData					= analysesData;		}
				void				setArchiveVersion(Version archiveVersion)			{ _archiveVersion				= archiveVersion;	}
				void				setWarningMessage(std::string message)				{ _warningMessage				= message;			}
				void				setDataFilePath(std::string filePath)				{ _dataFilePath					= filePath;			}
				void				setDatabaseJson(const Json::Value & dbInfo)			{ _database						= dbInfo;			}
				void				setInitialMD5(std::string initialMD5)				{ _initialMD5					= initialMD5;		}
				void				setDataFileTimestamp(uint timestamp)				{ _dataFileTimestamp			= timestamp;		}
				void				setDataFileReadOnly(bool readOnly)					{ _dataFileReadOnly				= readOnly;			}
				void				setAnalysesHTML(std::string html)					{ _analysesHTML					= html;				}
				void				setDataFilter(std::string filter)					{ _dataFilter					= filter;			}
				void				setDataSet(DataSet * dataSet);
				void				setIsArchive(bool isArchive)						{ _isArchive					= isArchive;		}
				void				setHasAnalysesWithoutData()							{ _hasAnalysesWithoutData		= true;				}
				void				setModified(bool value);
				void				setAnalysesHTMLReady()								{ _analysesHTMLReady			= true;				}
				void				setId(std::string id)								{ _id							= id;				}
				void				setWaitingForReady()								{ _analysesHTMLReady			= false;			}
				void				setLoaded(bool loaded = true);

				bool						initColumnAsScale(				size_t colNo,			std::string newName, const std::vector<double>		& values);
				bool						initColumnAsScale(				std::string colName,	std::string newName, const std::vector<double>		& values)	{ return initColumnAsScale(_dataSet->getColumnIndex(colName), newName, values); }
				bool						initColumnAsScale(				QVariant colID,			std::string newName, const std::vector<double>		& values);

				bool						initColumnAsNominalOrOrdinal(	size_t colNo,			std::string newName, const std::vector<int>			& values,	const std::map<int, std::string> &uniqueValues,	bool is_ordinal = false);
				bool						initColumnAsNominalOrOrdinal(	std::string colName,	std::string newName, const std::vector<int>			& values,	const std::map<int, std::string> &uniqueValues,	bool is_ordinal = false) { return initColumnAsNominalOrOrdinal(_dataSet->getColumnIndex(colName), newName, values, uniqueValues, is_ordinal); }
				bool						initColumnAsNominalOrOrdinal(	QVariant colID,			std::string newName, const std::vector<int>			& values,	const std::map<int, std::string> &uniqueValues,	bool is_ordinal = false);

				bool						initColumnAsNominalOrOrdinal(	size_t colNo,			std::string newName, const std::vector<int>			& values,	bool is_ordinal = false);
				bool						initColumnAsNominalOrOrdinal(	std::string colName,	std::string newName, const std::vector<int>			& values,	bool is_ordinal = false) { return initColumnAsNominalOrOrdinal(_dataSet->getColumnIndex(colName), newName, values, is_ordinal); }
				bool						initColumnAsNominalOrOrdinal(	QVariant colID,			std::string newName, const std::vector<int>			& values,	bool is_ordinal = false);

				std::map<int, std::string>	initColumnAsNominalText(		size_t colNo,			std::string newName, const std::vector<std::string>	& values,	const std::map<std::string, std::string> & labels = std::map<std::string, std::string>());
				std::map<int, std::string>	initColumnAsNominalText(		std::string colName,	std::string newName, const std::vector<std::string>	& values,	const std::map<std::string, std::string> & labels = std::map<std::string, std::string>())	{ return initColumnAsNominalText(_dataSet->getColumnIndex(colName), newName, values, labels); }
				std::map<int, std::string>	initColumnAsNominalText(		QVariant colID,			std::string newName, const std::vector<std::string>	& values,	const std::map<std::string, std::string> & labels = std::map<std::string, std::string>());

				void						columnSetDefaultValues(std::string columnName, columnType colType = columnType::unknown);
				bool						createColumn(std::string name, columnType colType);
				void						renameColumn(std::string oldColumnName, std::string newColumnName);
				void						removeColumn(std::string name);

				std::vector<std::string>	getColumnNames(bool includeComputed = true);
				bool						isColumnDifferentFromStringValues(std::string columnName, std::vector<std::string> strVals);
				size_t						findIndexByName(std::string name)		const;

				bool						getRowFilter(int row)					const;
				QVariant					getColumnTitle(int column)				const;
				QVariant					getColumnIcon(int column)				const;
				QVariant					getColumnTypesWithCorrespondingIcon()	const;
				std::string					getComputedColumnError(size_t colIndex) const;
				bool						getColumnHasFilter(int column)			const;
				bool						isColumnUsedInEasyFilter(int column)	const;
				bool						isColumnNameFree(std::string name)		const;
				bool						isColumnNameFree(QString name)			const	{ return isColumnNameFree(name.toStdString()); }
				bool						isColumnComputed(size_t colIndex)		const;
				bool						isColumnComputed(std::string name)		const;
				bool						isColumnInvalidated(size_t colIndex)	const;

				int							setColumnTypeFromQML(int columnIndex, int newColumnType);
				bool						setColumnType(int columnIndex, columnType newColumnType);

				int							columnsFilteredCount();

				std::string					getColumnTypeNameForJASPFile(columnType columnType);
				void						writeDataSetToOStream(std::ostream & out, bool includeComputed);
				columnType					parseColumnTypeForJASPFile(std::string name);
				Json::Value					columnToJsonForJASPFile(size_t columnIndex, Json::Value & labelsData, size_t & dataSize);
				void						columnLabelsFromJsonForJASPFile(Json::Value xData, Json::Value columnDesc, size_t columnIndex, std::map<std::string, std::map<int, int> > & mapNominalTextValues);

				enum columnType				getColumnType(std::string columnName)	const;
				enum columnType				getColumnType(size_t columnIndex)		const	{ return _dataSet ? _dataSet->column(columnIndex).getColumnType() : columnType::unknown; }
				std::string					getColumnName(size_t columnIndex)		const	{ return _dataSet ? _dataSet->column(columnIndex).name() : ""; }
				int							getColumnIndex(std::string name)		const	{ return !_dataSet ? -1 : _dataSet->getColumnIndex(name); }
				int							getColumnIndex(QString name)			const	{ return getColumnIndex(name.toStdString()); }
				std::vector<int>			getColumnDataInts(size_t columnIndex);
				std::vector<double>			getColumnDataDbls(size_t columnIndex);
				void						setColumnDataInts(size_t columnIndex, std::vector<int> ints);
				void						setColumnDataDbls(size_t columnIndex, std::vector<double> dbls);
				size_t						getMaximumColumnWidthInCharacters(int columnIndex) const;
				QStringList					getColumnLabelsAsStringList(std::string columnName)		const;
				QStringList					getColumnLabelsAsStringList(size_t columnIndex)			const;

				bool						setFilterData(std::string filter, std::vector<bool> filterResult);
				void						resetFilterAllows(size_t columnIndex);
				int							filteredOut(size_t column)									const;
				void						resetAllFilters();

				void						labelMoveRows(size_t column, std::vector<size_t> rows, bool up);
				void						labelReverse(size_t column);

				std::vector<bool>			filterVector();
				void						setFilterVectorWithoutModelUpdate(std::vector<bool> newFilterVector) { if(_dataSet) _dataSet->setFilterVector(newFilterVector); }


				void						databaseStartSynching(bool syncImmediately);



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
				void				pauseEnginesSignal(bool unloadData);
				void				resumeEnginesSignal();
				bool				enginesInitializingSignal();
				void				freeDatasetSignal(DataSet * dataset);
				void				filteredOutChanged(int column);
				bool				checkDoSync();
				void				modelInit();
				void				columnNamesChanged();
				void				columnAboutToBeRemoved(int column);
				void				nameChanged();
				void				folderChanged();
				void				windowTitleChanged();
				void				loadedChanged();
				void				currentFileChanged();
				void				synchingIntervalPassed();

public slots:
				void				refresh() { beginResetModel(); endResetModel(); }
				void				refreshColumn(QString columnName);
				void				columnWasOverwritten(std::string columnName, std::string possibleError);
				void				notifyColumnFilterStatusChanged(int columnIndex);
				void				setColumnsUsedInEasyFilter(std::set<std::string> usedColumns);
				void				emptyValuesChangedHandler();
				void				setCurrentFile(QString currentFile);
				void				setFolder(QString folder);

private:
				///This function allows you to run some code that changes something in the _dataSet and will try to enlarge it if it fails with an allocation error. Otherwise it might keep going for ever?
				void				enlargeDataSetIfNecessary(std::function<void()> tryThis, const char * callerText);
				bool				isThisTheSameThreadAsEngineSync();
				bool				setAllowFilterOnLabel(const QModelIndex & index, bool newAllowValue);


private:
	static DataSetPackage	*	_singleton;
	DataSet					*	_dataSet					= nullptr;
	EngineSync				*	_engineSync					= nullptr;
	emptyValsType				_emptyValuesMap;

	QString						_currentFile,
								_folder;
	std::string					_analysesHTML,
								_id,
								_warningMessage,
								_initialMD5,
								_dataFilePath,
								_dataFilter					= DEFAULT_FILTER,
								_filterConstructorJSON		= DEFAULT_FILTER_JSON;

	bool						_isArchive					= false,
								_dataFileReadOnly,
								_isModified					= false,
								_isLoaded					= false,
								_hasAnalysesWithoutData		= false,
								_analysesHTMLReady			= false,
								_filterShouldRunInit		= false,
								_enginesLoadedAtBeginSync;

	Json::Value					_analysesData,
								_database					= Json::nullValue;
	Version						_archiveVersion,
								_dataArchiveVersion;

	uint						_dataFileTimestamp;

	ComputedColumns				_computedColumns;
	bool						_synchingData				= false;
	std::map<std::string, bool> _columnNameUsedInEasyFilter;
	internalPointerType			_internalPointers;	///< The hacky solution we used prior to Qt 6 (with fake pointers) was not quite workable in the end, so it is replaced by actual pointers in conjunction with `DataSetPackageSubNodeModel`s that interface on an actual tree model

	SubNodeModel			*	_dataSubModel,
							*	_filterSubModel,
							*	_labelsSubModel;
	
	QTimer						_databaseIntervalSyncher;

	friend class ComputedColumns; //temporary! Or well, should be thought about anyway
};

#endif // FILEPACKAGE_H
