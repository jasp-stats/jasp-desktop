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
#include "undostack.h"

class EngineSync;
class DataSetPackageSubNodeModel;

///
/// This class is meant as the single bottleneck between the main application and Qt and the data stored in sqlite.
/// To this end a clean separation has been attempted between any access of the data so that it can easily be controlled.
///
/// In order to have all that data available through this class a tree-model has been chosen here.
///
/// The structure of this tree should be described here...
class DataSetPackage : public QAbstractItemModel //Not QAbstractTableModel because of: https://stackoverflow.com/a/38999940 (And this being a tree model)
{
	Q_OBJECT
	Q_PROPERTY(int			columnsFilteredCount	READ columnsFilteredCount								NOTIFY columnsFilteredCountChanged	)
	Q_PROPERTY(QString		folder					READ folder					WRITE setFolder				NOTIFY folderChanged				)
	Q_PROPERTY(QString		windowTitle				READ windowTitle										NOTIFY windowTitleChanged			)
	Q_PROPERTY(bool			modified				READ isModified				WRITE setModified			NOTIFY isModifiedChanged			)
	Q_PROPERTY(bool			loaded					READ isLoaded				WRITE setLoaded				NOTIFY loadedChanged				)
	Q_PROPERTY(QString		currentFile				READ currentFile			WRITE setCurrentFile		NOTIFY currentFileChanged			)
	Q_PROPERTY(bool			dataMode				READ dataMode											NOTIFY dataModeChanged				)
	Q_PROPERTY(bool			synchingExternally		READ synchingExternally		WRITE setSynchingExternally NOTIFY synchingExternallyChanged	) //might have to be moved to dataset when we have multiple datasets, alse CurrentDataFile in FileMenu will need to be looked at...
	Q_PROPERTY(bool			manualEdits				READ manualEdits			WRITE setManualEdits		NOTIFY manualEditsChanged			) ///< Did the user change something in the data in such a way that external synching should be disabled if enabled?

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

		SubNodeModel	*	dataSubModel	() { return _dataSubModel;		}
		SubNodeModel	*	filterSubModel	() { return _filterSubModel;	}
		SubNodeModel	*	labelsSubModel	() { return _labelsSubModel;	}
		UndoStack		*	undoStack		() { return _undoStack;			}
		
		DataSetBaseNode *	indexPointerToNode(			const QModelIndex	& index	) const;
		bool				dataSetBaseNodeStillExists(	DataSetBaseNode		* node	) const;
		
		void				waitForExportResultsReady();

		void				beginLoadingData(	bool informEngines = true);
		void				endLoadingData(		bool informEngines = true);
		void				beginSynchingData(	bool informEngines = true);
		void				endSynchingDataChangedColumns(stringvec	&	changedColumns,		bool hasNewColumns = false, bool informEngines = true);
		void				endSynchingData(const stringvec		&	changedColumns,
											const stringvec		&	missingColumns,
											const strstrmap		&	changeNameColumns,  //origname -> newname
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
				QString				insertColumnSpecial(int column, const QMap<QString, QVariant>& props);
				QString				appendColumnSpecial(			const QMap<QString, QVariant>& props);

				QModelIndex			indexForSubNode(DataSetBaseNode * node)														const;
				int					filteredRowCount()																			const { return _dataSet->filter()->filteredRowCount(); }
	static		QVariant			getDataSetViewLines(bool up=false, bool left=false, bool down=true, bool right=true);

				int					dataRowCount()		const;
				int					dataColumnCount()	const;


				std::string			id()								const	{ return _id;							}
				QString				name()								const;
				QString				folder()							const	{ return _folder;						}
				bool				dataMode()							const;
				
				bool				isReady()							const	{ return _analysesHTMLReady;			}
				bool				isLoaded()							const	{ return _isLoaded;						 }
				bool				isJaspFile()						const	{ return _isJaspFile;					  } ///< for readability
				bool				isModified()						const	{ return _isModified;					   }
				std::string			initialMD5()						const	{ return _initialMD5;						 }
				bool				manualEdits()						const;
				QString				windowTitle()						const;
				QString				description()						const;
				QString				currentFile()						const	{ return _currentFile;						 }
				bool				hasAnalyses()						const	{ return _analysesData.size() > 0;				}
				bool				synchingData()						const	{ return _synchingData;								}
				std::string			dataFilePath()						const	{ return _dataSet ? _dataSet->dataFilePath() : "";  }
				bool				dataFileCanHaveLabels()				const;
				bool				isDatabase()						const	{ return _database != Json::nullValue;				}
		const	Json::Value		&	databaseJson()						const	{ return _database;								}
		const	QString			&	analysesHTML()						const	{ return _analysesHTML;							}
		const	Json::Value		&	analysesData()						const	{ return _analysesData;							}
		const	std::string		&	warningMessage()					const	{ return _warningMessage;						}
		const	Version			&	archiveVersion()					const	{ return _archiveVersion;						}
		const	Version			&	jaspVersion()						const	{ return _jaspVersion;							}

				// The data file might be read-only if it comes from the examples or read from an external database
				bool				dataFileReadOnly()					const	{ return _dataFileReadOnly;						}
				bool				currentFileIsExample()				const;
				long				dataFileTimestamp()					const	{ return _dataSet ? _dataSet->dataFileTimestamp() : 0;	}
				bool				isDatabaseSynching()				const	{ return _databaseIntervalSyncher.isActive();	}
				bool				filterShouldRunInit()				const	{ return _filterShouldRunInit;					}


				void				setFilterShouldRunInit(bool shouldIt)				{ _filterShouldRunInit			= shouldIt;			}
				void				setAnalysesData(const Json::Value & analysesData)	{ _analysesData					= analysesData;		}
				void				setArchiveVersion(Version archiveVersion)			{ _archiveVersion				= archiveVersion;	}
				void				setJaspVersion(Version jaspVersion)					{ _jaspVersion					= jaspVersion;		}
				void				updateDbToCurrentVersion();							///< Should be ran immediately after loading the jasp file
				void				setWarningMessage(std::string message)				{ _warningMessage				= message;			}
				void				setDataFilePath(std::string filePath, long timestamp = 0);
				void				setDatabaseJson(const Json::Value & dbInfo);
				void				setInitialMD5(std::string initialMD5)				{ _initialMD5					= initialMD5;		}
				void				setDataFileReadOnly(bool readOnly)					{ _dataFileReadOnly				= readOnly;			}
				void				setAnalysesHTML(const QString & html)				{ _analysesHTML					= html;				}
				void				setIsJaspFile(bool isJaspFile)						{ _isJaspFile					= isJaspFile;		}
				void				setHasAnalysesWithoutData()							{ _hasAnalysesWithoutData		= true;				}
				void				setModified(bool value);
				void				setAnalysesHTMLReady()								{ _analysesHTMLReady			= true;				}
				void				setId(std::string id)								{ _id							= id;				}
				void				setWaitingForReady()								{ _analysesHTMLReady			= false;			}
				void				setManualEdits(bool newManualEdits);
				void				setLoaded(bool loaded = true);
				void				setDescription(const QString& description);
				
				bool						initColumnWithStrings(			QVariant			colId,		const std::string & newName, const stringvec	& values, const stringvec	& labels=stringvec(),	const std::string & title = "", columnType desiredType = columnType::unknown, const stringset & emptyValues = stringset());
				void						initializeComputedColumns();
				
				void						pasteSpreadsheet(size_t row, size_t column, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> & labels, const intvec & colTypes, const QStringList & colNames, const std::vector<boolvec> & selected = {}); ///< If selected.size() >0 it is assumed to be the same size as labels/values. And it will make sure that it will only overwrite values where it is `true`

				void						columnSetDefaultValues(	const std::string	& columnName, columnType colType = columnType::unknown, bool emitSignals = true);
				Column *					createColumn(			const std::string	& name,		columnType colType);
				Column *					createComputedColumn(	const std::string	& name,		columnType type, computedColumnType desiredType, Analysis * analysis = nullptr);
				void						renameColumn(			const std::string	& oldColumnName, const std::string & newColumnName);
				void						removeColumn(			const std::string	& name);
				bool						columnExists(			Column				* column);
				void						columnsReorder(			const stringvec		& order);

				stringvec					getColumnNames();
				bool						isColumnDifferentFromStringValues(const std::string & columnName, const std::string & title, const stringvec & strVals, const stringvec & strLabs, const stringset & strEmptyVals);
				int							findIndexByName(const std::string & name)	const;

				bool						getRowFilter(				int						row)		const;
				QVariant					getColumnTypesWithIcons()										const;
				std::string					getComputedColumnError(		size_t					colIndex)	const;

				bool						isColumnUsedInEasyFilter(	const std::string	&	name)		const;
				bool						isColumnNameFree(			const std::string	&	name)		const;
				bool						isColumnNameFree(			const QString		&	name)		const	{ return isColumnNameFree(name.toStdString()); }
				bool						isColumnComputed(			size_t					colIndex)	const;
				bool						isColumnComputed(			const std::string	&	name)		const;
				bool						isColumnAnalysisNotComputed(const std::string	&	name)		const;

				bool						isColumnInvalidated(		size_t					colIndex)	const;

				bool						setColumnType(int		columnIndex,	columnType newColumnType);
				bool						setColumnTypes(intset	columnIndexes,	columnType newColumnType);
				

				int							columnsFilteredCount();

				void						writeDataSetToOStream(std::ostream & out, bool includeComputed);

				int							getColumnIndex(						const std::string & name)			const	{ return !_dataSet ? -1 : _dataSet->getColumnIndex(name); }
				int							getColumnIndex(						const QString	  & name)			const	{ return getColumnIndex(name.toStdString()); }
				Column*						getColumn(							const std::string & name)					{ return _dataSet->column(name); }
				enum columnType				getColumnType(						size_t				columnIndex)	const;
				enum columnType				getColumnType(						const QString	  &	name)			const;
				std::string					getColumnName(						size_t				columnIndex)	const;
				stringvec					getColumnDataStrs(					size_t				columnIndex);
				void						setColumnName(						size_t				columnIndex, const std::string	& newName);
				void						setColumnTitle(						size_t				columnIndex, const std::string	& newTitle);
				void						setColumnDescription(				size_t				columnIndex, const std::string	& newDescription);
				void						setColumnComputedType(				size_t				columnIndex, computedColumnType	type);
				void						setColumnComputedType(				const std::string &	columnName,	computedColumnType	type);
				void						setColumnHasCustomEmptyValues(		size_t				columnIndex, bool				  hasCustomEmptyValue);
				void						setColumnCustomEmptyValues(			size_t				columnIndex, const stringset	& customEmptyValues);
				void						columnsReverseValues(				intset				columnIndex);
				void						columnsSetAutoSortForColumns(		std::map<int,bool>	columnutoSort);
				qsizetype					getMaximumColumnWidthInCharacters(	int					columnIndex)				const;
				QStringList					getColumnLabelsAsStringList(		size_t				columnIndex)				const;
				stringvec					getColumnLabelsAsStrVec(			size_t				columnIndex)				const;
				boolvec						getColumnFilterAllows(				size_t				columnIndex)				const;
				QList<QVariant>				getColumnValuesAsDoubleList(		size_t				columnIndex)				const;
				Json::Value					serializeColumn(					const std::string & columnName)					const;
				void						deserializeColumn(					const std::string & columnName, const Json::Value& col);

				void						resetFilterAllows(					size_t				columnIndex);
				int							filteredOut(						size_t				columnIndex)				const;
				bool						labelNeedsFilter(					size_t				columnIndex)				const;
				void						labelMoveRows(						size_t				columnIndex, std::vector<qsizetype> rows, bool up);
				void						labelReverse(						size_t				columnIndex);
				bool						setFilterData(const std::string & filter, const boolvec & filterResult);
				void						resetAllFilters();
				std::vector<bool>			filterVector();
				void						setFilterVectorWithoutModelUpdate(std::vector<bool> newFilterVector) { if(_dataSet) _dataSet->filter()->setFilterVector(newFilterVector); }
				
				const stringset&			workspaceEmptyValues()										const;
				void						setWorkspaceEmptyValues(const stringset& emptyValues, bool resetModel = true);
				void						setDefaultWorkspaceEmptyValues();

				void						databaseStartSynching(bool syncImmediately);
				void						databaseStopSynching();
				bool						synchingExternally() const;
				void						checkComputedColumnDependenciesForAnalysis(	Analysis * analysis);
				stringset					columnsCreatedByAnalysis(					Analysis * analysis);
				std::string					freeNewColumnName(size_t startHere);
				void						dbDelete();
				void						resetVariableTypes();


signals:
				void				datasetChanged(	QStringList				changedColumns,
													QStringList				missingColumns,
													QMap<QString, QString>	changeNameColumns,
													bool					rowCountChanged,
													bool					hasNewColumns);
				void				columnsFilteredCountChanged();
				void				runFilter();
				void				badDataEntered(const QModelIndex index);
				void				allFiltersReset();
				void				labelFilterChanged();
				void				labelChanged(			QString columnName, QString originalLabel, QString newLabel);
				void				columnDataTypeChanged(	QString columnName);
				void				labelsReordered(		QString columnName);
				void				columnAddedManually(	QString columnName);
				void				chooseColumn(			int		colId);
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
				void				synchingExternallyChanged(bool);
				bool				askUserForExternalDataFile();
				void				checkForDependentColumnsToBeSent(	QString columnName);
				void				showWarning(						QString title, QString msg);
				void				manualEditsChanged();
				void				columnsBeingRemoved(				int columnIndex, int count);
				void				workspaceEmptyValuesChanged();
				void				descriptionChanged();

public slots:
				void				refresh()							{ beginResetModel(); endResetModel(); }
				void				refreshWithDelay();
				void				refreshColumn(						QString columnName);
				void				columnWasOverwritten(				const std::string & columnName, const std::string & possibleError);
				void				notifyColumnFilterStatusChanged(	int columnIndex);
				void				setColumnsUsedInEasyFilter(			stringset usedColumns);
				void				setCurrentFile(						QString currentFile);
				void				setFolder(							QString folder);
				void				generateEmptyData();
				void				onDataModeChanged(					bool dataMode);
				void				setSynchingExternallyFriendly(		bool synchingExternally);
				void				setSynchingExternally(				bool synchingExternally);
				Column			 *	requestComputedColumnCreation(		const std::string & columnName, Analysis * analysis);
				bool				requestColumnCreation(				const std::string & columnName, Analysis * analysis, columnType type);
				bool				requestComputedColumnDestruction(	const std::string & columnName, Analysis * analysis);
				void				checkDataSetForUpdates();
				void				delayedRefresh();
				void				resetFilterCounters();
				
private:
				bool				isThisTheSameThreadAsEngineSync();
				bool				setLabelAllowFilter(	const QModelIndex & index, bool newAllowValue);
				bool				setLabelDescription(	const QModelIndex & index, const QString & newDescription);
				bool				setLabelDisplay(		const QModelIndex & index, const QString & newLabel);
				bool				setLabelValue(			const QModelIndex & index, const QString & newLabel);
				QModelIndex			lastCurrentCell();
				int					getColIndex(QVariant colID);
				void				columnsApply(intset columnIndexes, std::function<bool (Column *)> applyThis);
				void				columnsApply(intset columnIndexes, std::function<bool (Column *, int)> applyThis);

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

	bool						_isJaspFile					= false,
								_dataFileReadOnly,
								_isModified					= false,
								_isLoaded					= false,
								_hasAnalysesWithoutData		= false,
								_analysesHTMLReady			= false,
								_filterShouldRunInit		= false,
								_dataMode					= false,
								_manualEdits				= false;

	Json::Value					_analysesData,
								_database					= Json::nullValue;
	Version						_archiveVersion,
								_jaspVersion;

	bool						_synchingData				= false;
	std::map<std::string, bool> _columnNameUsedInEasyFilter;

	SubNodeModel			*	_dataSubModel,
							*	_filterSubModel,
							*	_labelsSubModel;
	
	QTimer						_databaseIntervalSyncher,
								_delayedRefreshTimer;
	UndoStack				*	_undoStack					= nullptr;
	
};

#endif // FILEPACKAGE_H
