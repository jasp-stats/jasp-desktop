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

#include <map>
#include <QUrl>
#include <QTimer>
#include <cstddef>
#include "version.h"
#include <QFileInfo>
#include <json/json.h>
#include "workspace.h"
#include "databaseinterface.h"
#include <QSortFilterProxyModel>
#include <QPointer>

class EngineSync;

///
/// DataSetPackage is the Desktop-side wrapper around a Workspace (the multi-dataset model in CommonData).
///
/// It handles loading and creation of DataSets, which handle interaction with the database via themselves and the other DataSetBaseNodes
/// 
class DataSetPackage : public QObject
{
	Q_OBJECT

	Q_PROPERTY(QString		folder					READ folder						WRITE setFolder					NOTIFY folderChanged				)
	Q_PROPERTY(QString		windowTitle				READ windowTitle												NOTIFY windowTitleChanged			)
	Q_PROPERTY(bool			modified				READ isModified					WRITE setModified				NOTIFY isModifiedChanged			)
	Q_PROPERTY(bool			modifiedAfterAutoSave	READ isModifiedAfterAutoSave	WRITE setModifiedAfterAutoSave	NOTIFY isModifiedAfterAutoSaveChanged			)
	Q_PROPERTY(bool			loaded					READ isLoaded					WRITE setLoaded					NOTIFY loadedChanged				)
	Q_PROPERTY(QString		currentFile				READ currentFile				WRITE setCurrentFile			NOTIFY currentFileChanged			)
	Q_PROPERTY(bool			dataMode				READ dataMode													NOTIFY dataModeChanged				)
	Q_PROPERTY(bool			manualEdits				READ manualEdits				WRITE setManualEdits			NOTIFY manualEditsChanged			) ///< Did the user change the *shown* dataset by hand, so that external synching should be off? The flag itself lives on DataSet.
	Q_PROPERTY(DataSet *	dataSet					READ dataSet													NOTIFY shownDataSetChanged			) 
	Q_PROPERTY(Workspace *	workspace				READ workspace													NOTIFY workspaceChanged				)
public:
	
	static DataSetPackage *	pkg() { return _singleton; }

							DataSetPackage(QObject * parent);
							~DataSetPackage();
		static Filter	*	filter();
		Workspace		*	workspace() const { return _workspace; }
		DataSet			*	dataSet() const { return _workspace ? _workspace->shownDataSet() : nullptr; } 
		void				setEngineSync(EngineSync * engineSync);
		void				reset(bool newDataSet = true);

		void				createWorkspace();
		DataSet			*	createDataSet();	///< Creates *OR* recreates a dataset in database
		void				connectWorkspace();
        void                loadWorkspace(std::function<void(float)> progressCallback = [](float){});      ///< Assumes internal.sqlite has just been loaded from a JASPFile and will init DataSet etc with it.
		void				deleteWorkspace(bool dbDeletePlease=true);	///< Deletes dataset from memory but not from database
		bool				hasDataSet() { return dataSet(); }

		void				pauseEngines();
		void				resumeEngines();
		bool				enginesInitializing()	{ return emit enginesInitializingSignal();	}

		void				waitForExportResultsReady();

		void				stopEngines();
		void				restartEngines();

				void				refresh();


				std::string			id()								const	{ return _id;							}
				QString				name()								const;
				QString				folder()							const	{ return _folder;						}
				bool				dataMode()							const;
				
				
				bool				isReady()							const	{ return _analysesHTMLReady;			}
				bool				isLoaded()							const	{ return _isLoaded;						 }
				bool				isJaspFile()						const	{ return _isJaspFile;					  }
				bool				isModified()						const	{ return _isModified;					   }
				bool				isModifiedAfterAutoSave()			const	{ return _isModifiedAfterAutoSave;	 	    }
				bool				hasAnalysesWithoutData()			const	{ return _hasAnalysesWithoutData;			 }
				std::string			initialMD5()						const	{ return _initialMD5;						 }
				QString				windowTitle()						const;
				QString				description()						const;
				QString				currentFile()						const	{ return _currentFile;						 }
				QString				autoSavedFileName()					const;
				bool				hasAnalyses()						const	{ return _analysesData.size() > 0;				}
		const	QString			&	analysesHTML()						const	{ return _analysesHTML;							}
		const	Json::Value		&	analysesData()						const	{ return _analysesData;							}
		const	std::string		&	warningMessage()					const	{ return _warningMessage;						}
		const	Version			&	archiveVersion()					const	{ return _archiveVersion;						}
		const	Version			&	jaspVersion()						const	{ return _jaspVersion;							}

				// The data file might be read-only if it comes from the examples or read from an external database
				bool				isReadOnlyFile()					const	{ return _fileReadOnly;						}
				bool				currentJaspFileIsNonSaveable()		const;
				bool				filePathIsNonSaveable(const QString &path) const;
				
				void				setAnalysesData(const Json::Value & analysesData);
				void				setArchiveVersion(Version archiveVersion)			{ _archiveVersion				= archiveVersion;	}
				void				setJaspVersion(Version jaspVersion)					{ _jaspVersion					= jaspVersion;		}
				void				updateDbToCurrentVersion();							///< Should be ran immediately after loading the jasp file
				void				setWarningMessage(std::string message)				{ _warningMessage				= message;			}
				void				setInitialMD5(std::string initialMD5)				{ _initialMD5					= initialMD5;		}
				void				setFileReadOnly(bool readOnly)						{ _fileReadOnly					= readOnly;			}
				void				setAnalysesHTML(const QString & html)				{ _analysesHTML					= html;				}
				void				setIsJaspFile(bool isJaspFile)						{ _isJaspFile					= isJaspFile;		}
				void				setHasAnalysesWithoutData()							{ _hasAnalysesWithoutData		= true;				}
				void				setModifiedAfterAutoSave(bool value);
				void				setModified(bool value = true);
				void				setModifiedFileMenu() { setModified(); }
				void				setAnalysesHTMLReady()								{ _analysesHTMLReady			= true;				}
				void				setId(std::string id)								{ _id							= id;				}
				void				setWaitingForReady()								{ _analysesHTMLReady			= false;			}
				void				setLoaded(bool loaded = true);
				void				setDescription(const QString& description);
				
	static		int					thresholdScale();
	static		int					orderByValueByDefault();
				const stringset&	currentDataSetEmptyValues()										const;
				bool				workspaceShowRSyntax()										const;
				void				setDataSetEmptyValues(const stringset& emptyValues, bool resetModel = true);
				void				setDefaultWorkspaceEmptyValues();
				void				setWorkspaceShowRSyntax(bool show);
				void				dbDelete();
				void				resetVariableTypes();

				
				bool				manualEdits() const;
				void				setManualEdits(bool newManualEdits);

				/// Is the shown dataset actually being kept in synch with an external data file (or database)?
				bool				synchingExternally() const;
				
signals:
				void				datasetChanged(	int						dataSetID,
													QStringList				changedColumns,
													QStringList				missingColumns,
													QMap<QString, QString>	changeNameColumns,
													bool					rowCountChanged,
													bool					hasNewColumns);
				void				runFilter();
				void				badDataEntered(const QModelIndex index);
				void				allFiltersReset();
				
				void				columnAddedManually(	QString		columnName);
				void				chooseColumn(			int			colId); /// In the currently shown DataSet!
				void				showAnalysis(			int			analysisId);
				void				isModifiedChanged();
				void				isModifiedAfterAutoSaveChanged();
				bool				enginesInitializingSignal();
				void				filteredOutChanged(int column);
				bool				checkDoSync();
				void				nameChanged();
				void				folderChanged();
				void				windowTitleChanged();
				void				loadedChanged();
				void				currentFileChanged();
				void				newDataLoaded();
				void				dataModeChanged(bool dataMode);
				bool				askUserForExternalDataFile();
				void				checkForDependentColumnsToBeSent(	QString columnName);
				void				showWarning(						QString title, QString msg);
				void				workspaceEmptyValuesChanged();
				void				descriptionChanged();
				void				refreshAllAnalyses(Filter * f);
				void				refreshAllCompCols(Filter * f);
				void				makeAnAutoSave();
				void				shownDataSetChanged(DataSet * dataSet);
				void				shownFilterChanged();
				void				dataSetCreated(int dataSetId);
				void				dataSetRemoved(int dataSetId);
				void				sendFilter(			int dataSetID, const QString & generatedFilter, const QString & filter);
				void				sendFilterByName(	int dataSetID, const QString & name, const QString & module);
				void				filtersCountChanged();
				void				workspaceChanged();
				void				runComputedColumn(int dataSetId, QString columnName, QString code, columnType columnType);
				void				runComputedDataSet(int dataSetId, QString code, int defaultInputFilterId);
				void				filterByNameDone(int dataSetId, const QString &name, const QString &error);
				void				manualEditsChanged();
				void				synchingExternallyChanged(bool synchingExternally);
				void				checkForDependentAnalyses(Column * column);
				
public slots:
				void				refreshColumn(						QString columnName);
				void				columnWasOverwritten(				const std::string & columnName, const std::string & possibleError);
				void				setCurrentFile(						QString currentFile);
				void				setFolder(							QString folder);
				void				generateEmptyData();
				void				onDataModeChanged(					bool dataMode);
				void				checkDataSetForUpdates();
				void				handleAutoSave();

				void				prepareForLanguageChange();
				void				languageChangeDone();
				void				handleAutoSavePrefChange();

				void				setSynchingExternally(			bool synchingExternally);	///< (Re)starts or stops the synching of the shown dataset with its external data file
				void				setSynchingExternallyFriendly(	bool synchingExternally);	///< Same, but lets the user generate or find a data file first when there is none (or when it was edited by hand)
				
private:
				void				onUndoCleanChanged(bool clean);	///< Undone back to the point where the data still matched the data file? Then the synching can go back on.
				void				emitSynchingExternallyChanged();
				void				trackShownDataSet();			///< Follows the synch- and edit-state of whichever dataset is shown, so this class keeps reflecting reality
				bool				isThisTheSameThreadAsEngineSync();
				void				columnsApply(int dataSetId, intset		columnIndxs, std::function<bool (Column *)>			applyThis);
				void				columnsApply(int dataSetId, stringset	columnNames, std::function<bool (Column *)>			applyThis);
				void				columnsApply(int dataSetId, intset		columnIndxs, std::function<bool (Column *, int)>	applyThis);
				void				columnsApply(int dataSetId, stringset	columnNames, std::function<bool (Column *, int)>	applyThis);

private:
	static DataSetPackage	*	_singleton;
	DatabaseInterface		*	_db							= nullptr;
	Workspace				*	_workspace					= nullptr;
	EngineSync				*	_engineSync					= nullptr;

	QString						_currentFile,
								_folder,
								_analysesHTML;
	std::string					_id,
								_warningMessage,
								_initialMD5;

	bool						_isJaspFile					= false,
								_fileReadOnly				= false,
								_isModified					= false,
								_isModifiedAfterAutoSave	= false,
								_isLoaded					= false,
								_hasAnalysesWithoutData		= false,
								_analysesHTMLReady			= false,
								_waitingForLanguageChange	= false;
	QPointer<DataSet>			_synchTrackedDataSet;
	Json::Value					_analysesData;
	Version						_archiveVersion,
								_jaspVersion;
	QTimer						_autoSaveTimer;
};

#endif // FILEPACKAGE_H
