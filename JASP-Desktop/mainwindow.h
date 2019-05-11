//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef MAINWIDGET_H
#define MAINWIDGET_H



#include <QSettings>
#include <QApplication>
#include <QQmlApplicationEngine>

#include "analysis/analyses.h"
#include "analysis/analysisform.h"

#include "dataset.h"
#include "data/asyncloader.h"
#include "data/asyncloaderthread.h"
#include "data/columnsmodel.h"
#include "data/computedcolumnsmodel.h"
#include "data/datasettablemodel.h"
#include "data/fileevent.h"
#include "data/filtermodel.h"
#include "engine/enginesync.h"
#include "modules/dynamicmodule.h"
#include "modules/ribbonmodel.h"
#include "modules/ribbonbutton.h"
#include "modules/ribbonmodelfiltered.h"
#include "gui/preferencesmodel.h"
#include "results/resultmenumodel.h"
#include "utilities/resultsjsinterface.h"
#include "utilities/jsonutilities.h"
#include "utilities/helpmodel.h"
#include "utilities/aboutmodel.h"
#include "variablespage/levelstablemodel.h"
#include "variablespage/labelfiltergenerator.h"
#include "widgets/filemenu/filemenu.h"

class MainWindow : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		progressBarVisible	READ progressBarVisible		WRITE setProgressBarVisible		NOTIFY progressBarVisibleChanged	)
	Q_PROPERTY(int		progressBarProgress	READ progressBarProgress	WRITE setProgressBarProgress	NOTIFY progressBarProgressChanged	)
	Q_PROPERTY(QString	progressBarStatus	READ progressBarStatus		WRITE setProgressBarStatus		NOTIFY progressBarStatusChanged		)
	Q_PROPERTY(bool		dataPanelVisible	READ dataPanelVisible		WRITE setDataPanelVisible		NOTIFY dataPanelVisibleChanged		)
	Q_PROPERTY(QString	windowTitle			READ windowTitle			WRITE setWindowTitle			NOTIFY windowTitleChanged			)
	Q_PROPERTY(bool		datasetLoaded		READ datasetLoaded											NOTIFY datasetLoadedChanged			)
	Q_PROPERTY(int		screenPPI			READ screenPPI				WRITE setScreenPPI				NOTIFY screenPPIChanged				)
	Q_PROPERTY(bool		dataAvailable		READ dataAvailable											NOTIFY dataAvailableChanged			)
	Q_PROPERTY(bool		analysesAvailable	READ analysesAvailable										NOTIFY analysesAvailableChanged		)

	friend class FileMenu;
public:
	explicit MainWindow(QApplication *application);
	void open(QString filepath);
	void testLoadedJaspFile(int timeOut, bool save);

	~MainWindow() override;

	bool	progressBarVisible()	const	{ return _progressBarVisible;	}
	int		progressBarProgress()	const	{ return _progressBarProgress;	}
	QString	progressBarStatus()		const	{ return _progressBarStatus;	}
	bool	dataPanelVisible()		const	{ return _dataPanelVisible;		}
	QString	windowTitle()			const	{ return _windowTitle;			}
	bool	analysesVisible()		const	{ return _analysesVisible;		}
	bool	datasetLoaded()			const	{ return _datasetLoaded;		}
	int		screenPPI()				const	{ return _screenPPI;			}
	bool	dataAvailable()			const	{ return _dataAvailable;		}
	bool	analysesAvailable()		const	{ return _analysesAvailable;	}

	static QString columnTypeToString(int columnType) { return _columnTypeMap[columnType]; }

public slots:
	void setImageBackgroundHandler(QString value);
	void setProgressBarVisible(bool progressBarVisible);
	void setProgressBarProgress(int progressBarProgress);
	void setProgressBarStatus(QString progressBarStatus);
	void setDataPanelVisible(bool dataPanelVisible);
	void setAnalysesVisible(bool analysesVisible);
	void setDatasetLoaded(bool datasetLoaded);
	void setWindowTitle(QString windowTitle);
	void setScreenPPI(int screenPPI);
	void setDataAvailable(bool dataAvailable);
	void setAnalysesAvailable(bool analysesAvailable);

	bool checkPackageModifiedBeforeClosing();
	void startDataEditorHandler();

	void showAbout();

	void saveKeyPressed();
	void openKeyPressed();
	void syncKeyPressed();
	void refreshKeyPressed();
	void zoomInKeyPressed();
	void zoomOutKeyPressed();
	void zoomResetKeyPressed();

	//For qml:
	void showWarning(QString title, QString msg)								{ MessageForwarder::showWarning(title, msg); }
	QString browseOpenFile(QString caption, QString browsePath, QString filter) { return MessageForwarder::browseOpenFile(caption, browsePath, filter); }
	QString browseOpenFileDocuments(QString caption, QString filter);

	void	openFolderExternally(QDir folder);
	void	showLogFolder();

private:
	void makeConnections();
	void initLog();
	void initQWidgetGUIParts();
	void startOnlineDataManager();
	void startDataEditor(QString path);
	void loadRibbonQML();
	void loadQML();

	void delayedLoadHandler();
	void checkUsedModules();

	void packageChanged(DataSetPackage *package);
	void packageDataChanged(DataSetPackage *package, std::vector<std::string> &changedColumns, std::vector<std::string> &missingColumns, std::map<std::string, std::string> &changeNameColumns,	bool rowCountChanged);
	void setDataSetAndPackageInModels(DataSetPackage *package);
	void setPackageModified();
	void refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns, std::vector<std::string> &missingColumns, std::map<std::string, std::string> &changeNameColumns, bool rowCountChanged);

	bool closeRequestCheck(bool &isSaving);
	void saveTextToFileHandler(const QString &filename, const QString &data);

	void		removeAnalysis(Analysis *analysis);
	void		getAnalysesUserData();
	void		analysesCountChangedHandler();
	void		analysisChangedDownstreamHandler(int id, QString options);
	void		analysisSaveImageHandler(int id, QString options);
	void		analysisEditImageHandler(int id, QString options);
	void		removeAnalysisRequestHandler(int id);
	void		matchComputedColumnsToAnalyses();
	Json::Value getResultsMeta();

	void startComparingResults();
	void analysesForComparingDoneAlready();
	void finishComparingResults();
	void finishSavingComparedResults();

	bool enginesInitializing();
	void pauseEngines();
	void resumeEngines();

signals:
	void saveJaspFile();
	void imageBackgroundChanged(QString value);
	void updateAnalysesUserData(QString userData);
	void runButtonTextChanged(QString runButtonText);
	void runButtonEnabledChanged(bool runButtonEnabled);
	void progressBarVisibleChanged(bool progressBarVisible);
	void progressBarProgressChanged(int progressBarProgress);
	void progressBarStatusChanged(QString progressBarStatus);
	void dataPanelVisibleChanged(bool dataPanelVisible);
	void analysesVisibleChanged(bool analysesVisible);
	void windowTitleChanged(QString windowTitle);
	void screenPPIChanged(int screenPPI);
	void datasetLoadedChanged(bool datasetLoaded);
	void dataAvailableChanged(bool dataAvailable);
	void analysesAvailableChanged(bool analysesAvailable);


private slots:
	void welcomeScreenIsCleared(bool callDelayedLoad);
	void resultsPageLoaded();
	void showResultsPanel() { setDataPanelVisible(false); }

	void analysisResultsChangedHandler(Analysis* analysis);
	void analysisImageSavedHandler(Analysis* analysis);
	void removeAllAnalyses();

	void dataSetIORequestHandler(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet(bool showData);
	void startDataEditorEventCompleted(FileEvent *event);
	void dataSetChanged(DataSet * dataSet);
	void analysisAdded(Analysis *analysis);

	void fatalError();
	void emptyValuesChangedHandler();

	void closeVariablesPage();
	void showProgress(bool showData = true);
	void hideProgress();
	void setProgressStatus(QString status, int progress);

	void resetQmlCache();
	void unitTestTimeOut();
	void saveJaspFileHandler();
	void logToFileChanged(bool logToFile);
	void logRemoveSuperfluousFiles(int maxFilesToKeep);

private:
	void _analysisSaveImageHandler(Analysis* analysis, QString options);

private:
	typedef std::map<Analysis*, AnalysisForm*> analysisFormMap;

	EngineSync					*	_engineSync				= nullptr;
	QQmlApplicationEngine		*	_qml					= nullptr;
	Analyses					*	_analyses				= nullptr;
	ResultsJsInterface			*	_resultsJsInterface		= nullptr;
	DataSetPackage				*	_package				= nullptr;
	DataSetTableModel			*	_tableModel				= nullptr;
	LevelsTableModel			*	_levelsTableModel		= nullptr;
	labelFilterGenerator		*	_labelFilterGenerator	= nullptr;
	ColumnsModel				*	_columnsModel			= nullptr;
	ComputedColumnsModel		*	_computedColumnsModel	= nullptr;
	FilterModel					*	_filterModel			= nullptr;
	OnlineDataManager			*	_odm					= nullptr;
	DynamicModules				*	_dynamicModules			= nullptr;
	RibbonModel					*	_ribbonModel			= nullptr;
	RibbonModelFiltered			*	_ribbonModelFiltered	= nullptr;
	QApplication				*	_application 			= nullptr;
	FileMenu					*	_fileMenu				= nullptr;
	HelpModel					*	_helpModel				= nullptr;
	AboutModel					*	_aboutModel				= nullptr;
	PreferencesModel			*	_preferences			= nullptr;
	ResultMenuModel				*	_resultMenuModel		= nullptr;
	FileEvent					*	_openEvent				= nullptr;

	QSettings						_settings;

	int								_progressBarProgress,
									_screenPPI		= 1;

	QString							_openOnLoadFilename,
									_fatalError,
									_currentFilePath,
									_progressBarStatus,
									_windowTitle;

	AsyncLoader						_loader;
	AsyncLoaderThread				_loaderThread;

	bool							_applicationExiting		= false,
									_resultsViewLoaded		= false,
									_openedUsingArgs		= false,
									_runButtonEnabled		= false,
									_progressBarVisible		= false,
									_dataPanelVisible		= false,
									_analysesVisible		= false,
									_datasetLoaded			= false,
									_dataAvailable			= false,
									_analysesAvailable		= false,
									_savingForClose			= false;

	static QString					_iconPath;
	static QMap<QString, QVariant>	_iconFiles,
									_iconInactiveFiles,
									_iconDisabledFiles;
	static QMap<int, QString>		_columnTypeMap; //Should this be in Column ?

};

#endif // MAINWIDGET_H
