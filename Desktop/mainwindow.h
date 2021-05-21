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
#include <QQmlApplicationEngine>
#include <QApplication>
#include <QFileSystemWatcher>

#include "analysis/analyses.h"
#include "analysisform.h"
#include "dataset.h"
#include "data/asyncloader.h"
#include "data/asyncloaderthread.h"
#include "data/columnsmodel.h"
#include "data/computedcolumnsmodel.h"
#include "data/datasettablemodel.h"
#include "data/fileevent.h"
#include "data/filtermodel.h"
#include "data/labelmodel.h"
#include "data/labelfiltergenerator.h"
#include "engine/enginesync.h"
#include "gui/aboutmodel.h"
#include "gui/columntypesmodel.h"
#include "gui/preferencesmodel.h"
#include "modules/dynamicmodule.h"
#include "modules/ribbonbutton.h"
#include "modules/ribbonmodelfiltered.h"
#include "modules/ribbonmodel.h"
#include "modules/upgrader/upgrader.h"
#include "jasptheme.h"
#include "results/ploteditormodel.h"
#include "results/resultsjsinterface.h"
#include "modules/ribbonmodeluncommon.h"
#include "results/resultmenumodel.h"
#include "utilities/jsonutilities.h"
#include "utilities/helpmodel.h"
#include "widgets/filemenu/filemenu.h"

#include "utilities/languagemodel.h"
#include <vector>

using namespace std;

using PlotEditor::PlotEditorModel;
using Modules::Upgrader;

///
/// Not only the main window of the application but also the main class.
/// Instantiates relevant models and loads QML (see loadQml)
/// it also handles a variety of slots
/// Most of the connections are also created by this (in makeConnections)
class MainWindow : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		progressBarVisible	READ progressBarVisible		WRITE setProgressBarVisible		NOTIFY progressBarVisibleChanged	)
	Q_PROPERTY(int		progressBarProgress	READ progressBarProgress	WRITE setProgressBarProgress	NOTIFY progressBarProgressChanged	)
	Q_PROPERTY(QString	progressBarStatus	READ progressBarStatus		WRITE setProgressBarStatus		NOTIFY progressBarStatusChanged		)
	Q_PROPERTY(QString	windowTitle			READ windowTitle											NOTIFY windowTitleChanged			)
	Q_PROPERTY(int		screenPPI			READ screenPPI				WRITE setScreenPPI				NOTIFY screenPPIChanged				)
	Q_PROPERTY(bool		dataAvailable		READ dataAvailable											NOTIFY dataAvailableChanged			)
	Q_PROPERTY(bool		analysesAvailable	READ analysesAvailable										NOTIFY analysesAvailableChanged		)
	Q_PROPERTY(bool		welcomePageVisible	READ welcomePageVisible		WRITE setWelcomePageVisible		NOTIFY welcomePageVisibleChanged	)
	Q_PROPERTY(QString	downloadNewJASPUrl	READ downloadNewJASPUrl		WRITE setDownloadNewJASPUrl		NOTIFY downloadNewJASPUrlChanged	)


	friend class FileMenu;
public:
	explicit MainWindow(QApplication *application);
	void open(QString filepath);
	void open(const Json::Value & dbJson);
	void testLoadedJaspFile(int timeOut, bool save);

	~MainWindow() override;

	bool	progressBarVisible()	const	{ return _progressBarVisible;	}
	int		progressBarProgress()	const	{ return _progressBarProgress;	}
	QString	progressBarStatus()		const	{ return _progressBarStatus;	}
	QString	windowTitle()			const;
	int		screenPPI()				const	{ return _screenPPI;			}
	bool	dataAvailable()			const	{ return _dataAvailable;		}
	bool	analysesAvailable()		const	{ return _analysesAvailable;	}
	bool	welcomePageVisible()	const	{ return _welcomePageVisible;	}
	bool	checkAutomaticSync()	const	{ return _checkAutomaticSync;	}
	QString downloadNewJASPUrl()	const	{ return _downloadNewJASPUrl;	}

	static MainWindow * singleton() { return _singleton; }

public slots:
	void setImageBackgroundHandler(QString value);
	void plotPPIChangedHandler(int ppi, bool wasUserAction);
	void setProgressBarProgress(int progressBarProgress);
	void setProgressBarVisible(bool progressBarVisible);
	void setWelcomePageVisible(bool welcomePageVisible);
	void setProgressBarStatus(QString progressBarStatus);
	void setAnalysesAvailable(bool analysesAvailable);
	void setDataAvailable(bool dataAvailable);
	void setScreenPPI(int screenPPI);

	void showRCommander();

	bool checkPackageModifiedBeforeClosing();
	void startDataEditorHandler();
	void clearModulesFoldersUser();

	void showAbout();

	void saveKeyPressed();
	void openKeyPressed();
	void syncKeyPressed();
	void refreshKeyPressed();
	void zoomInKeyPressed();
	void zoomOutKeyPressed();
	void zoomResetKeyPressed();	

	QObject * loadQmlData(QString data, QUrl url);

	QQmlContext * giveRootQmlContext();

	static QString	versionString();

	void	openFolderExternally(QDir folder);
	void	showLogFolder();

	void	setDownloadNewJASPUrl(QString downloadNewJASPUrl);

	void	showEnginesWindow(); //For debugging
	void	setCheckAutomaticSync(bool check)									{  _checkAutomaticSync = check;	}
	void	openGitHubBugReport() const;

private:
	void makeConnections();
	void initLog();
	void initQWidgetGUIParts();
	void startOnlineDataManager();
	void startDataEditor(QString path);
	void loadRibbonQML();
	void loadQML();


	void checkUsedModules();

	void packageChanged();
	void setPackageModified();
	bool closeRequestCheck(bool &isSaving);
	void saveTextToFileHandler(const QString &filename, const QString &data);

	void		removeAnalysis(Analysis *analysis);
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

	void _openFile();
	void _openDbJson();
	void connectFileEventCompleted(FileEvent * event);
	void refreshPlotsHandler(bool askUserForRefresh = true);

signals:
	void saveJaspFile();
	void editImageCancelled(		int			id);
	void updateAnalysesUserData(	QString		userData);
	void runButtonTextChanged(		QString		runButtonText);
	void runButtonEnabledChanged(	bool		runButtonEnabled);
	void progressBarVisibleChanged(	bool		progressBarVisible);
	void progressBarProgressChanged(int			progressBarProgress);
	void progressBarStatusChanged(	QString		progressBarStatus);
	void dataPanelVisibleChanged(	bool		dataPanelVisible);
	void analysesVisibleChanged(	bool		analysesVisible);
	void windowTitleChanged();
	void screenPPIChanged(			int			screenPPI);
	void dataAvailableChanged(		bool		dataAvailable);
	void analysesAvailableChanged(	bool		analysesAvailable);
	void welcomePageVisibleChanged(	bool		welcomePageVisible);
	void downloadNewJASPUrlChanged	(QString	downloadNewJASPUrl);
	void closeWindows();
	void hideDataPanel();

private slots:
	void resultsPageLoaded();
	void analysisResultsChangedHandler(Analysis* analysis);
	void analysisImageSavedHandler(Analysis* analysis);
	void removeAllAnalyses();

	void dataSetIORequestHandler(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet();
	void startDataEditorEventCompleted(FileEvent *event);
	void analysisAdded(Analysis *analysis);
	void resendResultsToWebEngine();

	void fatalError();
	void closeVariablesPage();
	void showProgress();
	void hideProgress();
	void setProgressStatus(QString status, int progress);
	void showAnalysis() { emit hideDataPanel(); _analyses->setVisible(true); }

	bool checkDoSync();
	void unitTestTimeOut();
	void saveJaspFileHandler();
	void logToFileChanged(bool logToFile);
	void logRemoveSuperfluousFiles(int maxFilesToKeep);

	void resetQmlCache();
	void setCurrentJaspTheme();
	//void onDataModeChanged(bool dataMode) { if(analysesAvailable()) setDataPanelVisible(dataMode); }
	void printQmlWarnings(const QList<QQmlError> &warnings);
	void setQmlImportPaths();

private:
	void _analysisSaveImageHandler(Analysis* analysis, QString options);
	void makeAppleMenu();

private:
	typedef std::map<Analysis*, AnalysisForm*> analysisFormMap;

	static MainWindow			*	_singleton;

	EngineSync					*	_engineSync				= nullptr;
	QQmlApplicationEngine		*	_qml					= nullptr;
	Analyses					*	_analyses				= nullptr;
	ResultsJsInterface			*	_resultsJsInterface		= nullptr;
	DataSetPackage				*	_package				= nullptr;
	DataSetTableModel			*	_datasetTableModel		= nullptr;
	labelFilterGenerator		*	_labelFilterGenerator	= nullptr;
	ColumnsModel				*	_columnsModel			= nullptr;
	ComputedColumnsModel		*	_computedColumnsModel	= nullptr;
	FilterModel					*	_filterModel			= nullptr;
	OnlineDataManager			*	_odm					= nullptr;
	Modules::DynamicModules		*	_dynamicModules			= nullptr;
	RibbonModel					*	_ribbonModel			= nullptr;
	RibbonModelFiltered			*	_ribbonModelFiltered	= nullptr;
	RibbonModelUncommon			*	_ribbonModelUncommon	= nullptr;
	QApplication				*	_application 			= nullptr;
	FileMenu					*	_fileMenu				= nullptr;
	HelpModel					*	_helpModel				= nullptr;
	AboutModel					*	_aboutModel				= nullptr;
	PreferencesModel			*	_preferences			= nullptr;
	ResultMenuModel				*	_resultMenuModel		= nullptr;
	LanguageModel				*	_languageModel			= nullptr;
	ColumnTypesModel			*	_columnTypesModel		= nullptr;
	LabelModel					*	_labelModel				= nullptr;
	PlotEditorModel				*	_plotEditorModel		= nullptr;
	JaspTheme					*	_jaspTheme				= nullptr;
	Upgrader					*	_upgrader				= nullptr;

	QSettings						_settings;

	int								_progressBarProgress,	//Runs from 0 to 100
									_screenPPI				= 1;

	QString							_openOnLoadFilename,
									_fatalError				= "The engine crashed...",
									_progressBarStatus,
									_downloadNewJASPUrl		= "";
	Json::Value						_openOnLoadDbJson		= Json::nullValue;

	AsyncLoader					*	_loader					= nullptr;
	AsyncLoaderThread				_loaderThread;

	bool							_applicationExiting		= false,
									_resultsViewLoaded		= false,
									_openedUsingArgs		= false,
									_runButtonEnabled		= false,
									_progressBarVisible		= false,
									_dataAvailable			= false,
									_analysesAvailable		= false,
									_savingForClose			= false,
									_welcomePageVisible		= true,
									_checkAutomaticSync		= false;

	QFont							_defaultFont;
};

#endif // MAINWIDGET_H
