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
#include "data/asyncloader.h"
#include "data/asyncloaderthread.h"
#include "data/columnsmodel.h"
#include "data/computedcolumnmodel.h"
#include "data/datasettablemodel.h"
#include "data/fileevent.h"
#include "data/filtermodel.h"
#include "data/columnmodel.h"
#include "data/labelfiltergenerator.h"
#include "engine/enginesync.h"
#include "gui/aboutmodel.h"
#include "models/columntypesmodel.h"
#include "gui/preferencesmodel.h"
#include "modules/ribbonmodelfiltered.h"
#include "modules/ribbonmodel.h"
#include "modules/upgrader/upgrader.h"
#include "jasptheme.h"
#include "results/ploteditormodel.h"
#include "results/resultsjsinterface.h"
#include "modules/ribbonmodeluncommon.h"
#include "results/resultmenumodel.h"
#include "utilities/helpmodel.h"
#include "utilities/messageforwarder.h"
#include "utilities/reporter.h"
#include "utilities/codepageswindows.h"
#include "widgets/filemenu/filemenu.h"
#include "data/workspacemodel.h"
#include "utilities/languagemodel.h"

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
	Q_PROPERTY(bool			progressBarVisible	READ progressBarVisible		WRITE setProgressBarVisible		NOTIFY progressBarVisibleChanged	)
	Q_PROPERTY(int			progressBarProgress	READ progressBarProgress	WRITE setProgressBarProgress	NOTIFY progressBarProgressChanged	)
	Q_PROPERTY(QString		progressBarStatus	READ progressBarStatus		WRITE setProgressBarStatus		NOTIFY progressBarStatusChanged		)
	Q_PROPERTY(QString		windowTitle			READ windowTitle											NOTIFY windowTitleChanged			)
	Q_PROPERTY(int			screenPPI			READ screenPPI				WRITE setScreenPPI				NOTIFY screenPPIChanged				)
	Q_PROPERTY(bool			dataAvailable		READ dataAvailable											NOTIFY dataAvailableChanged			)
	Q_PROPERTY(bool			analysesAvailable	READ analysesAvailable										NOTIFY analysesAvailableChanged		)
	Q_PROPERTY(bool			welcomePageVisible	READ welcomePageVisible		WRITE setWelcomePageVisible		NOTIFY welcomePageVisibleChanged	)
	Q_PROPERTY(QString		downloadNewJASPUrl	READ downloadNewJASPUrl		WRITE setDownloadNewJASPUrl		NOTIFY downloadNewJASPUrlChanged	)
	Q_PROPERTY(bool			contactVisible		READ contactVisible			WRITE setContactVisible			NOTIFY contactVisibleChanged		)
	Q_PROPERTY(bool			communityVisible	READ communityVisible		WRITE setCommunityVisible		NOTIFY communityVisibleChanged	)
	Q_PROPERTY(QString		commUrl				READ commUrl												CONSTANT							)
	Q_PROPERTY(QString		commGold			READ commGold												CONSTANT							)
	Q_PROPERTY(QString		commSilver			READ commSilver												CONSTANT							)
	Q_PROPERTY(QString		commBronze			READ commBronze												CONSTANT							)
	Q_PROPERTY(QStringList	commThankYou		READ commThankYou											CONSTANT							)
	Q_PROPERTY(QString		commUrlMembers		READ commUrlMembers											CONSTANT							)
	Q_PROPERTY(QString		commHowToSupport	READ commHowToSupport										CONSTANT							)
	Q_PROPERTY(QString		contactUrlFeatures	READ contactUrlFeatures										CONSTANT							)
	Q_PROPERTY(QString		contactUrlBugs		READ contactUrlBugs											CONSTANT							)
	Q_PROPERTY(QString		contactText			READ contactText											NOTIFY contactTextChanged			)
	Q_PROPERTY(QString		questionsUrl		READ questionsUrl											CONSTANT							)

	friend class FileMenu;
public:
	explicit MainWindow(QApplication *application);
			~MainWindow() override;

	static MainWindow * singleton() { return _singleton; }

	void				showNewData();
	void				open(QString filepath);
	void				open(const Json::Value & dbJson);
	void				testLoadedJaspFile(int timeOut, bool save);
	void				reportHere(QString dir);

	bool				progressBarVisible()	const	{ return _progressBarVisible;	}
	int					progressBarProgress()	const	{ return _progressBarProgress;	}
	const QString &		progressBarStatus()		const	{ return _progressBarStatus;	}
	QString				windowTitle()			const;
	int					screenPPI()				const	{ return _screenPPI;			}
	bool				dataAvailable()			const	{ return _dataAvailable;		}
	bool				analysesAvailable()		const	{ return _analysesAvailable;	}
	bool				welcomePageVisible()	const	{ return _welcomePageVisible;	}
	bool				checkAutomaticSync()	const	{ return _checkAutomaticSync;	}
	bool				contactVisible()		const;
	bool				communityVisible()		const;
	QString				downloadNewJASPUrl()	const	{ return _downloadNewJASPUrl;	}
	const QStringList & commThankYou()			const;
	const QString &		commGold()				const;
	const QString &		commSilver()			const;
	const QString &		commBronze()			const;
	const QString 		commHowToSupport()		const;
	const QString 		commUrl()				const;
	const QString 		commUrlMembers()		const;
	const QString 		contactUrlFeatures()	const;
	const QString 		contactUrlBugs()		const;
	const QString 		contactText()			const;
	const QString		questionsUrl()			const { return "https://forum.cogsci.nl/index.php?p=/categories/jasp-bayesfactor"; }

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
	void setContactVisible(bool newContactVisible);
	void setCommunityVisible(bool newCommunityVisible);
	void setDefaultWorkspaceEmptyValues();
	void resetVariableTypes();

	void showRCommander();

	bool checkPackageModifiedBeforeClosing();
	bool startDataEditorHandler();
	void clearModulesFoldersUser();

	void showAbout();
	void showContact();
	void showCommunity();

	void saveKeyPressed();
	void saveAsKeyPressed();
	void openKeyPressed();
	void syncKeyPressed();
	void refreshKeyPressed();
	void zoomInKeyPressed();
	void zoomOutKeyPressed();
	void zoomResetKeyPressed();	
	void undo();
	void redo();

	QObject * loadQmlData(QString data, QUrl url);

	QQmlContext * giveRootQmlContext();

	static QString	versionString();

	void	openFolderExternally(QDir folder);
	void	showLogFolder();

	void	setDownloadNewJASPUrl(QString downloadNewJASPUrl);

	void	showEnginesWindow(); //For debugging
	void	setCheckAutomaticSync(bool check)									{  _checkAutomaticSync = check;	}
	void	openGitHubBugReport() const;
	void	reloadResults() const;

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

	void			removeAnalysis(Analysis *analysis);
	void			analysesCountChangedHandler();
	void			analysisChangedDownstreamHandler(int id, QString options);
	void			analysisSaveImageHandler(int id, QString options);
	void			analysisEditImageHandler(int id, QString options);
	void			removeAnalysisRequestHandler(int id);
	Json::Value		getResultsMeta();
	const QString	commConcatter(QStringList listIn, const QString & name) const;

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
	void checkEmptyWorkspace();

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
	void exitSignal(				int			returnCode = 0) const;
	void contactVisibleChanged();
	void communityVisibleChanged();
	void contactTextChanged();
	void resizeData(int row, int col);
	void qmlLoadedChanged();

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
	void showAnalysis();

	bool checkDoSync();
	void unitTestTimeOut();
	void saveJaspFileHandler();
	void logToFileChanged(bool logToFile);
	void logRemoveSuperfluousFiles(int maxFilesToKeep);

	void resetQmlCache();
	void setCurrentJaspTheme();
	void onDataModeChanged(bool dataMode);
	void printQmlWarnings(const QList<QQmlError> &warnings);
	void setQmlImportPaths();

private:
	void _analysisSaveImageHandler(Analysis* analysis, QString options);
	void makeAppleMenu();
	void qmlLoaded();
	void handleDeferredFileLoad();
	void checkForUpdates();

private:
	typedef std::map<Analysis*, AnalysisForm*> analysisFormMap;

	static MainWindow			*	_singleton;

	EngineSync					*	_engineSync				= nullptr;
	QQmlApplicationEngine		*	_qml					= nullptr;
	Analyses					*	_analyses				= nullptr;
	ResultsJsInterface			*	_resultsJsInterface		= nullptr;
	MessageForwarder			*	_msgForwarder			= nullptr;
	DataSetPackage				*	_package				= nullptr;
	DataSetTableModel			*	_datasetTableModel		= nullptr,
								*	_dataSetModelVarInfo	= nullptr;
	labelFilterGenerator		*	_labelFilterGenerator	= nullptr;
	ColumnsModel				*	_columnsModel			= nullptr;
	ComputedColumnModel			*	_computedColumnsModel	= nullptr;
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
	ColumnModel					*	_columnModel			= nullptr;
	PlotEditorModel				*	_plotEditorModel		= nullptr;
	JaspTheme					*	_jaspTheme				= nullptr;
	Upgrader					*	_upgrader				= nullptr;
	Reporter					*	_reporter				= nullptr;
	CodePagesWindows			*	_windowsWorkaroundCPs	= nullptr;
	WorkspaceModel				*	_workspaceModel			= nullptr;

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
									_resultsPageLoaded		= false,
									_qmlLoaded				= false,
									_openedUsingArgs		= false,
									_runButtonEnabled		= false,
									_progressBarVisible		= false,
									_dataAvailable			= false,
									_analysesAvailable		= false,
									_savingForClose			= false,
									_welcomePageVisible		= true,
									_checkAutomaticSync		= false,
									_contactVisible			= false,
									_communityVisible		= false;
									
	QFont							_defaultFont;
};

#endif // MAINWIDGET_H
