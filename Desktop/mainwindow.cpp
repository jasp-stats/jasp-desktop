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
#include <QDir>

#include <QFile>
#include <QUrl>
#include <QShortcut>
#include <QDesktopServices>
#include <QQmlContext>
#include <QQuickItem>
#include <QQuickStyle>
#include <QQuickWindow>
#include <QtWebEngineQuick/qtwebenginequickglobal.h>
#include <QtWebEngineQuick/qquickwebengineprofile.h>
#include <QtWebEngineQuick/qquickwebenginedownloadrequest.h>
#include <QAction>
#include <QMenuBar>
#include <exception>
#include <iostream>

#include "log.h"
#include "timers.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "mainwindow.h"

#include "gui/preferencesmodel.h"
#include "data/exporters/jaspexporter.h"
#include "utilities/application.h"
#include "gui/jaspversionchecker.h"
#include "ALTNavigation/altnavcontrol.h"
#include "utilities/messageforwarder.h"

#include "modules/installedmodules.h"
#include "modules/dynamicmodules.h"
#include "utilities/reporter.h"
#include "modules/menumodel.h"

#include "qquick/datasetview.h"
#include "qquick/rcommander.h"

#include "resultstesting/compareresults.h"

#include "qutils.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "utilities/qmlutils.h"
#include "utilities/reporter.h"
#include "utilities/allhelp.h"

#include "widgets/filemenu/filemenu.h"
#include "rsyntax/formulabase.h"
#include "utilities/desktopcommunicator.h"

#include "rpc/jasprpcdispatcher.h"
#include "rpc/jasprpcserver.h"
#include "ai/agentstatetracker.h"

#include "boost/iostreams/stream.hpp"
#include <boost/iostreams/device/null.hpp>

#include "communitydefs.h"

using namespace std;
using namespace Modules;

MainWindow * MainWindow::_singleton	= nullptr;

MainWindow::MainWindow(Application * application) : QObject(application), _application(application)
{
	std::cout << "MainWindow constructor started" << std::endl;
	connect(this, &MainWindow::exitSignal, this, &QApplication::exit, Qt::QueuedConnection);

	assert(!_singleton);
	_singleton = this;
	JASPTIMER_START(MainWindowConstructor);

	
	QQuickStyle::setStyle("JASP.Style");
	QQuickStyle::setFallbackStyle("Basic");
	QQuickWindow::setTextRenderType(Settings::value(Settings::GUI_USE_QT_TEXTRENDER).toBool() ?
										QQuickWindow::QtTextRendering : QQuickWindow::NativeTextRendering);

	TempFiles::init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	makeAppleMenu(); //Doesnt do anything outside of magical apple land
	
	std::cout << "Going to construct the necessary models for JASP to function." << std::endl;

	//The order of these constructors is deliberate (up to some extent anyway). If you change the order you might find that stuff explodes randomly (although most likely during startup)
	_rpcDispatcher 		= new JaspRpcDispatcher();
	_rpcServer 			= new JaspRpcServer(*_rpcDispatcher, this,
		PreferencesModel::prefs()->rpcServerIp(),
		PreferencesModel::prefs()->rpcServerPort());
	_qml					= new QQmlApplicationEngine(this);
	_languageModel			= new LanguageModel(application, _qml, this);
	_loader					= new AsyncLoader(nullptr);
	_preferences			= new PreferencesModel(this);
	_aiConfigModel			= new AIConfigModel(this);
	_package				= new DataSetPackage(this);
	_dynamicModules			= new DynamicModules(this);
	_upgrader				= new Upgrader(this);
	_analyses				= new Analyses();
	_engineSync				= new EngineSync(this);
	_datasetTableModel		= new DataSetTableModel(this);
	_columnModel			= new ColumnModel();

	initLog(); //initLog needs _preferences and _engineSync!

	Log::log() << "JASP " << AppInfo::version.asString() << " from commit " << AppInfo::gitCommit << " and branch " << AppInfo::gitBranch << " is continuing initialization." << std::endl;

	_aiBridge				= new AiBridge(this);

		_resultsJsInterface		= new ResultsJsInterface();
	_odm					= new OnlineDataManager(this);
	_columnsModel			= new ColumnsModel(_datasetTableModel);			// We do not want filtered-out columns/levels to be selectable in other guis, see: https://github.com/jasp-stats/INTERNAL-jasp/issues/2322
	_workspaceModel			= new WorkspaceModel(this);
	_filterModel			= new FilterModel(this);
	_ribbonModel			= new RibbonModel();
	_ribbonModelFiltered	= new RibbonModelFiltered(this, _ribbonModel);
	_ribbonModelUncommon	= new RibbonModelUncommon(this, _ribbonModel);
	_fileMenu				= new FileMenu(this);
	_helpModel				= new HelpModel(this);
	_allHelp				= new AllHelp(this);
	_aboutModel				= new AboutModel(this);
	_encryptionModel		= new EncryptionSettingsModel(this);
	_resultMenuModel		= new ResultMenuModel(this);
	_plotEditorModel		= new PlotEditorModel();
	_columnTypesModel		= new ColumnTypesModel(this);
	_jaspConfiguration		= JASPConfiguration::getInstance(this);
	_moduleLibrary			= new ModuleLibrary();
	_csvPreviewModel		= new CsvPreviewModel(this);

#ifdef WIN32
	_windowsWorkaroundCPs	= new CodePagesWindows(this);
#endif

	_msgForwarder = new MessageForwarder(this);

	startOnlineDataManager();

	makeConnections();

	qmlRegisterUncreatableType<MessageForwarder>				("JASP",			1, 0, "MessageForwarder",	"You can't touch this"				);

	qmlRegisterType<DataSetView>								("JASP",			1, 0, "DataSetView"						);
	qmlRegisterType<JaspTheme>									("JASP",			1, 0, "JaspTheme"						);
	qmlRegisterType<RCommander>									("JASP",			1, 0, "RCommander"						);
	qmlRegisterType<ResultsJsInterface>							("JASP",			1, 0, "ResultsJsInterface"				);
	qmlRegisterType<ColumnModel>								("JASP",			1, 0, "ColumnModel"						);
	qmlRegisterUncreatableType<PlotEditor::AxisModel>			("JASP.PlotEditor",	1, 0, "AxisModel",					"Can't make it");
	qmlRegisterUncreatableType<PlotEditor::PlotEditorModel>		("JASP.PlotEditor",	1, 0, "PlotEditorModel",			"Can't make it");

	ALTNavControl::ctrl()->enableAlTNavigation(_preferences->ALTNavModeActive());
	QmlUtils::setGlobalPropertiesInQMLContext(_qml->rootContext());
	QmlUtils::registerQmlModuleTypes();

	QTimer::singleShot(0, this, [&]() { loadQML(); });

	_languageModel->setApplicationEngine(_qml);

	_engineSync->start();
	
	checkForUpdates();

	QTimer::singleShot(0, this, [&]() { _jaspConfiguration->processConfiguration();  });
	
	_progressBarTimer = new QTimer(this);
	_progressBarTimer->setSingleShot(true);
	
	connect(_progressBarTimer, &QTimer::timeout, this, [this](){ _setProgressBarVisible(false); });
	
	_languageModel->setDefaultLocaleFromCurrent(); //Make sure (Q)ColumnUtils knows whats up

	Log::log() << "JASP Desktop started and Engines initalized." << std::endl;

	// Ensure the agent state tracker is initialized (also done by AiBridge,
	// but this covers the case where the AI feature is not yet active).
	AgentStateTracker::init();

	registerRpcHandlers();

	if (PreferencesModel::prefs()->rpcServerEnabled() && !_rpcServer->start())
		Log::log() << "JASP-RPC server failed to start." << std::endl;
	

	JASPTIMER_FINISH(MainWindowConstructor);
}


void MainWindow::checkForUpdates()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() || QCoreApplication::applicationName() == "JASPTest")
		return;
	
	if(PreferencesModel::prefs()->checkUpdatesAskUser())
	{
		bool answer = MessageForwarder::showYesNo(
					tr("Check for updates"), 
					tr(R"MultiLine(Should JASP check for updates at our server and let you know if there is a new version?

JASP doesn't share any of your data when it gets updates, not even which version of JASP you are using.
It does share your IP-address with our server but that is required for internet to function.

This setting can always be changed in the Interface Preferences.)MultiLine"), 
					tr("Yes"), 
					tr("No"));
		
		PreferencesModel::prefs()->setCheckUpdatesAskUser(false);
		PreferencesModel::prefs()->setCheckUpdates(answer);
	}
	
	if(PreferencesModel::prefs()->checkUpdates() && JASPVersionChecker::timeForDailyCheck())
	{
		JASPVersionChecker * jaspVersionChecker = new JASPVersionChecker(this);
		
		connect(jaspVersionChecker, &JASPVersionChecker::showDownloadButton, this, &MainWindow::setDownloadNewJASPUrl);
	}
}

MainWindow::~MainWindow()
{
	Log::log() << "MainWindow::~MainWindow()" << std::endl;
	
	delete _aiBridge;
	delete _rpcServer;
	delete _rpcDispatcher;

	_engineSync->killProcessTimer();

	try
	{
		DatabaseInterface::closeInterfaces();
	}
	catch(...) {}

	try
	{
		_analyses->destroyAllForms();
	}
	catch(...) {}

	_singleton = nullptr;

	try
	{
		//Clean up all QML to get rid of warnings and hopefully fix https://github.com/jasp-stats/jasp-issues/issues/667
		//Going backwards to make sure the theme isnt deleted before everything that depends on it
		for(int i=_qml->rootObjects().size() - 1; i >= 0; i--)
			delete _qml->rootObjects().at(i);

		delete _qml;

	}
	catch(...)	{}

	try
	{
		_odm->clearAuthenticationOnExit(OnlineDataManager::OSF);

		delete _resultsJsInterface;
	}
	catch(...)	{}
}

QString MainWindow::windowTitle() const
{
	return _package->windowTitle();
}

QString MainWindow::currentFileUserReadable() const
{
	QString name	= '"' + QFileInfo(_package->currentFile()).fileName() + '"',
			folder	= _package->folder();
	
#ifdef _WIN32
	if(folder.startsWith(AppDirs::examples().replace('/', '\\')))
#else
	if(folder.startsWith(AppDirs::examples()))
#endif
		folder = "";

	
	if(folder != "")
		return tq("%1 in %2").arg(name).arg('"'+folder+'"');
	else
		return name;
}

const QStringList & MainWindow::commThankYou() const
{
	static QStringList thankYou = [](){
		QStringList thankThese = Coop::goldTier();
		for(const QString & silver : Coop::silverTier())
			thankThese.append(silver);
		return thankThese;
	}();
	
	return thankYou;
}


const QString MainWindow::commConcatter(QStringList listIn, const QString & name) const
{
	if(listIn.size() == 0)
		return "Something is wrong with " + name;

	if(listIn.size() > 1)
		listIn[listIn.size()-1] = tr("and %1").arg(listIn[listIn.size()-1]);

	return listIn.join(", ");
}

const QString & MainWindow::commGold() const
{
	static QString golds = commConcatter(Coop::goldTier(), "Coop::goldTier()");
	return golds;
}

const QString & MainWindow::commSilver() const
{
	static QString silvers = commConcatter(Coop::silverTier(), "Coop::silverTier()");
	return silvers;
}

const QString & MainWindow::commBronze() const
{
	static QString bronzes = commConcatter(Coop::bronzeTier(), "Coop::bronzeTier()");
	return bronzes;
}

const QString MainWindow::commHowToSupport() const
{
	return Coop::howToSupport();
}

const QString MainWindow::commUrl() const
{
	return Coop::communityUrl();
}

const QString MainWindow::commUrlMembers() const
{
	return Coop::communityMembersUrl();
}

const QString MainWindow::contactUrlFeatures() const
{
#ifdef PRO
	return QString("http://support.jasp-services.com/") + PRO_COMPANY_NAME + "/issues/new?template=.gitea%2fISSUE_TEMPLATE%2ffeature-request.yml";	
#else
	return "https://jasp-stats.org/request-feature";	
#endif
}

const QString MainWindow::contactUrlBugs() const
{
#ifdef PRO
	return QString("http://support.jasp-services.com/") + PRO_COMPANY_NAME + "/issues/new?template=.gitea%2fISSUE_TEMPLATE%2fbug-report.yml";	
#else
	return "https://jasp-stats.org/report-bug";
#endif
}

const QString MainWindow::contactUrlCrashReport() const
{
#ifdef PRO
	return QString("http://support.jasp-services.com/") + PRO_COMPANY_NAME + "/issues/new?template=.gitea%2fISSUE_TEMPLATE%2fcrash-report.yml";	
#else
	return "https://jasp-stats.org/report-bug";
#endif
}

const QString MainWindow::contactText() const
{
#ifdef PRO
	return tr(
		"<h3>Contact</h3>\n"
		"The following links will bring you directly to your company's own issue tracker.\n"
		"<ul><li><a href=\"%1\">Feature requests</a>, when you would like something added to JASP.</li>"
		"<li><a href=\"%2\">Bug reports</a>, when a feature in JASP doesn't work as it should.</li>"
		"<li><a href=\"%3\">Crash reports</a>, for the unfortunate situation where JASP crashes.</li>"
		"</ul>\n"
		"There you will be in direct contact with the JASP software developers.\n"
		"\n"
		"You can find out more about JASP Services BV at <a href=\"%4\">our website</a>."
	)
	.replace("&", "&amp;").replace(", ", ",&nbsp;").replace("\n", "<br>")
	.arg(	contactUrlFeatures()
	,		contactUrlBugs()
	,		contactUrlCrashReport()
	,		"https://jasp-services.com");
#else
	return tr(
		"<h3>Contact</h3>\n"
		"For <a href=\"%1\">feature requests</a> and <a href=\"%2\">bug reports</a>: please post an issue on our GitHub page, <a href=\"%3\">as explained here.</a>\n"
		"This will bring you in direct contact with the JASP software developers.\n"
		"\n"
		"For statistical questions: please post an issue <a href=\"%4\">on the JASP Forum.</a>\n"
		"\n"
		"For information on the JASP Community: please read <a href=\"%5\">the information on the JASP website</a>\n"
		"\n"
		"For suggesting we add your institution to the <a href=\"%6\">JASP World Map</a> please send an email to <a href=\"%7\">communications@jasp-stats.org</a>.\n"
		"\n"
		"For individual donations: please visit <a href=\"%8\">the JASP website</a>.\n"
	)
	.replace("&", "&amp;").replace(", ", ",&nbsp;").replace("\n", "<br>")
	.arg(	contactUrlFeatures()
	,		contactUrlBugs()
	,		"https://jasp-stats.org/2018/03/29/request-feature-report-bug-jasp/"
	,		"https://forum.cogsci.nl/index.php?p=/categories/jasp-bayesfactor"
	,		commUrl()
	,		"https://jasp-stats.org/world-map/"
	,		"mailto:communications@jasp-stats.org"
	,		"https://jasp-stats.org/donate/");
#endif
}


void MainWindow::showAnalysis()
{
	_ribbonModel->showStatistics();
	emit hideDataPanel();
	_analyses->setVisible(true);
}

bool MainWindow::checkDoSync()
{
	//Only do this if we are *not* running in reporting mode. 
	if (!_reporter && checkAutomaticSync() && !MessageForwarder::showYesNo(tr("Datafile changed"), tr("The datafile that was used by this JASP file was modified. Do you want to reload the analyses with this new data?")))
	{
		setCheckAutomaticSync(false);
		//DataSetPackage::pkg()->setSynchingExternally(false);
		return false;
	}

	return true;
}

void MainWindow::startOnlineDataManager()
{
	_loader->moveToThread(&_loaderThread);
	_loaderThread.start();
	_loader->setOnlineDataManager(_odm);

	_fileMenu->setOnlineDataManager(_odm);

}

Q_DECLARE_METATYPE(columnType)

void MainWindow::makeConnections()
{
	connect(this,					&MainWindow::saveJaspFile,							this,					&MainWindow::saveJaspFileHandler,							Qt::QueuedConnection);
	connect(this,					&MainWindow::editImageCancelled,					_resultsJsInterface,	&ResultsJsInterface::cancelImageEdit						);
	connect(this,					&MainWindow::screenPPIChanged,						_preferences,			&PreferencesModel::setDefaultPPI							);
	connect(this,					&MainWindow::dataAvailableChanged,					_dynamicModules,		&DynamicModules::setDataLoaded								);
	connect(this,					&MainWindow::dataAvailableChanged,					_ribbonModel,			&RibbonModel::dataLoadedChanged								);
	connect(this,					&MainWindow::dataAvailableChanged,					this,					&MainWindow::checkEmptyWorkspace							);
	connect(this,					&MainWindow::analysesAvailableChanged,				this,					&MainWindow::checkEmptyWorkspace							);
	connect(this,					&MainWindow::resetVariableTypes,					_package,				&DataSetPackage::resetVariableTypes							);

	//connect(_package,				&DataSetPackage::synchingExternallyChanged,			_ribbonModel,			&RibbonModel::synchronisationChanged						);
	connect(_package,				&DataSetPackage::datasetChanged,					_columnsModel,			&ColumnsModel::datasetChanged								);
	connect(_package,				&DataSetPackage::isModifiedChanged,					this,					&MainWindow::packageChanged									);
	connect(_package,				&DataSetPackage::workspaceChanged,					this,					&MainWindow::onWorkspaceChanged								);
	connect(_package,				&DataSetPackage::isModifiedChanged,					_fileMenu,				&FileMenu::workspaceModified								);
	connect(_package,				&DataSetPackage::windowTitleChanged,				this,					&MainWindow::windowTitleChanged								);
	connect(_package,				&DataSetPackage::checkDoSync,						_loader,				&AsyncLoader::checkDoSync,									Qt::DirectConnection); //Force DirectConnection because the signal is called from Importer which means it is running in AsyncLoaderThread...
	connect(_package,				&DataSetPackage::newDataLoaded,						this,					[this](){ populateUIfromDataSet(); }						); //Through a lambda because a default argument is not part of the type of &MainWindow::populateUIfromDataSet, so it cannot fill in for the argument the signal does not carry.
	connect(_package,				&DataSetPackage::newDataLoaded,						_fileMenu,				[&](){ _fileMenu->enableButtonsForOpenedWorkspace(); }		);
	connect(_package,				&DataSetPackage::dataModeChanged,					_analyses,				&Analyses::dataModeChanged									);
	connect(_package,				&DataSetPackage::dataModeChanged,					_engineSync,			&EngineSync::dataModeChanged								);
	connect(_package,				&DataSetPackage::dataModeChanged,					this,					&MainWindow::onDataModeChanged								);
	connect(_package,				&DataSetPackage::askUserForExternalDataFile,		this,					&MainWindow::startDataEditorHandler							);
	connect(_package,				&DataSetPackage::makeAnAutoSave,					this,					&MainWindow::saveTmpFileHandler								); 
	connect(_package,				&DataSetPackage::showWarning,						_msgForwarder,			&MessageForwarder::showWarningQML,							Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::workspaceEmptyValuesChanged,		_analyses,				&Analyses::refreshAllAnalyses								);
	connect(_package,				&DataSetPackage::refreshAllAnalyses,				_analyses,				&Analyses::refreshAllAnalysesOfFilter,						Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::sendFilter,						_engineSync,			&EngineSync::sendFilter										);
	connect(_package,				&DataSetPackage::sendFilterByName,					_engineSync,			&EngineSync::sendFilterByName								);
	connect(_package,				&DataSetPackage::shownDataSetChanged,				_datasetTableModel,		&DataSetTableModel::handleDataSetChange						);
	connect(_package,				&DataSetPackage::shownDataSetChanged,				this,					&MainWindow::updateShownFilterInQmlContext					);
	//Every dataset (not just the currently shown one) must trigger its own reload when it needs to sync.
	//Qt::UniqueConnection is required because wireDataSetSync is re-run on every shownDataSetChanged
	//(emitted from both setShownDataSet and refresh) and on dataSetCreated; without it the same
	//(sender, signal) -> loader connection would accumulate, fanning one logical sync out to many reloads.
	auto wireDataSetSync = [this](DataSet * ds) {
		if(ds)
			connect(ds, &DataSet::syncRequired, _loader, &AsyncLoader::onSyncRequired, static_cast<Qt::ConnectionType>(Qt::QueuedConnection | Qt::UniqueConnection));
	};
	//Datasets that already exist (e.g. loaded from a file/db before connections were made) and any created later.
	wireDataSetSync(_package->workspace() ? _package->workspace()->shownDataSet() : nullptr);
	for(DataSet * ds : _package->workspace() ? _package->workspace()->dataSets() : DataSets())
		wireDataSetSync(ds);
	connect(_package,				&DataSetPackage::shownDataSetChanged,				this,					[wireDataSetSync](DataSet * ds){ wireDataSetSync(ds); });
	connect(_package,				&DataSetPackage::dataSetCreated,					this,					[this](int dataSetId){
		DataSet * ds = _package->workspace() ? _package->workspace()->dataSetById(dataSetId) : nullptr;
		if(ds)
			connect(ds, &DataSet::syncRequired, _loader, &AsyncLoader::onSyncRequired, static_cast<Qt::ConnectionType>(Qt::QueuedConnection | Qt::UniqueConnection));
	});
	//The worker thread finishes the sync; route the completion back to the dataset's syncer on the main
	//thread (via a QueuedConnection, since syncCompleted is emitted from the loader worker) so its
	//re-entrancy guard (_isSyncing) is released exactly once for whichever dataset syncs.
	//A (non-sync) load added a dataset to the workspace on the worker thread; refresh the workspace
	//table model here, on the GUI thread, so views bound to it (dataset tabbuttons) pick it up.
	connect(_loader,				&AsyncLoader::dataSetsChanged,						this,					[this](){
	  if(_package->workspace())
		  _package->workspace()->refresh();
	}, Qt::QueuedConnection);

	connect(_loader,				&AsyncLoader::syncCompleted,						this,					[this](int dataSetId, bool success){
		Log::log() << "[MainWindow::syncCompleted] Received: dataSetId=" << dataSetId << ", success=" << success << std::endl;
		DataSet * ds = _package->workspace() ? _package->workspace()->dataSetById(dataSetId) : nullptr;
		Log::log() << "[MainWindow::syncCompleted] dataSetById returned: " << (ds ? QString::number(ds->id()) : "NULL") << std::endl;
		if(ds)
		{
		  Log::log() << "[MainWindow::syncCompleted] Calling setSyncingResult for datasetId=" << ds->id() << std::endl;
		  ds->syncer().setSyncingResult(success);
		  Log::log() << "[MainWindow::syncCompleted] setSyncingResult returned" << std::endl;
		}
	}, Qt::QueuedConnection);

	connect(_package,				&DataSetPackage::shownFilterChanged,				this,					&MainWindow::updateShownFilterInQmlContext					);
	connect(_package,				&DataSetPackage::shownFilterChanged,				_filterModel,			&FilterModel::filterChanged,								Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::filtersCountChanged,				_filterModel,			&FilterModel::filterDropDownListChanged						);
	connect(_package,				&DataSetPackage::runComputedColumn,					_engineSync,			&EngineSync::computeColumn,									Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::runComputedDataSet,				_engineSync,			&EngineSync::computeDataSet,								Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::checkForDependentAnalyses,			_analyses,				&Analyses::checkForDependentAnalyses);
	connect(_package,				&DataSetPackage::workspaceEmptyValuesChanged,		_datasetTableModel,		&DataSetTableModel::emptyValuesChanged			);

	connect(_engineSync,			&EngineSync::engineTerminated,						this,					&MainWindow::fatalError										);
	connect(_engineSync,			&EngineSync::refreshAllPlotsExcept,					_analyses,				&Analyses::refreshAllPlots									);
	connect(_engineSync,			&EngineSync::plotEditorRefresh,						_plotEditorModel,		&PlotEditorModel::refresh									);
	connect(_engineSync,			&EngineSync::checkDataSetForUpdates,				_package,				&DataSetPackage::checkDataSetForUpdates,					Qt::QueuedConnection);

	qRegisterMetaType<columnType>();
	qRegisterMetaType<ListModel*>();
	qRegisterMetaType<DbType>();
	qRegisterMetaType<PlotEditor::References::ReferenceType>();

	
	connect(_package,				&DataSetPackage::showAnalysis,						_analyses,				&Analyses::selectAnalysisById								);
	connect(_package,				&DataSetPackage::showAnalysis,						this,					&MainWindow::showAnalysis									);
	
			
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_columnModel,			&ColumnModel::languageChangedHandler,						Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLocaleChanged,				_resultsJsInterface,	&ResultsJsInterface::setLocale,								Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_allHelp,				&AllHelp::helpChanged,										Qt::QueuedConnection);

	connect(_resultsJsInterface,	&ResultsJsInterface::packageModified,				this,					&MainWindow::setPackageModified								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisChangedDownstream,		this,					&MainWindow::analysisChangedDownstreamHandler				);
	connect(_resultsJsInterface,	&ResultsJsInterface::saveTextToFile,				this,					&MainWindow::saveTextToFileHandler							);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSaveImage,				this,					&MainWindow::analysisSaveImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisResizeImage,			this,					&MainWindow::analysisEditImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::refreshAllAnalyses,			this,					&MainWindow::refreshKeyPressed								);
	connect(_resultsJsInterface,	&ResultsJsInterface::removeAllAnalyses,				this,					&MainWindow::removeAllAnalyses								);
	connect(_resultsJsInterface,	&ResultsJsInterface::openFileTab,					_fileMenu,				&FileMenu::showFileOpenMenu									);
	connect(_resultsJsInterface,	&ResultsJsInterface::removeAnalysisRequest,			_analyses,				&Analyses::removeAnalysisById								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSelected,				_analyses,				&Analyses::analysisIdSelectedInResults						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisUnselected,			_analyses,				&Analyses::analysesUnselectedInResults						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisTitleChangedInResults,	_analyses,				&Analyses::analysisTitleChangedInResults					);
	connect(_resultsJsInterface,	&ResultsJsInterface::duplicateAnalysis,				_analyses,				[this](int id){ _analyses->duplicateAnalysis(size_t(id)); });
	connect(_resultsJsInterface,	&ResultsJsInterface::showDependenciesInAnalysis,	_analyses,				&Analyses::showDependenciesInAnalysis						);
	connect(_resultsJsInterface,	&ResultsJsInterface::showPlotEditor,				_plotEditorModel,		&PlotEditorModel::showPlotEditor							);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsMetaChanged,			_analyses,				&Analyses::resultsMetaChanged								);
	connect(_resultsJsInterface,	&ResultsJsInterface::allUserDataChanged,			_analyses,				&Analyses::allUserDataChanged								);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsPageLoadedSignal,		_languageModel,			&LanguageModel::resultsPageLoaded,							Qt::QueuedConnection);
	connect(_resultsJsInterface,	&ResultsJsInterface::showRSyntaxInResults,			_analyses,				&Analyses::showRSyntaxInResults								);

	//Whatever the results page downloads by itself (the plotly menu offers to save a plot as png for
	//instance) is cancelled by webengine unless someone accepts it, so ask the user where to put it.
	//This is the default profile on purpose: the plot/img/jaspPersona scheme handlers are installed
	//there as well, so the results view cannot be given a profile of its own without losing those.
	connect(QQuickWebEngineProfile::defaultProfile(),	&QQuickWebEngineProfile::downloadRequested,	this,	&MainWindow::webEngineDownloadRequested						);

	connect(_columnModel,			&ColumnModel::columnNameForIndex,					_datasetTableModel,		&DataSetTableModel::columnName								);

	connect(_analyses,				&Analyses::countChanged,							this,					&MainWindow::analysesCountChangedHandler					);
	connect(_analyses,				&Analyses::analysisResultsChanged,					this,					&MainWindow::analysisResultsChangedHandler					);
	connect(_analyses,				&Analyses::analysisResultsChanged,					this,					&MainWindow::waitForAllAnalysesFinishedBeforeStartingEvent	);
	connect(_analyses,				&Analyses::analysisImageSaved,						this,					&MainWindow::analysisImageSavedHandler						);
	connect(_analyses,				&Analyses::emptyQMLCache,							this,					&MainWindow::resetQmlCache									);
	connect(_analyses,				&Analyses::analysisAdded,							this,					&MainWindow::analysisAdded									);
	connect(_analyses,				&Analyses::analysisAdded,							_fileMenu,				&FileMenu::analysisAdded									);
	connect(_analyses,				&Analyses::analysesExportResults,					_fileMenu,				&FileMenu::analysesExportResults							);
	connect(_analyses,				&Analyses::analysisStatusChanged,					_resultsJsInterface,	&ResultsJsInterface::setStatus								);
	connect(_analyses,              &Analyses::analysisTitleChanged,                    _resultsJsInterface,    &ResultsJsInterface::changeTitle							);
	connect(_analyses,              &Analyses::analysisDataSpecChanged,                 _resultsJsInterface,    &ResultsJsInterface::changeDataSpec							);
	connect(_analyses,				&Analyses::analysisOverwriteUserdata,				_resultsJsInterface,	&ResultsJsInterface::overwriteUserdata						);
	connect(_analyses,				&Analyses::showAnalysisInResults,					_resultsJsInterface,	&ResultsJsInterface::showAnalysis							);
	connect(_analyses,				&Analyses::unselectAnalysisInResults,				_resultsJsInterface,	&ResultsJsInterface::unselect								);
	connect(_analyses,				&Analyses::analysisImageEdited,						_resultsJsInterface,	&ResultsJsInterface::analysisImageEditedHandler				);
	connect(_analyses,				&Analyses::analysisRemoved,							_resultsJsInterface,	&ResultsJsInterface::removeAnalysis							);
	connect(_analyses,				&Analyses::setResultsMeta,							_resultsJsInterface,	&ResultsJsInterface::setResultsMeta							);
	connect(_analyses,				&Analyses::moveAnalyses,							_resultsJsInterface,	&ResultsJsInterface::moveAnalyses							);
	connect(_analyses,				&Analyses::developerMode,							_preferences,			&PreferencesModel::developerMode							);
	connect(_analyses,				&Analyses::somethingModified,						[&](){					if(_package) _package->setModified(true); }					);
	connect(_analyses,				&Analyses::analysisImageEdited,						_plotEditorModel,		&PlotEditorModel::updateOptions								);

	connect(_fileMenu,				&FileMenu::exportSelected,							_resultsJsInterface,	&ResultsJsInterface::exportSelected							);
	connect(_fileMenu,				&FileMenu::showAbout,								this,					&MainWindow::showAbout										);
	connect(_fileMenu,				&FileMenu::showContact,								this,					&MainWindow::showContact									);
	connect(_fileMenu,				&FileMenu::showCommunity,							this,					&MainWindow::showCommunity								);

	connect(_odm,					&OnlineDataManager::progress,						this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);

	connect(_loader,				&AsyncLoader::progress,								this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);
	connect(_loader,				&AsyncLoader::checkDoSync,							this,					&MainWindow::checkDoSync,									Qt::BlockingQueuedConnection);

	connect(_preferences,			&PreferencesModel::dataLabelNAChanged,				_package,				&DataSetPackage::refresh,									Qt::QueuedConnection);
	connect(_preferences,			&PreferencesModel::plotBackgroundChanged,			this,					&MainWindow::setImageBackgroundHandler						);
	connect(_preferences,			&PreferencesModel::plotPPIChanged,					this,					&MainWindow::plotPPIChangedHandler							);
	connect(_preferences,			&PreferencesModel::exactPValuesChanged,				_resultsJsInterface,	&ResultsJsInterface::setExactPValuesHandler					);
	connect(_preferences,			&PreferencesModel::normalizedNotationChanged,		_resultsJsInterface,	&ResultsJsInterface::setNormalizedNotationHandler			);
	connect(_preferences,			&PreferencesModel::showInteractiveDefaultChanged,	_resultsJsInterface,	&ResultsJsInterface::setShowInteractiveDefaultHandler		);
	connect(_preferences,			&PreferencesModel::fixedDecimalsChangedString,		_resultsJsInterface,	&ResultsJsInterface::setFixDecimalsHandler					);
	connect(_preferences,			&PreferencesModel::uiScaleChanged,					_resultsJsInterface,	&ResultsJsInterface::uiScaleChangedHandler					);
	connect(_preferences,			&PreferencesModel::developerModeChanged,			_analyses,				&Analyses::refreshAllAnalyses								);
	connect(_preferences,			&PreferencesModel::currentJaspThemeChanged,			this,					&MainWindow::setCurrentJaspTheme							);
	connect(_preferences,			&PreferencesModel::currentThemeNameChanged,			_resultsJsInterface,	&ResultsJsInterface::setThemeCss							);
	connect(_preferences,			&PreferencesModel::currentThemeNameChanged,			_fileMenu,				&FileMenu::refresh											);
	connect(_preferences,			&PreferencesModel::uiScaleChanged,					_fileMenu,				&FileMenu::refresh											);
	connect(_preferences,			&PreferencesModel::resultFontChanged,				_resultsJsInterface,	&ResultsJsInterface::setFontFamily							);
	connect(_preferences,			&PreferencesModel::resultFontChanged,				_engineSync,			&EngineSync::refreshAllPlots								);
	connect(_preferences,			&PreferencesModel::restartAllEngines,				_engineSync,			&EngineSync::haveYouTriedTurningItOffAndOnAgain				);
	connect(_preferences,			&PreferencesModel::developerFolderChanged,			_dynamicModules,		&DynamicModules::uninstallJASPDeveloperModule				);
	connect(_preferences,			&PreferencesModel::showRSyntaxInResultsChanged,		_analyses,				&Analyses::showRSyntaxInResults								);
	connect(_preferences,			&PreferencesModel::ALTNavModeActiveChanged,			ALTNavControl::ctrl(),	&ALTNavControl::enableAlTNavigation							);
	connect(_preferences,			&PreferencesModel::remoteConfigurationChanged,		_jaspConfiguration,		&JASPConfiguration::remoteChanged							);
	connect(_preferences,			&PreferencesModel::remoteConfigurationURLChanged,	_jaspConfiguration,		&JASPConfiguration::remoteChanged							);
	connect(_preferences,			&PreferencesModel::useConfigurationFileChanged,		_jaspConfiguration,		&JASPConfiguration::processConfiguration					);
	connect(_preferences,			&PreferencesModel::orderByValueByDefaultChanged,	[&](){	Column::setAutoSortByValuesByDefault(PreferencesModel::prefs()->orderByValueByDefault()); });

	Column::setAutoSortByValuesByDefault(PreferencesModel::prefs()->orderByValueByDefault());
	
	auto * dCSingleton = DesktopCommunicator::singleton();

	//Needed to allow for a hard split between Desktop/QMLComps:
	connect(_preferences,			&PreferencesModel::uiScaleChanged,					dCSingleton,			&DesktopCommunicator::uiScaleChanged			);
	connect(_preferences,			&PreferencesModel::interfaceFontChanged,			dCSingleton,			&DesktopCommunicator::interfaceFontChanged		);
	connect(_preferences,			&PreferencesModel::currentJaspThemeChanged,			dCSingleton,			&DesktopCommunicator::currentJaspThemeChanged	);
	connect(dCSingleton,			&DesktopCommunicator::useNativeFileDialogSignal,	_preferences,			&PreferencesModel::useNativeFileDialog			);
	connect(dCSingleton,			&DesktopCommunicator::engineSandboxSignal,			_preferences,			&PreferencesModel::engineSandbox				);
	connect(dCSingleton,			&DesktopCommunicator::queryEncryptionSettingsSignal, _encryptionModel,		&EncryptionSettingsModel::queryEncryptionSettings);
	connect(_encryptionModel,		&EncryptionSettingsModel::queryComplete,			dCSingleton,			&DesktopCommunicator::encryptionSettingsQueryComplete);
	connect(dCSingleton,			&DesktopCommunicator::askCsvDelimiterSignal,		_csvPreviewModel,		&CsvPreviewModel::preparePreview);

	connect(_ribbonModel,			&RibbonModel::analysisClickedSignal,				_analyses,				&Analyses::analysisClickedHandler							);
	connect(_ribbonModel,			&RibbonModel::showRCommander,						this,					&MainWindow::showRCommander									);
	connect(_ribbonModel,			&RibbonModel::dataModeChanged,						_package,				&DataSetPackage::dataModeChanged							);
	//connect(_ribbonModel,			&RibbonModel::setDataSynchronisation,				_package,				&DataSetPackage::setSynchingExternallyFriendly				);

	connect(_dynamicModules,		&DynamicModules::dynamicModuleUnloadBegin,			_analyses,				&Analyses::removeAnalysesOfDynamicModule					);
	connect(_dynamicModules,		&DynamicModules::dynamicModuleChanged,				_analyses,				&Analyses::refreshAnalysesOfDynamicModule						);
	connect(_dynamicModules,		&DynamicModules::dynamicModuleQmlChanged,			_analyses,				&Analyses::reloadQmlAnalysesDynamicModule						);
	connect(_dynamicModules,		&DynamicModules::dynamicModuleReplaced,				_analyses,				&Analyses::replaceAnalysesOfDynamicModule,					Qt::DirectConnection);
	connect(_dynamicModules,		&DynamicModules::descriptionReloaded,				_analyses,				&Analyses::rescanAnalysisEntriesOfDynamicModule,			Qt::QueuedConnection);
	connect(_dynamicModules,		&DynamicModules::reloadHelpPage,					_helpModel,				&HelpModel::reloadPage										);
	connect(_dynamicModules,		&DynamicModules::moduleEnabledChanged,				_preferences,			&PreferencesModel::moduleEnabledChanged						);
	connect(_dynamicModules,		&DynamicModules::loadModuleTranslationFile,			_languageModel,			&LanguageModel::loadModuleTranslationFiles					);
	connect(_dynamicModules,		&DynamicModules::reloadQmlImportPaths,				this,					&MainWindow::setQmlImportPaths,								Qt::QueuedConnection); //If this is queued this should make the loadingprocess of qml a bit less weird I think.
	connect(_dynamicModules,		&DynamicModules::dynamicModuleUnloadBegin,			_engineSync,			&EngineSync::killModuleEngine								);
	connect(_dynamicModules,		&DynamicModules::isModuleInstallRequestActive,		_engineSync,			&EngineSync::isModuleInstallRequestActive					);
	
	connect(_dynamicModules,		&DynamicModules::storeAnalysesJson,					_analyses,				&Analyses::saveAnalysesJsonForReload						);
	connect(_dynamicModules,		&DynamicModules::reloadAnalysesJson,				_analyses,				&Analyses::reloadSavedAnalysesJson,							Qt::QueuedConnection);

	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_fileMenu,				&FileMenu::refresh											);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_csvPreviewModel,		&CsvPreviewModel::updateLocale,							Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::aboutToChangeLanguage,				_analyses,				&Analyses::prepareForLanguageChange							);
	connect(_languageModel,			&LanguageModel::aboutToChangeLanguage,				_package,				&DataSetPackage::prepareForLanguageChange					);
	connect(_languageModel,			&LanguageModel::languageChangeDone,					_package,				&DataSetPackage::languageChangeDone							);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_analyses,				&Analyses::languageChangedHandler,							Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_helpModel,				&HelpModel::generateJavascript,								Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				this,					&MainWindow::contactTextChanged,							Qt::QueuedConnection); //Probably not necessary but we can check once there actually are translations
	connect(_languageModel,			&LanguageModel::stopEngines,						_engineSync,			&EngineSync::stopEngines									);
	connect(_languageModel,			&LanguageModel::resumeEngines,						_engineSync,			&EngineSync::resumeEngines,									Qt::QueuedConnection);

	connect(_qml,					&QQmlApplicationEngine::warnings,					this,					&MainWindow::printQmlWarnings								);

	connect(_plotEditorModel,		&PlotEditorModel::saveImage,						this,					&MainWindow::analysisSaveImageHandler						);
	connect(_jaspConfiguration,		&JASPConfiguration::configurationProcessed,			this,					&MainWindow::loadModulesFromUserConfiguration				);
}

void MainWindow::onWorkspaceChanged()
{
	_qml->rootContext()->setContextProperty("workspace", Workspace::singleton());
}


void MainWindow::printQmlWarnings(const QList<QQmlError> &warnings)
{
	Log::log()		<< "Received QML warnings:\n";
	for(const QQmlError & warning : warnings)
		Log::log(false)	<< "\t" << warning.toString() << "\n";
	Log::log(false) << std::endl;
}

void MainWindow::updateShownFilterInQmlContext()
{
	_qml->rootContext()->setContextProperty("shownFilter",								DataSetPackage::pkg()->filter()					);	
}


void MainWindow::loadQML()
{
	Log::log() << "Initializing QML" << std::endl;

	_qml->rootContext()->setContextProperty("mainWindow",								this											);
	_qml->rootContext()->setContextProperty("columnModel",								_columnModel									);
	_qml->rootContext()->setContextProperty("aboutModel",								_aboutModel										);
	_qml->rootContext()->setContextProperty("encryptionModel",							_encryptionModel								);
	_qml->rootContext()->setContextProperty("dataSetModel",								_datasetTableModel								);
	_qml->rootContext()->setContextProperty("columnsModel",								_columnsModel									);
	_qml->rootContext()->setContextProperty("workspaceModel",							_workspaceModel									);
	_qml->rootContext()->setContextProperty("analysesModel",							_analyses										);
	_qml->rootContext()->setContextProperty("dynamicModules",							_dynamicModules									);
	_qml->rootContext()->setContextProperty("plotEditorModel",							_plotEditorModel								);
	_qml->rootContext()->setContextProperty("preferencesModel",							_preferences									);
	_qml->rootContext()->setContextProperty("resultsJsInterface",						_resultsJsInterface								);
	_qml->rootContext()->setContextProperty("aiBridge",									_aiBridge										);
	_qml->rootContext()->setContextProperty("aiConfigModel",							_aiConfigModel									);
	_qml->rootContext()->setContextProperty("messages",									_msgForwarder									);
	_qml->rootContext()->setContextProperty("dataSetPackage",							DataSetPackage::pkg()							);
	_qml->rootContext()->setContextProperty("ribbonModelFiltered",						_ribbonModelFiltered							);
	_qml->rootContext()->setContextProperty("windowsCodePagesHelper",					_windowsWorkaroundCPs							); //is nullptr on not-windows!
	_qml->rootContext()->setContextProperty("ribbonModelUncommon",						_ribbonModelUncommon							);
	_qml->rootContext()->setContextProperty("columnTypesModel",							_columnTypesModel								);
	_qml->rootContext()->setContextProperty("resultMenuModel",							_resultMenuModel								);
	_qml->rootContext()->setContextProperty("fileMenuModel",							_fileMenu										);
	_qml->rootContext()->setContextProperty("filterModel",								_filterModel									);
	_qml->rootContext()->setContextProperty("shownFilter",								DataSetPackage::pkg()->filter()					);
	_qml->rootContext()->setContextProperty("ribbonModel",								_ribbonModel									);
	_qml->rootContext()->setContextProperty("engineSync",								_engineSync										);
	_qml->rootContext()->setContextProperty("helpModel",								_helpModel										);
	_qml->rootContext()->setContextProperty("allHelp",									_allHelp										);
	_qml->rootContext()->setContextProperty("jaspTheme",								nullptr											); //Will be set from jaspThemeChanged()!
	_qml->rootContext()->setContextProperty("qmlUtils",									new QmlUtils(this)								);

	_qml->rootContext()->setContextProperty("baseBlockDim",								20												); //should be taken from Theme
	_qml->rootContext()->setContextProperty("baseFontSize",								16												);
	_qml->rootContext()->setContextProperty("languageModel",							_languageModel									);
	_qml->rootContext()->setContextProperty("jaspTmpDir",                               tq(Dirs::tempDir())      						);

	_qml->rootContext()->setContextProperty("columnTypeScale",							int(columnType::scale)							);
	_qml->rootContext()->setContextProperty("columnTypeOrdinal",						int(columnType::ordinal)						);
	_qml->rootContext()->setContextProperty("columnTypeNominal",						int(columnType::nominal)						);
	_qml->rootContext()->setContextProperty("columnTypeUnknown",						int(columnType::unknown)						);
	_qml->rootContext()->setContextProperty("columnTypeNominalText",					int(columnType::nominalText)					);
	
	_qml->rootContext()->setContextProperty("computedColumnTypeRCode",					int(computedColumnType::rCode)					);
	_qml->rootContext()->setContextProperty("computedColumnTypeAnalysis",				int(computedColumnType::analysis)				);
	_qml->rootContext()->setContextProperty("computedColumnTypeNotComputed",			int(computedColumnType::notComputed)			);
	_qml->rootContext()->setContextProperty("computedColumnTypeConstructorCode",		int(computedColumnType::constructorCode)		);
	_qml->rootContext()->setContextProperty("computedColumnTypeAnalysisNotComputed",	int(computedColumnType::analysisNotComputed)	);
	_qml->rootContext()->setContextProperty("moduleLibrary",							_moduleLibrary									);
	_qml->rootContext()->setContextProperty("csvPreviewModel",							_csvPreviewModel								);

	_qml->setOutputWarningsToStandardError(true);

	setQmlImportPaths();

	QMetaObject::Connection exitOnFailConnection = connect(_qml, &QQmlApplicationEngine::objectCreated, [&](QObject * obj, QUrl url)
	{
		if(obj == nullptr)
		{
			std::cerr << "Could not load QML: " + url.toString().toStdString() << std::endl;
			emit exitSignal(10);
		}
		else
			Log::log() << "QML loaded, url: '" << url.toString() << "' and obj name: '" << obj->objectName() << "'" << std::endl;
	});

	Log::log() << "Loading Themes" << std::endl;

	// load chosen theme first
	if(_preferences->currentThemeName() == "lightTheme")
	{
		_qml->load(QUrl("qrc:///components/JASP/Theme/Theme.qml"));
		_qml->load(QUrl("qrc:///components/JASP/Theme/DarkTheme.qml"));
	}
	else
	{
		_qml->load(QUrl("qrc:///components/JASP/Theme/DarkTheme.qml"));
		_qml->load(QUrl("qrc:///components/JASP/Theme/Theme.qml"));
	}

	setCurrentJaspTheme();

	JaspTheme::initializeUIScales();

	for(const auto & keyval : JaspTheme::themes())
	{
		connect(keyval.second,		&JaspTheme::currentThemeNameChanged,			_preferences,		&PreferencesModel::currentThemeNameHandler	);
		connect(keyval.second,		&JaspTheme::currentThemeReady,					_preferences,		&PreferencesModel::currentThemeReady		);
		connect(_preferences,		&PreferencesModel::uiScaleChanged,				keyval.second,		&JaspTheme::uiScaleHandler					);
		connect(_preferences,		&PreferencesModel::maxFlickVelocityChanged, 	keyval.second,		&JaspTheme::maxFlickVeloHandler				);
	}

	_fileMenu->refresh(); //Now that the theme is loaded we can determine the proper width for the buttons in the filemenu


    Log::log() << "Loading AIChatWindow"					<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/ChatWindow.qml"));

	// Find the ChatWindow and track its active state for the toggle button
	for (QObject* obj : _qml->rootObjects())
	{
		if (obj->objectName() == "chatWindow")
		{
			_chatWindow = qobject_cast<QWindow*>(obj);
			if (_chatWindow)
				{
					connect(_chatWindow, &QWindow::activeChanged, this, &MainWindow::checkChatWindowActive);
					Log::log() << "ChatWindow found and connected." << std::endl;
				}
			break;
		}
	}

    Log::log() << "Loading HelpWindow"					<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/HelpWindow.qml"));
	Log::log() << "Loading AboutWindow"					<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/AboutWindow.qml"));
	Log::log() << "Loading ContactWindow"				<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/ContactWindow.qml"));
	Log::log() << "Loading CommunityWindow"				<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/CommunityWindow.qml"));
	Log::log() << "Loading EncryptionSettingsWindow"	<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/EncryptionSettingsWindow.qml"));
	Log::log() << "Loading CSV Preview"				<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/CsvPreview.qml"));
	Log::log() << "Loading MainWindow"					<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/MainWindow.qml"));

	if(!DataSetView::mainDataViewer())
		throw std::runtime_error("The main data viewer did not load, without which JASP cannot run.");
	
	//To make sure we connect to the "main datasetview":
	connect(_preferences, &PreferencesModel::uiScaleChanged,			DataSetView::mainDataViewer(),	&DataSetView::viewportChangedDelayed);
	connect(_preferences, &PreferencesModel::interfaceFontChanged,		DataSetView::mainDataViewer(),	&DataSetView::viewportChangedDelayed);
	connect(_ribbonModel, &RibbonModel::dataInsertComputedColumnBefore,	DataSetView::mainDataViewer(),	&DataSetView::columnComputedInsertBefore);
	connect(_ribbonModel, &RibbonModel::dataInsertComputedColumnAfter,	DataSetView::mainDataViewer(),	&DataSetView::columnComputedInsertAfter);
	connect(_ribbonModel, &RibbonModel::dataInsertColumnBefore,			DataSetView::mainDataViewer(),	&DataSetView::columnInsertBefore);
	connect(_ribbonModel, &RibbonModel::dataInsertColumnAfter,			DataSetView::mainDataViewer(),	&DataSetView::columnInsertAfter);
	connect(_ribbonModel, &RibbonModel::finishCurrentEdit,				DataSetView::mainDataViewer(),	&DataSetView::commitLastEdit);
	connect(_ribbonModel, &RibbonModel::dataInsertRowBefore,			DataSetView::mainDataViewer(),	&DataSetView::rowInsertBefore);
	connect(_ribbonModel, &RibbonModel::dataInsertRowAfter,				DataSetView::mainDataViewer(),	&DataSetView::rowInsertAfter);
	connect(_ribbonModel, &RibbonModel::dataRemoveColumn,				DataSetView::mainDataViewer(),	&DataSetView::columnsDeleteSelected);
	connect(_ribbonModel, &RibbonModel::dataRemoveRow,					DataSetView::mainDataViewer(),	&DataSetView::rowsDeleteSelected);
	connect(_ribbonModel, &RibbonModel::cellsClear,						DataSetView::mainDataViewer(),	&DataSetView::cellsClear);
	connect(_ribbonModel, &RibbonModel::dataUndo,						DataSetView::mainDataViewer(),	&DataSetView::undo);
	connect(_ribbonModel, &RibbonModel::dataRedo,						DataSetView::mainDataViewer(),	&DataSetView::redo);
	connect(this,		  &MainWindow::resizeData,						DataSetView::mainDataViewer(),	&DataSetView::resizeData);
	connect(_ribbonModel, &RibbonModel::showNewData,					this,							&MainWindow::showNewData);
	connect(_ribbonModel, &RibbonModel::addNewDataSet,					this,							&MainWindow::addNewDataSet);
	

	//connect(DataSetView::lastInstancedDataSetView(), &DataSetView::selectionStartChanged,	_columnModel,	&ColumnModel::changeSelectedColumn);

	Log::log() << "QML Initialized!"  << std::endl;

	//And now we disconnect the exit on fail lambda because we won't be needing it later
	disconnect(exitOnFailConnection);

	//Load the ribbonmodel modules now because we have an actual qml context to do so in.
	_ribbonModel->loadModules(InstalledModules::getModules());
	
	qmlLoaded();	
}


void MainWindow::showEnginesWindow()
{
	Log::log() << "Showing EnginesWindow"  << std::endl;
	_qml->load(QUrl("qrc:///components/JASP/Widgets/EnginesWindow.qml"));
}

void MainWindow::setDefaultWorkspaceEmptyValues()
{
	DataSetPackage::pkg()->setDefaultWorkspaceEmptyValues();
}

void MainWindow::toggleChat()
{
	if (!PreferencesModel::prefs()->aiEnabled()) return;
	// Find the ChatWindow if not already cached (handles edge case of late load)
	if (!_chatWindow)
	{
		for (QObject* obj : _qml->rootObjects())
		{
			if (obj->objectName() == "chatWindow")
			{
				_chatWindow = qobject_cast<QWindow*>(obj);
				if (_chatWindow)
					connect(_chatWindow, &QWindow::activeChanged, this, &MainWindow::checkChatWindowActive);
				break;
			}
		}
	}

	if (!_chatWindow)
	{
		// Fallback: if chat window not found, just toggle visibility
		setAiChatVisible(!_aiChatVisible);
		return;
	}

	if (!_chatWindow->isVisible())
	{
		// Chat is hidden → show it and bring to front
		setAiChatVisible(true);
		_chatWindow->raise();
		_chatWindow->requestActivate();
	}
	else if (_chatWindow->visibility() == QWindow::Minimized)
	{
		// Chat is minimized → restore and bring to front
		_chatWindow->setVisibility(QWindow::Windowed);
		_chatWindow->raise();
		_chatWindow->requestActivate();
	}
	else if (!_chatWindow->isActive())
	{
		// Chat is visible but behind other windows → bring to front
		_chatWindow->raise();
		_chatWindow->requestActivate();
	}
	else
	{
		// Chat is visible and in front → hide it
		setAiChatVisible(false);
	}
}

void MainWindow::checkChatWindowActive()
{
	bool active = _chatWindow && _chatWindow->isActive();
	if (_chatWindowActive != active)
	{
		_chatWindowActive = active;
		emit chatWindowActiveChanged();
	}
}

void MainWindow::annotateAnalysis()
{
	if (!PreferencesModel::prefs()->aiEnabled()) return;

	AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
	if (!pm->activePersonaAllowAnnotation())
		return;

	// Find the ChatWindow if not already cached
	if (!_chatWindow)
	{
		for (QObject* obj : _qml->rootObjects())
		{
			if (obj->objectName() == "chatWindow")
			{
				_chatWindow = qobject_cast<QWindow*>(obj);
				if (_chatWindow)
					connect(_chatWindow, &QWindow::activeChanged, this, &MainWindow::checkChatWindowActive);
				break;
			}
		}
	}

	if (!_chatWindow) return;

	// Always open and bring to front (never close)
	setAiChatVisible(true);
	if (_chatWindow->visibility() == QWindow::Minimized)
		_chatWindow->setVisibility(QWindow::Windowed);
	_chatWindow->raise();
	_chatWindow->requestActivate();

	// Always use the stored annotation prompt (default or custom)
	QString prompt = PreferencesModel::prefs()->aiAnnotationPrompt();


	// Delegate to the QML ChatWindow's submit function
	QMetaObject::invokeMethod(_chatWindow.data(), "submitUserMessage",
		Qt::QueuedConnection, Q_ARG(QVariant, QVariant(prompt)));
}

void MainWindow::setQmlImportPaths()
{
	static QStringList originalImportPaths = _qml->importPathList();

	QStringList newImportPaths = originalImportPaths;

	newImportPaths.append(":/jasp-stats.org/imports");
	newImportPaths.append("qrc:///components");
	newImportPaths.append(_dynamicModules->importPaths());

	if(_qml->importPathList() == newImportPaths)
		return;

	_qml->setImportPathList(newImportPaths);

	if(_preferences->developerMode())
	{
		
		QString importLog = "QML has the following import paths:" + _qml->importPathList().join("\n\t") + "\n";
		static QString previous;
		
		if(previous != importLog)
		{
			Log::log() << importLog << std::endl;
			previous = importLog;
		}
	}
}

QObject * MainWindow::loadQmlData(QString data, QUrl url)
{
	QObject *	createdObject = nullptr;
	bool		lambdaCalled = false;

	QMetaObject::Connection returnTheObjectConn = connect(_qml, &QQmlApplicationEngine::objectCreated, [&](QObject * obj, QUrl url)
	{
			//ignore the warnings about the out of scope thing, the lambda is disconnected in this function itself and crashes it on purpose if it didn't et a response by then.
			createdObject = obj;
			lambdaCalled = true;
	});

	Log::log() << "Loading QML data from url '" << url.toString() << "'" << std::endl;
	_qml->loadData(data.toUtf8(), url);

	//The lambda is called now and createdObject filled

	disconnect(returnTheObjectConn);

	if(lambdaCalled)
		return createdObject;

	throw std::runtime_error("loadQmlData did not get a response from the lambda on time!");

}

void MainWindow::showRCommander()
{
	if(RCommander::opened())
	{
		Log::log() << "RCommander already loaded, making it active now." << std::endl;
		RCommander::makeActive();
	}
	else
	{
		Log::log() << "Loading RCommander"  << std::endl;
		_qml		-> load(QUrl("qrc:///components/JASP/Widgets/RCommanderWindow.qml"));

		//To reload page because of https://github.com/jasp-stats/INTERNAL-jasp/issues/1280
		reloadResults();
	}
}

void MainWindow::reloadResults() const
{
	_resultsJsInterface->resetResults();//To reload page

	QTimer::singleShot(500, this, &MainWindow::resendResultsToWebEngine);

}

void MainWindow::resendResultsToWebEngine()
{
	//Make sure the result are reloaded after triggering a qml wipe
	_analyses	-> applyToAll([&](Analysis * a){ emit a->resultsChangedSignal(a); });
}

void MainWindow::setCurrentJaspTheme()
{
	_qml->rootContext()->setContextProperty("jaspTheme", JaspTheme::currentTheme());
}

void MainWindow::onDataModeChanged(bool dataMode)
{
	if(dataMode && welcomePageVisible())
		setWelcomePageVisible(false);
}

void MainWindow::initLog()
{
	assert(_engineSync != nullptr && _preferences != nullptr);

	static boost::iostreams::stream<boost::iostreams::null_sink> nullstream((boost::iostreams::null_sink())); //https://stackoverflow.com/questions/8243743/is-there-a-null-stdostream-implementation-in-c-or-libraries

	Log::logFileNameBase = (AppDirs::logDir() + "JASP "  + getSortableTimestamp()).toStdString();
	Log::init(&nullstream);
	Log::setLogFileName(Log::logFileNameBase + " Desktop.log");
	Log::setLoggingToFile(_preferences->logToFile());
	logRemoveSuperfluousFiles(_preferences->logFilesMax());

	connect(_preferences, &PreferencesModel::logToFileChanged,		this,			&MainWindow::logToFileChanged									); //Not connecting preferences directly to Log to keep it Qt-free (for Engine/R-Interface)
	connect(_preferences, &PreferencesModel::logToFileChanged,		_engineSync,	&EngineSync::logToFileChanged,			Qt::QueuedConnection	);
	connect(_preferences, &PreferencesModel::logFilesMaxChanged,	this,			&MainWindow::logRemoveSuperfluousFiles							);
}

void MainWindow::logToFileChanged(bool logToFile)
{
	Log::setLoggingToFile(logToFile);
}

void MainWindow::logRemoveSuperfluousFiles(int maxFilesToKeep)
{
	QDir logFileDir(AppDirs::logDir());

	QFileInfoList logs = logFileDir.entryInfoList({"*.log"}, QDir::Filter::Files, QDir::SortFlag::Name | QDir::SortFlag::Reversed);

	if(logs.size() < maxFilesToKeep)
		return;

	for(int i=logs.size() - 1; i >= maxFilesToKeep; i--)
		logFileDir.remove(logs[i].fileName());
}

void MainWindow::openFolderExternally(QDir folder) const
{
	QDesktopServices::openUrl(QUrl::fromLocalFile(folder.absolutePath()));
}

void MainWindow::showLogFolder() const
{
	openFolderExternally(AppDirs::logDir());
}

bool MainWindow::openURLFile(QString fileURLPath)
{
	QUrl fileUrl = fileURLPath.startsWith("file:") ? QUrl(fileURLPath) : QUrl::fromLocalFile(fileURLPath);
	if (!fileUrl.isLocalFile())
	{
		MessageForwarder::showWarning(tr("Open file"), tr("Cannot access file %1").arg(fileURLPath));
        return false;
	}

	QString filePath = fileUrl.toLocalFile();
	QFileInfo fileInfo(filePath);

	if (!fileInfo.exists())
	{
		MessageForwarder::showWarning(tr("Open file"), tr("File %1 is not found.").arg(filePath));
        return false;
	}

    Utils::FileType fileType = Utils::getTypeFromFileName(fq(filePath));

    if (fileType == Utils::FileType::empty || fileType == Utils::FileType::unknown || fileType == Utils::FileType::html ||  fileType == Utils::FileType::pdf)
	{
		MessageForwarder::showWarning(tr("Open file"), tr("JASP does not support this file type %1.").arg(filePath));
        return false;
	}

	if (_preferences->syncDroppedDatafile() && _package->hasDataSet() && fileType != Utils::FileType::jasp)
	{
		FileEvent * syncEvent = new FileEvent(this, FileEvent::FileSyncData);
		syncEvent->setPath(filePath);
		syncEvent->setDataSet(_package->dataSet());

		syncEvent->starts();
	}
	else
        open(filePath);

    return true;
}

void MainWindow::open(const QString & mainFilePath, const QString & inputDataFile, const QString & outputFile, bool keepJASPOpen)
{
	if(resultXmlCompare::compareResults::theOne()->testMode())
		resultXmlCompare::compareResults::theOne()->setFilePath(mainFilePath);

	_openedUsingArgs = true;
	if (_resultsJsInterface->resultsLoaded())
		_open(mainFilePath, inputDataFile, outputFile, keepJASPOpen);
	else
		connect(_resultsJsInterface, &ResultsJsInterface::resultsPageLoadedSignal, this, [this, mainFilePath, inputDataFile, outputFile, keepJASPOpen](){_open(mainFilePath, inputDataFile, outputFile, keepJASPOpen); }, Qt::SingleShotConnection);
}

void MainWindow::_open(const QString & mainFilePath, const QString & inputDataFile, const QString & outputFile, bool keepJASPOpen)
{
	FileEvent * openEvent = _fileMenu->open(mainFilePath);
	if (!inputDataFile.isEmpty() && _package->hasDataSet())
	{
		FileEvent * syncEvent = new FileEvent(this, FileEvent::FileSyncData);
		syncEvent->setPath(inputDataFile);
		syncEvent->chain(openEvent, true);

		// Once the synchronization is finalized, decide what to do based on whether it succeeded:
		//  - failed sync:             do not export, and exit with a non-zero code (unless we keep JASP open).
		//  - success, no output file: we are done, exit with success.
		//  - success, with output:    export the results, but only after all analyses have refreshed.
		connect(syncEvent, &FileEvent::finalized, this, [this, syncEvent, outputFile, keepJASPOpen]()
		{
			if (!syncEvent->isSuccessful())
			{
				if (!keepJASPOpen)
					emit exitSignal(1);
				return;
			}

			if (outputFile.isEmpty())
			{
				if (!keepJASPOpen)
					emit exitSignal(0);
				return;
			}

			FileEvent * exportEvent = new FileEvent(this, FileEvent::FileExportResults);
			exportEvent->setPath(outputFile);

			if (!keepJASPOpen)
				connect(exportEvent, &FileEvent::finalized, this, [this, exportEvent]() { emit exitSignal(exportEvent->isSuccessful() ? 0 : 1); });

			_waitingEvent = exportEvent;
			waitForAllAnalysesFinishedBeforeStartingEvent();
		} );
	}
}

void MainWindow::waitForAllAnalysesFinishedBeforeStartingEvent()
{
	if (!_waitingEvent || _waitingEvent->isStarted())
		return;

	if (_analyses->allFinished())
	{
		//Take ownership of the waiting event *before* doing any work: setErrorInResults() below ends up
		//emitting analysisResultsChanged, which re-enters this slot - the cleared _waitingEvent makes that
		//re-entry return at the guard above. Note we must NOT disconnect from analysisResultsChanged to
		//achieve that: this slot has to stay connected for any later event that waits on the analyses
		//(a second sync/export with --keepJASPOpen), which would otherwise wait forever.
		FileEvent * waitingEvent = _waitingEvent;
		_waitingEvent            = nullptr;

		_analyses->applyToAll([&](Analysis * a)
		{
			//An analysis whose form never got instantiated (a module that failed to load for instance)
			//has no error to report either, so leave it alone instead of dereferencing nothing.
			if (a->form() && a->form()->hasError())
				a->setErrorInResults(fq(tr("Validation error: %1").arg(a->form()->getError(true))));
		});

		waitingEvent->starts();
	}
}

///This function assumes there should afterwards be only 1 DataSet!
void MainWindow::showNewData()
{
	_ribbonModel->showData();
	_package->generateEmptyData();
}


void MainWindow::addNewDataSet()
{
	//createDataSet() (re)creates the workspace if it is null (e.g. right after a reset on a fresh
	//JASP), so obtain the new DataSet first and never dereference workspace() before it exists.
	DataSet * newSet = _package->createDataSet();

	_package->workspace()->setShownDataSet(newSet);

	newSet->setColumnCount(1);
	newSet->setRowCount(1, false);
	newSet->column(0)->initFromLookups(newSet->freeNewColumnName(0), 1, [](size_t){return "";}, [](size_t){return "";}, "", columnType::scale, {}, PreferencesModel::prefs()->thresholdScale(), PreferencesModel::prefs()->orderByValueByDefault());

}

void MainWindow::open(const Json::Value & dbJson)
{
	_openedUsingArgs = true;
	if (_resultsJsInterface->resultsLoaded())	_fileMenu->open(dbJson);
	else										_openOnLoadDbJson = dbJson;
}

void MainWindow::saveKeyPressed()
{
	if (_package->isModified()) _fileMenu->save();
}

void MainWindow::saveAsKeyPressed()
{
	if (_package->isLoaded()) _fileMenu->saveAs();
}

void MainWindow::openKeyPressed()
{
	_fileMenu->showFileOpenMenu();
}

void MainWindow::refreshKeyPressed()
{
	_analyses->refreshAllAnalyses();
}

void MainWindow::zoomInKeyPressed()
{
	_preferences->zoomIn();
}

void MainWindow::zoomOutKeyPressed()
{
	_preferences->zoomOut();
}

void MainWindow::zoomResetKeyPressed()
{
	_preferences->zoomReset();
}

void MainWindow::undo()
{
	if (UndoStack::singleton())
		UndoStack::singleton()->undo();
}

void MainWindow::redo()
{
	if (UndoStack::singleton())
		UndoStack::singleton()->redo();
}

void MainWindow::syncKeyPressed()
{
	_fileMenu->sync();
}

void MainWindow::packageChanged()
{
	emit windowTitleChanged();
}


void MainWindow::setImageBackgroundHandler(QString)
{
	refreshPlotsHandler();
}


void MainWindow::plotPPIChangedHandler(int, bool wasUserAction)
{
	refreshPlotsHandler(wasUserAction);
}

void MainWindow::refreshPlotsHandler(bool askUserForRefresh)
{
	if (_analyses->allFresh())
		_engineSync->refreshAllPlots();
	else if (askUserForRefresh && MessageForwarder::showYesNo(tr("Version incompatibility"), tr("Your analyses were created in an older version of JASP, to change the PPI of the images they must be refreshed first.\n\nRefresh all analyses?")))
		_analyses->refreshAllAnalyses();
}

void MainWindow::checkEmptyWorkspace()
{
	if (!analysesAvailable() && !dataAvailable())
	{
		if(DataSetPackage::pkg()->hasAnalysesWithoutData())
			_fileMenu->close();

	}
}

void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	static bool showInstructions = true;

	if (showInstructions)
	{
		if (Settings::value(Settings::INSTRUCTIONS_SHOWN).toBool() == false)
		{
			Settings::setValue(Settings::INSTRUCTIONS_SHOWN, true);
			_resultsJsInterface->showInstruction();
		}

		showInstructions = false;
	}

	_resultsJsInterface->setLocale(_languageModel->currentLocale().bcp47Name(), _languageModel->useThousandSeps());
	_resultsJsInterface->analysisChanged(analysis);

	setPackageModified();

	if(resultXmlCompare::compareResults::theOne()->testMode())
		analysesForComparingDoneAlready();
	
	if(_reporter && _analyses->allFinished())
		_reporter->analysesFinished();
}

void MainWindow::webEngineDownloadRequested(QQuickWebEngineDownloadRequest * download)
{
	if(!download)
		return;

	//Without a name of its own fall back on what the page suggested, and on something recognizable
	//after that, so the dialog never opens on an empty file name.
	QString suggested = download->downloadFileName();

	if(suggested.isEmpty())
		suggested = download->suggestedFileName();

	if(suggested.isEmpty())
		suggested = "download";

	QFileInfo	suggestedInfo(suggested);
	QString		extension	= suggestedInfo.suffix(),
				filter		= extension.isEmpty()
							? tr("All Files") + " (*)"
							: tr("%1 Files").arg(extension.toUpper()) + " (*." + extension + ");;" + tr("All Files") + " (*)",
				finalPath	= MessageForwarder::browseSaveFile(tr("Save File"), suggested, filter);

	if(finalPath.isEmpty())
	{
		download->cancel();
		return;
	}

	QFileInfo finalInfo(finalPath);

	//browseSaveFile hands back the whole path while the request wants it split, and it overwrites
	//itself, so remove whatever is there instead of letting webengine pick "file (1).png".
	if(finalInfo.exists())
		QFile::remove(finalPath);

	download->setDownloadDirectory(finalInfo.absolutePath());
	download->setDownloadFileName(finalInfo.fileName());
	download->accept();

	Log::log() << "Accepted a download from the webengine, storing it as '" << finalPath << "'" << std::endl;
}

void MainWindow::analysisSaveImageHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == nullptr)
		return;

	if (analysis->needsRefresh())
	{
		if(		analysis->storedWithoutState() 
			?	MessageForwarder::showYesNo(tr("Stored without state"), tr("This analysis was saved without state, to save the image it must be refreshed first.\n\nRefresh the analysis?"))
			:	MessageForwarder::showYesNo(tr("Version incompatibility"), tr("This analysis was created in an older version of JASP, to save the image it must be refreshed first.\n\nRefresh the analysis?"))
				
		)
			analysis->refresh();
	}
	else
		_analysisSaveImageHandler(analysis, options);
}

void MainWindow::_analysisSaveImageHandler(Analysis* analysis, QString options)
{
	Json::Value root;
	Json::Reader().parse(fq(options), root);

	QString selectedExtension,
			finalPath			= MessageForwarder::browseSaveFile(tr("Save JASP Image"), "", tr("Portable Network Graphics (*.png);;Portable Document Format (*.pdf);;Encapsulated PostScript (*.eps);;300 dpi Tagged Image File (*.tiff);;PowerPoint (*.pptx);;Scalable Vector Graphics (*.svg)"), &selectedExtension);

	if (!finalPath.isEmpty())
	{
		root["type"] = fq(selectedExtension);

		if(root["type"].asString() != "png")
		{
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(root);
		}
		else
		{
			QString imagePath = QString::fromStdString(TempFiles::sessionDirName()) + "/" + root.get("data", Json::nullValue).asCString();

			if (QFile::exists(finalPath))
				QFile::remove(finalPath);

			QFile::copy(imagePath, finalPath);
		}
	}
}


void MainWindow::analysisImageSavedHandler(Analysis *analysis)
{
	Json::Value results = analysis->imgResults();
	if (results.isNull())
		return;

	Json::Value inputOptions	= results.get("inputOptions", Json::nullValue);
	QString		imagePath		= QString::fromStdString(TempFiles::sessionDirName()) + "/" + results.get("name", Json::nullValue).asCString(),
				finalPath		= QString::fromStdString(inputOptions.get("finalPath", Json::nullValue).asCString());

	if (!finalPath.isEmpty())
	{
		Log::log() << "analysisImageSavedHandler, imagePath: " << imagePath.toStdString() << ", finalPath: " << finalPath.toStdString() << std::endl;

		if (QFile::exists(finalPath))
			QFile::remove(finalPath);
		QFile::copy(imagePath, finalPath);
	}
}

void MainWindow::analysisEditImageHandler(int id, QString options)
{

	Analysis *analysis = _analyses->get(id);
	if (analysis == nullptr)
		return;

	if (analysis->needsRefresh())
	{
		if (	analysis->storedWithoutState() 
			?	MessageForwarder::showYesNo(tr("Stored without state"), tr("This analysis was stored without state, to resize the image it must be refreshed first.\n\nRefresh the analysis?"))
			:	MessageForwarder::showYesNo(tr("Version incompatibility"), tr("This analysis was created in an older version of JASP, to resize the image it must be refreshed first.\n\nRefresh the analysis?"))
		)
			analysis->refresh();
		else
		{
			Json::Value opts;
			Json::Reader().parse(fq(options), opts);
			emit editImageCancelled(id, tq(opts.get("name", "").asString()));
		}
	}
	else
	{
		string utf8 = fq(options);
		Json::Value root;
		Json::Reader().parse(utf8, root);
		
		root[".meta"] = analysis->optionsMeta();
		
		analysis->editImage(root);
	}
}

void MainWindow::registerRpcHandlers()
{
	auto* disp = JaspRpcDispatcher::singleton();
	if (!disp)
		return;

	// Shared helper — builds the dataset metadata portion used by
	// data_load (on success), data_load_status (on complete), and data_info.
	auto resolveDataSet = [](const Json::Value& params) -> DataSet *
	{
		Workspace * ws = DataSetPackage::pkg() ? DataSetPackage::pkg()->workspace() : nullptr;
		if (!ws)
			return nullptr;

		if (params.isMember("dataSetId"))
			return ws->dataSetById(params["dataSetId"].asInt());

		return ws->shownDataSet();
	};

	auto buildDataInfo = [](DataSet * ds) -> Json::Value
	{
		Json::Value info;

		if (!ds)
		{
			info["loaded"] = false;
			return info;
		}

		info["loaded"]      = true;
		info["path"]        = tq(ds->dataFilePath()).toStdString();
		info["rowCount"]    = static_cast<int>(ds->rowCount());

		auto colTypes = ds->getColumnTypesMap();
		info["columnCount"] = static_cast<int>(colTypes.size());

		Json::Value columns(Json::arrayValue);
		for (const auto& [name, type] : colTypes)
		{
			Json::Value col;
			col["name"] = name;
			col["type"] = columnTypeToString(type);

			if (type == columnType::nominal || type == columnType::nominalText || type == columnType::ordinal)
			{
				Column * column = ds->column(name);
				if (column)
					col["distinctCount"] = static_cast<int>(column->labelsNonEmptyCount());
			}

			columns.append(col);
		}
		info["columns"] = columns;

		return info;
	};

	// --- data_load ---
	disp->registerMethodByName("data_load", [this, buildDataInfo](const Json::Value& params) -> Json::Value
	{
		// Reject if a load is already in progress
		for (const auto& [id, job] : _rpcJobs)
		   if (job.status == "running")
			   return JaspRpcDispatcher::errorResult(
				   "A data load is already in progress (job " + std::to_string(id) + ").");

		bool wait      = params.get("wait", true).asBool();
		int  timeoutMs = params.get("timeoutMs", 30000).asInt();

		// Set CSV delimiter before load to skip interactive preview popup
		std::string delimStr = params.get("delimiter", ",").asString();
		if (!delimStr.empty())
		   DesktopCommunicator::singleton()->setKnownCsvDelimiter(delimStr[0]);

		std::string path = params["path"].asString();

		int jobId = _nextRpcJobId++;
		_rpcJobs[jobId] = {"running", ""};

		auto* event = new FileEvent(this, FileEvent::FileOpen);
		event->setSilent(true);
		event->setPath(QString::fromStdString(path));

		connect(event, &FileEvent::completed, this, [this, jobId, event]()
				{
					auto& job = _rpcJobs[jobId];

					if (event->isSuccessful())
					{
					   DataSetPackage* pkg = DataSetPackage::pkg();
					   pkg->setCurrentFile(event->path());
					   emit pkg->newDataLoaded();

					   job.status = "complete";
					}
					else
					{
					   job.status = "error";
					   job.error  = event->message().toStdString();
					}
				}
		);

		event->starts();

		// Fast path: non-blocking — return jobId immediately
		if (!wait)
		{
		   Json::Value response = JaspRpcDispatcher::successResult();
		   response["status"] = "accepted";
		   response["jobId"]  = jobId;
		   return response;
		}

	   // Blocking wait — poll until complete, error, or timeout
		JaspRpcDispatcher::waitAndProcessEvents(timeoutMs,
											   [&](QEventLoop& loop, QTimer&) {
												   auto* pollTimer = new QTimer(&loop);
												   QObject::connect(pollTimer, &QTimer::timeout, &loop, [&]() {
													   auto it = _rpcJobs.find(jobId);
													   if (it == _rpcJobs.end() || it->second.status != "running")
														   loop.quit();
												   });
												   pollTimer->start(100);
											   });

	   // Build response based on final job state
	   auto it = _rpcJobs.find(jobId);
	   if (it == _rpcJobs.end())
		   return JaspRpcDispatcher::errorResult("Job vanished: " + std::to_string(jobId));

	   const auto& job = it->second;

	   if (job.status == "error")
	   {
		   Json::Value response = JaspRpcDispatcher::errorResult(job.error);
		   response["jobId"]  = jobId;
		   response["status"] = "error";
		   return response;
	   }

	   if (job.status == "running")
	   {
		   Json::Value response = JaspRpcDispatcher::successResult();
		   response["jobId"]  = jobId;
		   response["status"] = "running";
		   return response;
	   }

	   // job.status == "complete" — return full metadata
	   {
		   Json::Value response = buildDataInfo(DataSetPackage::pkg() && DataSetPackage::pkg()->workspace() ? DataSetPackage::pkg()->workspace()->shownDataSet() : nullptr);
		   response["status"] = "success";
		   response["jobId"]  = jobId;

		   // Agent just loaded new data — clear data dirty flags
		   AgentStateTracker::notifyDataObserved();

		   return response;
	   }
	});

	// --- data_load_status ---
	disp->registerMethodByName("data_load_status", [this, buildDataInfo](const Json::Value& params) -> Json::Value
	{
	   int jobId = params["jobId"].asInt();

	   auto it = _rpcJobs.find(jobId);
	   if (it == _rpcJobs.end())
		   return JaspRpcDispatcher::errorResult(
			   "Unknown jobId: " + std::to_string(jobId));

	   bool wait      = params.get("wait", true).asBool();
	   int  timeoutMs = params.get("timeoutMs", 30000).asInt();

	   // Fast path: already done or not waiting
	   if (!wait || it->second.status != "running")
	   {
		   const auto& job = it->second;

		   if (job.status == "error")
		   {
			   Json::Value response = JaspRpcDispatcher::errorResult(job.error);
			   response["jobId"]  = jobId;
			   response["status"] = "error";
			   return response;
		   }

		   if (job.status == "complete")
		   {
			   Json::Value response = buildDataInfo(DataSetPackage::pkg() && DataSetPackage::pkg()->workspace() ? DataSetPackage::pkg()->workspace()->shownDataSet() : nullptr);
			   response["status"] = "complete";
			   response["jobId"]  = jobId;
			   AgentStateTracker::notifyDataObserved();
			   return response;
		   }

		   Json::Value response = JaspRpcDispatcher::successResult();
		   response["jobId"]  = jobId;
		   response["status"] = "running";
		   return response;
	   }

	   // Blocking wait
	   JaspRpcDispatcher::waitAndProcessEvents(timeoutMs,
											   [&](QEventLoop& loop, QTimer&) {
												   auto* pollTimer = new QTimer(&loop);
												   QObject::connect(pollTimer, &QTimer::timeout, &loop, [&]() {
													   auto it2 = _rpcJobs.find(jobId);
													   if (it2 == _rpcJobs.end() || it2->second.status != "running")
														   loop.quit();
												   });
												   pollTimer->start(100);
											   });

	   // Re-read after wait
	   it = _rpcJobs.find(jobId);
	   if (it == _rpcJobs.end())
		   return JaspRpcDispatcher::errorResult("Job vanished: " + std::to_string(jobId));

	   const auto& job = it->second;

	   if (job.status == "error")
	   {
		   Json::Value response = JaspRpcDispatcher::errorResult(job.error);
		   response["jobId"]  = jobId;
		   response["status"] = "error";
		   return response;
	   }

	   if (job.status == "complete")
	   {
		   Json::Value response = buildDataInfo(DataSetPackage::pkg() && DataSetPackage::pkg()->workspace() ? DataSetPackage::pkg()->workspace()->shownDataSet() : nullptr);
		   response["status"] = "complete";
		   response["jobId"]  = jobId;
		   AgentStateTracker::notifyDataObserved();
		   return response;
	   }

	   Json::Value response = JaspRpcDispatcher::successResult();
	   response["jobId"]  = jobId;
	   response["status"] = "running";
	   return response;
	});

	// --- data_info ---
	disp->registerMethodByName("data_info", [buildDataInfo, resolveDataSet](const Json::Value& params) -> Json::Value
	{
	   Json::Value response = buildDataInfo(resolveDataSet(params));
	   response["status"] = "success";

	   // Agent just observed the dataset — clear data dirty flags
	   AgentStateTracker::notifyDataObserved();

	   return response;
	});

	Log::log() << "[RPC] Registered data_load, data_load_status, and data_info handlers." << std::endl;
}

bool MainWindow::startDetached(const QString & applicationPath, const QStringList & args) const
{
	QProcess detachMe;

	detachMe.setProgram(applicationPath);
	detachMe.setArguments(args);
#ifdef __unix__
	detachMe.setUnixProcessParameters(QProcess::UnixProcessFlag::IgnoreSigPipe | QProcess::UnixProcessFlag::CreateNewSession | QProcess::UnixProcessFlag::ResetSignalHandlers | QProcess::UnixProcessFlag::DisconnectControllingTerminal);
#endif
	detachMe.setStandardErrorFile(QProcess::nullDevice());
	detachMe.setStandardInputFile(QProcess::nullDevice());
	detachMe.setStandardOutputFile(QProcess::nullDevice());

	qint64 pidResult;
	bool worked = detachMe.startDetached(&pidResult);


	Log::log() << (worked ? "Started" : "Failed to start" ) << " application " << applicationPath << " with args: (" << args.join(", ") << ") and got pid: " << pidResult << std::endl;

	return worked;
}

void MainWindow::fileEventRequestHandler(FileEvent *event)
{
	if (event->operation() == FileEvent::FileNew)
	{
		if (_package->isLoaded())
			MainWindow::startDetached(QCoreApplication::applicationFilePath(), QStringList("--newData"));
		else
			showNewData();
	}
	else if (event->operation() == FileEvent::FileOpen)
	{
		//A .jasp file contains an entire workspace, so opening one while a workspace is already loaded
		//must happen in a separate instance (that instance opens it for real). Datafiles and database
		//connections, in contrast, add a dataset to the current workspace (creating the workspace on
		//first use), so they always open in this instance.
		bool isJaspFile = (event->type() == Utils::FileType::jasp);

		if (_package->isLoaded() && isJaspFile)
		{
			// If this instance has a valid OSF connection save this setting for a new instance
			_odm->savePasswordFromAuthData(OnlineDataManager::OSF);

			// begin new instance
			MainWindow::startDetached(QCoreApplication::applicationFilePath(), QStringList(event->path()));
		}
		else
		{
			setWelcomePageVisible(false);

			_loader->io(event);
			showProgress();
		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		_resultsJsInterface->exportPreviewHTML();
		_package->setAnalysesData(_analyses->asJson());

		JASPExporter::createSnapshot(event->isTmp() ? "jasp_autosave_snapshot_" : "jasp_snapshot_");

		_loader->io(event);
	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportData || event->operation() == FileEvent::FileGenerateData)
	{
		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (!_package->hasDataSet())
			return;

		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (_package->isModified() && (dataAvailable() || analysesAvailable()))
		{
			switch(MessageForwarder::showSaveDiscardCancel(tr("%1 has been modified").arg(currentFileUserReadable()), tr("Would you like to save your changes?")))
			{
			default:
			case MessageForwarder::DialogResponse::Cancel:
				event->setComplete(false);
				return;

			case MessageForwarder::DialogResponse::Save:
				event->chain(_fileMenu->save()); // event will be set completed when the save event is first completed.
				break;

			case MessageForwarder::DialogResponse::Discard:
				FileEvent::removeAutoSaveIfItExists();
				event->setComplete(true);
				break;
			}
		}
		else
		{
			event->setComplete();
		}
	}
}

///Returns true if the caller can go ahead and close up shop.
bool MainWindow::checkPackageModifiedBeforeClosing()
{
	if(_savingForClose)
		return false; //Come on user, be patient!

	if(!_package->isModified())
		return true;

	switch(MessageForwarder::showSaveDiscardCancel(tr("%1 has been modified").arg(currentFileUserReadable()), tr("Would you like to save your changes?")))
	{
	case MessageForwarder::DialogResponse::Save:
	{
		FileEvent * saveEvent = _fileMenu->save();

		if(saveEvent->isCompleted())	return saveEvent->isSuccessful();
		else							_savingForClose = true;
	}
	[[fallthrough]];

	case MessageForwarder::DialogResponse::Cancel:			return false;

	default:												[[fallthrough]];
	case MessageForwarder::DialogResponse::Discard:			
	{
		FileEvent::removeAutoSaveIfItExists();	
		return true;
	}
	}
}

void MainWindow::closeVariablesPage()
{
	_columnModel->setVisible(false);
}

void MainWindow::fileEventRequestFinalize(FileEvent *event)
{
	hideProgress(event->isTmp() && event->operation() == FileEvent::FileSave);
	
	if (event->operation() == FileEvent::FileNew)
	{
	}
	else if (event->operation() == FileEvent::FileOpen)
	{
		if (event->isSuccessful())
		{
			populateUIfromDataSet(event->type() == Utils::FileType::jasp);

			_package->setCurrentFile(event->path());
			

			if(_package->currentFile().startsWith(AppDirs::autoSaveDir()))
				_package->setModified(true); //Its autosaved after all

			if(event->osfPath() != "")
				_package->setFolder("OSF://" + event->osfPath()); //It is also set by setCurrentPath, but then we get some weirdlooking OSF path

			if (event->type() == Utils::FileType::jasp)
			{
				if(!_package->dataSet()->dataFilePath().empty() && !_package->isReadOnlyFile() && strncmp("http", _package->dataSet()->dataFilePath().c_str(), 4) != 0)
				{
					QString dataFilePath = QString::fromStdString(_package->dataSet()->dataFilePath());
					if (QFileInfo::exists(dataFilePath))
					{
						qint64 currentDataFileTimestamp = QFileInfo(dataFilePath).lastModified().toSecsSinceEpoch();
						if (currentDataFileTimestamp > _package->dataSet()->dataFileTimestamp())
						{
							setCheckAutomaticSync(true);
							_package->dataSet()->syncer().startFileSyncing(dataFilePath);
						}
					}
					else
					{
						_package->dataSet()->setDataFile("");
					}
				}
				
				if(_package->dataSet()->databaseJson() != Json::nullValue)
					_package->dataSet()->syncer().startDatabaseSyncing(_package->dataSet()->databaseJson(), true);
			}
			else if(event->isDatabase())
				_package->dataSet()->syncer().startDatabaseSyncing(event->database(), false);

			if (resultXmlCompare::compareResults::theOne()->testMode())
			{				
				//Give it like 3secs to have the ribbon load and the engines to load the data
				QTimer::singleShot(3000, this, &MainWindow::startComparingResults);
			}
			else if(_reporter && !_reporter->isJaspFileNotDabaseOrSynching())
					emit exitSignal(12);
		}
		else if (!event->isCancelled()) //A cancelled Open dialog must NOT reset the workspace: the user merely dismissed the dialog and expects their current data/analyses to survive. A failed open still has to, silent or not, since the loader already tore the workspace down.
		{
			_package->reset();
			setWelcomePageVisible(true);

			if (!event->isSilent())
				MessageForwarder::showWarning(tr("Unable to open file because:\n%1").arg(event->message()));

			if (_openedUsingArgs)	emit exitSignal(3);
		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		bool testingAndSaving = resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->shouldSave();

		if (event->isSuccessful())
		{
			if(!event->isTmp())
			{
				{ //Before changing the currentfile in DataSetPackage we first check this was a recovery file and if so delete it now. The user succesfully saved after all
					QFileInfo	curFileI	( _package->currentFile());
					bool		wasRecovery = curFileI.dir() == QDir(AppDirs::autoSaveDir());

					if(wasRecovery && curFileI.exists())
					{
						QFile removeRecoveryFile(curFileI.absoluteFilePath());
						if(!removeRecoveryFile.moveToTrash())
							removeRecoveryFile.remove();
					}
				}

				_package->setCurrentFile(event->path());
				if(event->osfPath() != "")
					_package->setFolder("OSF://" + event->osfPath()); //It is also set by setCurrentPath, but then we get some weirdlooking OSF path
	
				_package->setModified(false);
				
				FileEvent::removeAutoSaveIfItExists();	
	
				if(testingAndSaving)
					std::cerr << "Tested and saved " << event->path().toStdString() << " succesfully!" << std::endl;
	
				if(_savingForClose)
					emit exitSignal(0);
			}
			else
				_package->setModifiedAfterAutoSave(false);

		}
		else
		{
			if (!event->isCancelled() && !event->isSilent())
				MessageForwarder::showWarning(tr("Save failed"), tr("Unable to save file.\n\n%1").arg(event->message()));

			if(testingAndSaving)
				std::cerr << "Tested " << event->path().toStdString() << " but saving failed because of: " << event->message().toStdString() << std::endl;

			_savingForClose = false; //User should get to try again.
		}

		if(testingAndSaving)
			finishSavingComparedResults();
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (event->isSuccessful())
		{

			setDataAvailable(false);
			setWelcomePageVisible(true);
			closeVariablesPage();

			_resultsJsInterface->resetResults();
			_analyses->setVisible(false);
			_analyses->clear();
			_package->dbDelete();
			_package->reset(true);
			_ribbonModel->showStatistics();
			_fileMenu->buttonsForEmptyWorkspace();
			_filterModel->reset();

			if(!_applicationExiting)
				_engineSync->cleanRestart();
			else
				emit exitSignal();
		}
		else
			_applicationExiting = false;

	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		if(!event->path().endsWith(".pdf") && _preferences->currentThemeName() != "lightTheme")
			_resultsJsInterface->setThemeCss(_preferences->currentThemeName());
	}
	//FileSyncData completion is handled solely via AsyncLoader::syncCompleted -> DataSetSyncer::setSyncingResult
	//(see wireDataSetSync), which covers both the automatic and file-menu initiated syncs exactly once.
	//Routing it also through FileEvent::completed here would double-release the syncer's guard.
}




void MainWindow::populateUIfromDataSet(bool loadAnalyses)
{
	JASPTIMER_SCOPE(MainWindow::populateUIfromDataSet);
	bool errorFound = false;
	stringstream errorMsg;
	
	_resultsJsInterface->setScrollAtAll(false);

	if (loadAnalyses)
		_analyses->loadAnalysesFromDatasetPackage(errorFound, errorMsg, _ribbonModel);

	if (_analyses->count() == 1 && !resultXmlCompare::compareResults::theOne()->testMode()) //I do not want to see QML forms in unit test mode to make sure stuff breaks when options are changed
		(*_analyses)[0]->expandAnalysis(); //Show options for only analysis

	bool hasAnalyses = _analyses->count() > 0;

	hideProgress();
	setWelcomePageVisible(false);
	
	setDataAvailable(_package->dataSet() && (_package->dataSet()->rowCount() > 0 && _package->dataSet()->columnCount() > 0));

	_analyses->setVisible(hasAnalyses && !resultXmlCompare::compareResults::theOne()->testMode());

	if (_package->warningMessage() != "")	MessageForwarder::showWarning(_package->warningMessage());
	else if (errorFound)					MessageForwarder::showWarning(errorMsg.str());

	_package->setLoaded(true);
	checkUsedModules();
	
	_resultsJsInterface->setScrollAtAll(true);
	_package->setModified(false);
}

void MainWindow::checkUsedModules()
{
	_analyses->applyToAll([&](Analysis * analysis)
	{
		if(_ribbonModel->isModuleName(analysis->module()))
			_ribbonModel->ribbonButtonModel(analysis->module())->setEnabled(true);
	});
}

void MainWindow::qmlLoaded()
{
	Log::log() << "MainWindow::qmlLoaded()" << std::endl;
	emit qmlLoadedChanged();
				
	if(!_openOnLoadDbJson.isNull())
		QTimer::singleShot(0, this, &MainWindow::_openDbJson);
}

void MainWindow::_openDbJson()
{
	_fileMenu->open(_openOnLoadDbJson);
	_openOnLoadDbJson = Json::nullValue;
}

void MainWindow::openGitHubBugReport() const
{
	static bool alreadyOpened = false;

	if (alreadyOpened) return;
	alreadyOpened = true;

	bool	openGitHubUserRegistration = false,
			openBrowseFolder = false;

	if(!Settings::value(Settings::USER_HAS_GITHUB_ACCOUNT).toBool())
	{
		if(MessageForwarder::showYesNo(tr("Do you have a GitHub account?"), tr("To be able to report the bug you need to have a GitHub account, do you have such an account?")))
			Settings::setValue(Settings::USER_HAS_GITHUB_ACCOUNT, true);
		else
		{
			openGitHubUserRegistration = true;
			MessageForwarder::showWarning(tr("Join GitHub"),
				tr("We will open two pages for you in your webbrowser.\n"
				"The second will be the 'Join GitHub' page where you can register for an account with GitHub."
				"\n"
				"The first will be a login page that leads to a partly filled bug report after you sign in with your new GitHub account.\n\n"
				"Please fill in all missing information there."));
		}
	}

	std::stringstream systemInfo, debugInfo;

	try			{ systemInfo << "* JASP version: " << AppInfo::version.asString()	<< std::endl; }
	catch(...)	{ systemInfo << "* JASP version: ???" << std::endl; }

	try			{ systemInfo <<	"* OS name and version: " << QSysInfo::prettyProductName() << std::endl; }
	catch(...)	{ systemInfo << "* OS name and version: ???" << std::endl; }

	try			{ systemInfo << "* Commit used: " << AboutModel::commitUrl() << std::endl; }
	catch(...)	{ systemInfo << "Commit couldn't be found\n"; }

	try
	{
		if (!_preferences->logToFile())
			debugInfo << tr("No log files are available. To get more information, please turn logging on. For this: open the file menu (the blue hamburger button left top), navigate to Advanced Preferences and check the 'Log to file' checkbox.") << std::endl;
		else
		{
			QDir logDir(AppDirs::logDir());
			QFileInfoList files = logDir.entryInfoList(QDir::Files, QDir::Time);

			debugInfo << tr("Please drag and drop these log files into this issue: ") << std::endl;
			for (const QFileInfo& file : files)
			{
				debugInfo << "* " << file.fileName() << std::endl;
				if (file.fileName().contains("Desktop")) // The Engine log files are newer, the Desktop file is the oldest log file
					break;
			}
			debugInfo << std::endl;
			openBrowseFolder = true;
		}
	}
	catch(...)	{ debugInfo << "No Log files path found"; }

	try			{ debugInfo << "Debug information: " << _engineSync->currentStateForDebug() << std::endl; }
	catch(...)	{ debugInfo << "No debug information found"; }

	try
	{
		QString systemInfoStr	= QUrl::toPercentEncoding(tq(systemInfo.str())),
				debugInfoStr	= QUrl::toPercentEncoding(tq(debugInfo.str()));

		QString baseIssueUrl = "https://github.com/jasp-stats/jasp-issues/issues/new?template=crash-report.yml&title=JASP+crashed";

		QUrl issueUrl = baseIssueUrl + "&system-info=" + systemInfoStr + "&log=" + debugInfoStr;

		QDesktopServices::openUrl(issueUrl);

		if(openGitHubUserRegistration)
			QTimer::singleShot(0, []()
			{
				QDesktopServices::openUrl(QUrl("https://github.com/join"));
			});

		if(openBrowseFolder)
			showLogFolder();
	}
	catch(...)
	{
		MessageForwarder::showWarning(tr("GitHub couldn't be openend for you"), tr("Something went wrong with leading you to GitHub..\nYou can still report the bug by going to https://github.com/jasp-stats/jasp-issues/issues"));
	}
	
	emit exitSignal(1);
}

void MainWindow::fatalError()
{
	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;
		
		_engineSync->killProcessTimer();
		
		MessageForwarder::DialogResponse response = MessageForwarder::showYesNoCancel(
					tr("Error"), 
					tr("JASP has experienced an unexpected internal error:\n%1").arg(_fatalError) + "\n\n" +
					tr("JASP had a serious error and cannot calculate anymore.\n\nWe would be grateful if you could report this error to the JASP team."), 
					tr("Report"), tr("Salvage"), tr("Exit"), QMessageBox::Icon::Critical);
		
		switch(response)
		{
		case MessageForwarder::DialogResponse::Yes:
			openGitHubBugReport();
			break;
			
		case MessageForwarder::DialogResponse::Cancel:
			exit(2);
			break;
			
		default:
			break;
		}

		MessageForwarder::showWarning(tr("Salvaging"), tr("We're very sorry JASP has had a fatal error.\n"
														  "To allow you to salvage or recover something out of this mess JASP will stay partly functional.\n\n"
														  "You can for instance save your current workspace, or try changing a setting to perhaps prevent the problem next time.\n\n"
														  "Analyses or computed columns and the like will not function anymore until you restart JASP."),
									  QMessageBox::Critical);
		
		
		_hadFatalError = true;
		emit hadFatalErrorChanged();
	}
}

void MainWindow::saveTextToFileHandler(const QString &filename, const QString &data)
{
	if (filename == "%PREVIEW%" || filename == "%EXPORT%")
	{
		_package->setAnalysesHTML(data);
		_package->setAnalysesHTMLReady();

		finishComparingResults();
	}
	else
	{
		QFile file(filename);
		if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate))
			Log::log() << "Cannot open file: " << file.fileName() << "with error: " << file.errorString() << std::endl;

		QTextStream stream(&file);

		stream << data;
		stream.flush();
		file.close();
	}
}

void MainWindow::analysesCountChangedHandler()
{
	setAnalysesAvailable(_analyses->count() > 0);
}

void MainWindow::setPackageModified()
{
	_package->setModified(true);
}

void MainWindow::analysisChangedDownstreamHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == nullptr)
		return;

	string utf8 = fq(options);

	Json::Value root;

	Json::Reader parser;
	parser.parse(utf8, root);

	analysis->setBoundValues(root);
}

bool MainWindow::startDataEditorHandler()
{
	setCheckAutomaticSync(false);
	QString dataFilePath = QString::fromStdString(_package->dataSet()->dataFilePath());

	if (
			(dataFilePath.isEmpty() || _package->manualEdits())
			|| dataFilePath.startsWith("http")
			|| !QFileInfo::exists(dataFilePath)
			|| Utils::getFileSize(dataFilePath.toStdString()) == 0
			|| _package->isReadOnlyFile()
	)
	{
		bool manualEditsMode = _package->manualEdits() && !dataFilePath.isEmpty() && !_package->isReadOnlyFile();

		QString									message = tr("JASP was started without associated data file (csv, sav or ods file). But to edit the data, JASP starts a spreadsheet editor based on this file and synchronize the data when the file is saved. Does this data file exist already, or do you want to generate it?");
		if (dataFilePath.startsWith("http"))	message = tr("JASP was started with an online data file (csv, sav or ods file). But to edit the data, JASP needs this file on your computer. Does this data file also exist on your computer, or do you want to generate it?");
		else if (_package->isReadOnlyFile())	message = tr("JASP was started with a read-only data file (probably from the examples). But to edit the data, JASP needs to write to the data file. Does the same file also exist on your computer, or do you want to generate it?");
		else if (manualEditsMode)				message = tr("JASP has an associated data file, but you edited it. Would you like to reload from the associated data or generate a new file?");

		MessageForwarder::DialogResponse choice;

		if (manualEditsMode)
			choice = MessageForwarder::showYesNoCancel(tr("Start Spreadsheet Editor"), message, tr("Generate Data File"), tr("Reload Data File"));
		else
			choice = MessageForwarder::showYesNoCancel(tr("Start Spreadsheet Editor"), message, tr("Generate Data File"), tr("Find Data File"));

		if (choice != MessageForwarder::DialogResponse::Yes && choice != MessageForwarder::DialogResponse::No)
			return false;

		if (manualEditsMode && choice == MessageForwarder::DialogResponse::No)
		{
			startDataEditor(dataFilePath);
			return true;
		}

		FileEvent *event = nullptr;

		if (choice == MessageForwarder::DialogResponse::Yes)
		{
			QString	caption = "Generate Data File as CSV",
					filter = "CSV Files (*.csv)",
					name = windowTitle();

			Log::log() << "Currently startDataEditorHandler treats title as: " << name.toStdString() << std::endl;

			if (name.endsWith("*"))
			{
				name.truncate(name.length() - 1);
				name = name.replace('#', '_');
			}

			// Default to the shown dataset's dataFilePath if known, else use the window name
			std::string datasetDataFile = _package->dataSet() ? _package->dataSet()->dataFilePath() : "";
			if(!datasetDataFile.empty())
			{
				QFileInfo fi(tq(datasetDataFile));
				name = fi.dir().absoluteFilePath(fi.completeBaseName() + ".csv");
			}
			else
				name = QDir::current().absoluteFilePath(_package->name().replace('#', '_') + ".csv");

			dataFilePath = MessageForwarder::browseSaveFile(caption, name, filter);

			if (dataFilePath == "")
				return false;

			if (!dataFilePath.endsWith(".csv", Qt::CaseInsensitive))
				dataFilePath.append(".csv");

			event = new FileEvent(this, FileEvent::FileGenerateData);
		}
		else
		{
			QString caption = "Find Data File";
			QString filter = "Data File (*.csv *.txt *.tsv *.sav *.ods *.xls *.xlsx *.rdata *.rds *.mwx *.mpx)";

			dataFilePath = MessageForwarder::browseOpenFile(caption, "", filter);
			if (dataFilePath == "")
				return false;

			event = new FileEvent(this, FileEvent::FileSyncData);
			event->setDataSet(_package->dataSet());
		}

		event->setPath(dataFilePath);
		connect(event, &FileEvent::completed, this,	[this, event]()		{ startDataEditorEventCompleted(event); });
		event->starts();
	}
	else
		startDataEditor(dataFilePath);

	return true;
}

void MainWindow::clearModulesFoldersUser()
{
	if(!MessageForwarder::showYesNo(tr("Clean user installed modules and pkgs"), tr("Cleaning up your modules and packages will make sure you only use those bundled with JASP. \n\nMake sure to restart JASP afterwards!"), tr("Clean"), tr("Cancel")))
		return;
	
	delete _engineSync;
	_engineSync = nullptr;
	
	QDir	renvroot(AppDirs::renvRootLocation()),
			usermods(AppDirs::userModulesDir());
	
	if(renvroot.exists())	renvroot.removeRecursively();
	if(usermods.exists())	usermods.removeRecursively();

#ifdef __APPLE__
	QDir devModPatchDir(AppDirs::devModulePatchDir());
	if(devModPatchDir.exists())	devModPatchDir.removeRecursively();
#endif

}

/* the following does not seem to work: the new process crashes immediately... 
void MainWindow::restartJASP()
{
	MainWindow::startDetached(QCoreApplication::applicationFilePath());
	QApplication::quit();
}*/

void MainWindow::showAbout()
{
	_aboutModel->setVisible(true);
}

void MainWindow::showContact()
{
	setContactVisible(true);
}

void MainWindow::showCommunity()
{
	setCommunityVisible(true);
}

void MainWindow::startDataEditorEventCompleted(FileEvent* event)
{
	Log::log() << "[MainWindow::startDataEditorEventCompleted] START: event->isSuccessful()=" << event->isSuccessful() << ", event->path()=" << event->path().toStdString() << std::endl;
	hideProgress();

	if (event->isSuccessful())
	{
		Log::log() << "[MainWindow::startDataEditorEventCompleted] Event successful, updating dataset" << std::endl;
		_package->dataSet()->setDataFile(event->path().toStdString());
		Log::log() << "[MainWindow::startDataEditorEventCompleted] Dataset file set to: " << event->path().toStdString() << std::endl;
		_package->setFileReadOnly(false);
        _fileMenu->setSyncFile(event);
		_package->setModified(true);
		Log::log() << "[MainWindow::startDataEditorEventCompleted] Calling startDataEditor" << std::endl;
		startDataEditor(event->path());
		Log::log() << "[MainWindow::startDataEditorEventCompleted] startDataEditor returned" << std::endl;
	}
	else
	{
		Log::log() << "[MainWindow::startDataEditorEventCompleted] Event NOT successful" << std::endl;
	}
	Log::log() << "[MainWindow::startDataEditorEventCompleted] END" << std::endl;
}


void MainWindow::startDataEditor(QString path)
{
	QFileInfo fileInfo(path);

#ifdef __linux__
	//Linux means flatpak, which doesn't support launching a random binary
#else

	bool useDefaultSpreadsheetEditor = Settings::value(Settings::USE_DEFAULT_SPREADSHEET_EDITOR).toBool();
	QString appname = Settings::value(Settings::SPREADSHEET_EDITOR_NAME).toString();

	if (appname.isEmpty())
		useDefaultSpreadsheetEditor = true;

	if (!useDefaultSpreadsheetEditor)
	{
		QStringList args;
#ifdef __APPLE__
		args = {"-a", appname, path};
		appname = "open";
#else
		args = {path};
#endif
		if (!MainWindow::startDetached(appname, args))
			MessageForwarder::showWarning(tr("Start Editor"), tr("Unable to start the editor : %1. Please check your editor settings in the preference menu.").arg(appname));
	}
	else
#endif
		if (!QDesktopServices::openUrl(QUrl::fromLocalFile(path)))
		{
			if (fileInfo.suffix() == "csv")
				MessageForwarder::showWarning(tr("Start Spreadsheet Editor"), tr("No default spreadsheet editor for file %1. Use Preferences to set the right editor.").arg(fileInfo.fileName()));
			else
			{
				QString message = tr("No default spreadsheet editor for file %1. Do you want to export the data into a CSV file and start the default spreadsheet editor for this CSV file?").arg(fileInfo.fileName());
				if (MessageForwarder::showYesNo(tr("Start Spreadsheet Editor"), message, tr("Generate Data File as CSV"), tr("Cancel")))
				{
					QString	caption = tr("Generate Data File as CSV"),
							filter = "CSV Files (*.csv)",
							name = fileInfo.baseName() + ".csv";

					path = MessageForwarder::browseSaveFile(caption, name, filter);

					if (path == "")
						return;

					if (!path.endsWith(".csv", Qt::CaseInsensitive))
						path.append(".csv");

					FileEvent *event = new FileEvent(this, FileEvent::FileGenerateData);
                    event->setPath(path);
                    connect(event, &FileEvent::completed, this,	 [this, event]() { startDataEditorEventCompleted(event); });
					event->starts();
				}
			}
		}
}

void MainWindow::showProgress()
{
	_fileMenu->setVisible(false);

	setProgressBarVisible(true);
}

void MainWindow::hideProgress(bool wasAutoSave)
{
	setProgressBarVisible(false, wasAutoSave);
}


void MainWindow::setProgressStatus(QString status, int progress)
{
	setProgressBarStatus(status);
	setProgressBarProgress(progress);
}

void MainWindow::testLoadedJaspFile(int timeOut, bool save)
{
	Log::log() << "Enabling testmode for JASP with a timeout of " << timeOut << " minutes!" << std::endl;
	resultXmlCompare::compareResults::theOne()->enableTestMode();

	if(save)
		resultXmlCompare::compareResults::theOne()->enableSaving();

	QTimer::singleShot(60000 * timeOut, this, &MainWindow::unitTestTimeOut);
}

void MainWindow::reportHere(QString dir)
{
	_reporter = new Reporter(this, dir);
}

void MainWindow::unitTestTimeOut()
{
	//If we are showing the user whatever went wrong we shouldnt close JASP automatically because it could get confusing
	if(resultXmlCompare::compareResults::theOne()->analysisHadError())
		return;

	std::cerr << "Time out for unit test!" << std::endl;
	emit exitSignal(3);
}

void MainWindow::startComparingResults()
{
	if (resultXmlCompare::compareResults::theOne()->testMode())
	{
		_analyses->refreshAllAnalyses();
		resultXmlCompare::compareResults::theOne()->setRefreshCalled();
	}
}



void MainWindow::analysesForComparingDoneAlready()
{
	if(	resultXmlCompare::compareResults::theOne()->testMode()		&& 
		resultXmlCompare::compareResults::theOne()->refreshed()		&&
		_analyses->allFinished()									)
		{
			_resultsJsInterface->exportPreviewHTML();
			resultXmlCompare::compareResults::theOne()->setExportCalled();
		}
}

void MainWindow::finishComparingResults()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->exportCalled() && !resultXmlCompare::compareResults::theOne()->comparedAlready())
	{
		QString resultHtml = _package->analysesHTML();
		resultXmlCompare::compareResults::theOne()->setRefreshResult(resultHtml);

		resultXmlCompare::compareResults::theOne()->compare();

		if(resultXmlCompare::compareResults::theOne()->shouldSave())
		{
			if(resultXmlCompare::compareResults::theOne()->checkForAnalysisError())
			{
				MessageForwarder::showWarning("Error in an analysis", "At least one of the analyses loaded for testing and saving had an error, please check what is going on.\n\nIf you are running a recursive unittest it will continue with the rest of the data library once this JASP is manually closed.");
			}
			else
				emit saveJaspFile();
		}
		else
			emit exitSignal(resultXmlCompare::compareResults::theOne()->compareSucces() ? 0 : 1);
	}
}

void MainWindow::finishSavingComparedResults()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->shouldSave())
	{
		emit exitSignal(resultXmlCompare::compareResults::theOne()->compareSucces() ? 0 : 1);
	}
}

void MainWindow::saveJaspFileHandler()
{
	FileEvent * saveEvent = new FileEvent(this, FileEvent::FileSave);
	saveEvent->setPath(resultXmlCompare::compareResults::theOne()->filePath());
	saveEvent->starts();
}

void MainWindow::saveTmpFileHandler()
{
	if (JASPExporter::isSaveInProgress())
		return;

	FileEvent * saveEvent = new FileEvent(this, FileEvent::FileSave);
	saveEvent->setTmp(true);
	saveEvent->starts();
}

bool MainWindow::enginesInitializing()
{
	return _engineSync->allEnginesInitializing();
}

void MainWindow::setProgressBarVisible(bool progressBarVisible, bool wasAutoSave)
{
	if(_progressBarVisible == progressBarVisible)
		return; 
	
	const int64_t	minimumShow	 = 1000; //Is this long enough?
	
	static int64_t lastShownMs = -1;
	
	if(progressBarVisible)
	{
		_progressBarTimer->stop();
		lastShownMs = Utils::currentMillis();
		_setProgressBarVisible(true);
	}
	else
	{
		int64_t diff = lastShownMs == -1 ? -1 : Utils::currentMillis() - lastShownMs;
		
		
		if(!wasAutoSave || lastShownMs == -1 || diff >= minimumShow)
			_setProgressBarVisible(false);
		else
		{
			_progressBarTimer->setInterval(minimumShow-diff);
			_progressBarTimer->start();
			//lastShownMs = -1;
		}
	}
	
	
}

void MainWindow::_setProgressBarVisible(bool progressBarVisible)
{
	if (_progressBarVisible == progressBarVisible)
		return;

	_progressBarVisible = progressBarVisible;
	emit progressBarVisibleChanged(_progressBarVisible);
}

void MainWindow::setProgressBarProgress(int progressBarProgress)
{
	if (_progressBarProgress == progressBarProgress)
		return;

	_progressBarProgress = progressBarProgress;
	emit progressBarProgressChanged(_progressBarProgress);
}

void MainWindow::setProgressBarStatus(QString progressBarStatus)
{
	if (_progressBarStatus == progressBarStatus)
		return;

	_progressBarStatus = progressBarStatus;
	emit progressBarStatusChanged(_progressBarStatus);
}

void MainWindow::removeAnalysis(Analysis *analysis)
{
	_analyses->removeAnalysis(analysis);
	_resultsJsInterface->removeAnalysis(analysis);
}

void MainWindow::removeAllAnalyses()
{
	if (MessageForwarder::showYesNo(tr("Remove All Analyses"), tr("Do you really want to remove all analyses?")))
	{
		_resultsJsInterface->removeAnalyses();
		_analyses->clear();
	}
}

void MainWindow::analysisAdded(Analysis *)
{
	if (!_package->isLoaded())
		_package->setHasAnalysesWithoutData();
	setWelcomePageVisible(false);
}

void MainWindow::setScreenPPI(int screenPPI)
{
	if (_screenPPI == screenPPI)
		return;

	_screenPPI = screenPPI;
	emit screenPPIChanged(_screenPPI);
}


void MainWindow::setDataAvailable(bool dataAvailable)
{
	if (_dataAvailable == dataAvailable)
		return;

	_dataAvailable = dataAvailable;
	emit dataAvailableChanged(_dataAvailable);
}

void MainWindow::setAnalysesAvailable(bool analysesAvailable)
{
	Log::log() << "MainWindow::setAnalysesAvailable(" << (analysesAvailable ? "true" : "false") << ")" << std::endl;

	if (_analysesAvailable == analysesAvailable)
		return;

	_analysesAvailable = analysesAvailable;
	emit analysesAvailableChanged(_analysesAvailable);

	if(!_analysesAvailable && !_package->isLoaded())
	{
		_package->setModified(false);
		setWelcomePageVisible(true);
	}
	else
		_package->setModified(true);

}

void MainWindow::resetQmlCache()
{
	_qml->clearComponentCache();
}

void MainWindow::makeAppleMenu()
{
#ifdef __APPLE__
	//see https://doc.qt.io/qt-5/qmenubar.html#qmenubar-as-a-global-menu-bar
	QMenuBar	*appleMenuBar	= new QMenuBar(0);
	QMenu		*quitMenu		= appleMenuBar->addMenu("quit"),
				*aboutMenu		= appleMenuBar->addMenu("about.JASP"),
				*prefMenu		= appleMenuBar->addMenu("preferences");

	QAction		*macQuit		= new QAction("Quit JASP",				this),
				*macAbout		= new QAction("About JASP",				this),
				*macPreferences = new QAction("Preferences of JASP",	this);

	macQuit->setShortcut(Qt::Key_Close);

	macQuit->setMenuRole(			QAction::QuitRole);
	macAbout->setMenuRole(			QAction::AboutRole);
	macPreferences->setMenuRole(	QAction::PreferencesRole);

	connect(macQuit,		&QAction::triggered, macQuit,			[&](){ if(checkPackageModifiedBeforeClosing()) _application->quit(); });
	connect(macAbout,		&QAction::triggered, macAbout,			[&](){ showAbout(); });
	connect(macPreferences, &QAction::triggered, macPreferences,	[&](){ _fileMenu->showPreferences(); });

	quitMenu->addAction(macQuit);
	aboutMenu->addAction(macAbout);
	prefMenu->addAction(macPreferences);
#endif
}

void MainWindow::setWelcomePageVisible(bool welcomePageVisible)
{
	if (_welcomePageVisible == welcomePageVisible)
		return;

	_welcomePageVisible = welcomePageVisible;
	emit welcomePageVisibleChanged(_welcomePageVisible);
}

void MainWindow::setDownloadNewJASPUrl(QString downloadNewJASPUrl)
{
	if (_downloadNewJASPUrl == downloadNewJASPUrl)
		return;

	_downloadNewJASPUrl = downloadNewJASPUrl;
	emit downloadNewJASPUrlChanged(_downloadNewJASPUrl);
}

QQmlContext * MainWindow::giveRootQmlContext()
{
	return _qml->rootContext();
}

QString MainWindow::versionString()
{
	return	"JASP "
		+	QString::fromStdString(AppInfo::version.asString())
#ifdef JASP_DEBUG
		+	"-Debug"
#endif
#ifdef __APPLE__
		+	" (" + QString::fromStdString(AppInfo::getArchLabel()) + ")"
#endif
			;
}

bool MainWindow::contactVisible() const
{
	return _contactVisible;
}

void MainWindow::setContactVisible(bool newContactVisible)
{
	if (_contactVisible == newContactVisible)
		return;
	_contactVisible = newContactVisible;
	emit contactVisibleChanged();
}

bool MainWindow::communityVisible() const
{
	return _communityVisible;
}

void MainWindow::setCommunityVisible(bool newCommunityVisible)
{
	if (_communityVisible == newCommunityVisible)
		return;
	_communityVisible = newCommunityVisible;
	emit communityVisibleChanged();
}


void MainWindow::loadModulesFromUserConfiguration(configState state)
{
	if(state == configState::FAIL)
		return;

	for(const QString& moduleName : *_jaspConfiguration->getAdditionalModules())
	{
		auto button = _ribbonModel->ribbonButtonModel(moduleName.toStdString());
		_ribbonModel->setModuleEnabled(_ribbonModel->ribbonButtonModelIndex(button), true);
	}

	// Apply OverrideCommon to refresh Common/Extra
	const QStringList* overrideCommon = _jaspConfiguration->getOverrideCommon();
	if(overrideCommon && !overrideCommon->isEmpty())
	{
		DynamicModules::dynMods()->refreshCommonModules(*overrideCommon);
	}
}


bool MainWindow::hadFatalError() const
{
	return _hadFatalError;
}
