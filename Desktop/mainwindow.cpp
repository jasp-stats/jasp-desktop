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
#include <QAction>
#include <QMenuBar>

#include <iostream>

#include "log.h"
#include "timers.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "processinfo.h"

#include "mainwindow.h"

#include "analysisform.h"
#include "controls/jaspcontrol.h"
#include "controls/checkboxbase.h"
#include "controls/comboboxbase.h"
#include "controls/textinputbase.h"
#include "controls/componentslistbase.h"
#include "controls/rsyntaxhighlighter.h"
#include "controls/factorsformbase.h"
#include "controls/inputlistbase.h"
#include "controls/textareabase.h"
#include "controls/sliderbase.h"
#include "controls/expanderbuttonbase.h"
#include "controls/variableslistbase.h"
#include "controls/variablesformbase.h"
#include "controls/factorlevellistbase.h"
#include "controls/tableviewbase.h"
#include "controls/radiobuttonbase.h"
#include "controls/radiobuttonsgroupbase.h"
#include "controls/jaspdoublevalidator.h"
#include "controls/groupboxbase.h"

#include "gui/jaspversionchecker.h"
#include "gui/preferencesmodel.h"
#include "ALTNavigation/altnavigation.h"
#include "ALTNavigation/altnavcontrol.h"
#include "utilities/messageforwarder.h"

#include "modules/activemodules.h"
#include "modules/dynamicmodules.h"
#include "modules/menumodel.h"
#include "modules/description/entrybase.h"

#include "qquick/datasetview.h"
#include "qquick/rcommander.h"

#include "resultstesting/compareresults.h"

#include "utilities/qutils.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "utilities/qmlutils.h"
#include "utilities/reporter.h"

#include "widgets/filemenu/filemenu.h"
#include "rsyntax/formulabase.h"
#include "utilities/desktopcommunicator.h"

#include "boost/iostreams/stream.hpp"
#include <boost/iostreams/device/null.hpp>

#include "communitydefs.h"

using namespace std;
using namespace Modules;

MainWindow * MainWindow::_singleton	= nullptr;

MainWindow::MainWindow(QApplication * application) : QObject(application), _application(application)
{
	std::cout << "MainWindow constructor started" << std::endl;

	connect(this, &MainWindow::exitSignal, this, &QApplication::exit, Qt::QueuedConnection);

	assert(!_singleton);
	_singleton = this;
	JASPTIMER_START(MainWindowConstructor);
	
	QQuickStyle::setStyle("Basic");
	QQuickWindow::setTextRenderType(Settings::value(Settings::GUI_USE_QT_TEXTRENDER).toBool() ?
										QQuickWindow::QtTextRendering : QQuickWindow::NativeTextRendering);

	TempFiles::init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	makeAppleMenu(); //Doesnt do anything outside of magical apple land
	
	std::cout << "Going to construct the necessary models for JASP to function." << std::endl;

	//The order of these constructors is deliberate (up to some extent anyway). If you change the order you might find that stuff explodes randomly (although most likely during startup)
	_qml					= new QQmlApplicationEngine(this);
	_languageModel			= new LanguageModel(application, _qml, this);
	_loader					= new AsyncLoader(nullptr);
	_preferences			= new PreferencesModel(this);
	_package				= new DataSetPackage(this);
	_dynamicModules			= new DynamicModules(this);
	_upgrader				= new Upgrader(this);
	_analyses				= new Analyses();
	_engineSync				= new EngineSync(this);
	_datasetTableModel		= new DataSetTableModel();
	_dataSetModelVarInfo	= new DataSetTableModel(false);
	_columnModel			= new ColumnModel(_datasetTableModel);
	
	initLog(); //initLog needs _preferences and _engineSync!

	Log::log() << "JASP " << AppInfo::version.asString() << " from commit " << AppInfo::gitCommit << " and branch " << AppInfo::gitBranch << " is continuing initialization." << std::endl;

	_resultsJsInterface		= new ResultsJsInterface();
	_odm					= new OnlineDataManager(this);
	_labelFilterGenerator	= new labelFilterGenerator(_columnModel,		this);
	_columnsModel			= new ColumnsModel(_dataSetModelVarInfo);			// We do not want filtered-out columns/levels to be selectable in other guis, see: https://github.com/jasp-stats/INTERNAL-jasp/issues/2322
	_workspaceModel			= new WorkspaceModel(this);
	_computedColumnsModel	= new ComputedColumnModel();
	_filterModel			= new FilterModel(_labelFilterGenerator);
	_ribbonModel			= new RibbonModel();
	_ribbonModelFiltered	= new RibbonModelFiltered(this, _ribbonModel);
	_ribbonModelUncommon	= new RibbonModelUncommon(this, _ribbonModel);
	_fileMenu				= new FileMenu(this);
	_helpModel				= new HelpModel(this);
	_aboutModel				= new AboutModel(this);
	_resultMenuModel		= new ResultMenuModel(this);
	_plotEditorModel		= new PlotEditorModel();
	_columnTypesModel		= new ColumnTypesModel(this);

#ifdef WIN32
	_windowsWorkaroundCPs	= new CodePagesWindows(this);
#endif

	_msgForwarder = new MessageForwarder(this);

	startOnlineDataManager();

	makeConnections();

	qmlRegisterUncreatableType<JASPControl>						("JASP",			1, 0, "JASP",				"Impossible to create JASP Object"	); //This is here to keep JASP.enum short I guess?
	qmlRegisterUncreatableType<MessageForwarder>				("JASP",			1, 0, "MessageForwarder",	"You can't touch this"				);

	qmlRegisterType<DataSetView>								("JASP",			1, 0, "DataSetView"						);
	qmlRegisterType<JaspTheme>									("JASP",			1, 0, "JaspTheme"						);
	qmlRegisterType<AnalysisForm>								("JASP",			1, 0, "AnalysisForm"					);
	qmlRegisterType<RCommander>									("JASP",			1, 0, "RCommander"						);
	qmlRegisterType<JASPControl>								("JASP",			1, 0, "JASPControl"						);
	qmlRegisterType<GroupBoxBase>								("JASP",			1, 0, "GroupBoxBase"					);
	qmlRegisterType<ExpanderButtonBase>							("JASP",			1, 0, "ExpanderButtonBase"				);
	qmlRegisterType<CheckBoxBase>								("JASP",			1, 0, "CheckBoxBase"					);
	qmlRegisterType<SliderBase>									("JASP",			1, 0, "SliderBase"						);
	qmlRegisterType<TextInputBase>								("JASP",			1, 0, "TextInputBase"					);
	qmlRegisterType<TextAreaBase>								("JASP",			1, 0, "TextAreaBase"					);
	qmlRegisterType<ComboBoxBase>								("JASP",			1, 0, "ComboBoxBase"					);
	qmlRegisterType<RadioButtonBase>							("JASP",			1, 0, "RadioButtonBase"					);
	qmlRegisterType<RadioButtonsGroupBase>						("JASP",			1, 0, "RadioButtonsGroupBase"			);
	qmlRegisterType<RSyntaxHighlighterQuick>					("JASP",			1, 0, "RSyntaxHighlighterQuick"			);
	qmlRegisterType<ComponentsListBase>							("JASP",			1, 0, "ComponentsListBase"				);
	qmlRegisterType<FactorsFormBase>							("JASP",			1, 0, "FactorsFormBase"					);
	qmlRegisterType<InputListBase>								("JASP",			1, 0, "InputListBase"					);
	qmlRegisterType<FactorLevelListBase>						("JASP",			1, 0, "FactorLevelListBase"				);
	qmlRegisterType<VariablesListBase>							("JASP",			1, 0, "VariablesListBase"				);
	qmlRegisterType<VariablesFormBase>							("JASP",			1, 0, "VariablesFormBase"				);
	qmlRegisterType<TableViewBase>								("JASP",			1, 0, "TableViewBase"					);
	qmlRegisterType<JASPDoubleValidator>						("JASP",			1, 0, "JASPDoubleValidator"				);
	qmlRegisterType<ResultsJsInterface>							("JASP",			1, 0, "ResultsJsInterface"				);
	qmlRegisterType<ColumnModel>								("JASP",			1, 0, "ColumnModel"						);
	qmlRegisterType<FormulaBase>								("JASP",			1, 0, "Formula"							);
	qmlRegisterUncreatableType<PlotEditor::AxisModel>			("JASP.PlotEditor",	1, 0, "AxisModel",					"Can't make it");
	qmlRegisterUncreatableType<PlotEditor::PlotEditorModel>		("JASP.PlotEditor",	1, 0, "PlotEditorModel",			"Can't make it");

	ALTNavigation::registerQMLTypes("JASP");
	ALTNavControl::ctrl()->enableAlTNavigation(_preferences->ALTNavModeActive());

	_dynamicModules->registerQMLTypes();

	QTimer::singleShot(0, [&]() { loadQML(); });

	_languageModel->setApplicationEngine(_qml);

	_engineSync->start(_preferences->plotPPI());
	
	checkForUpdates();

	Log::log() << "JASP Desktop started and Engines initalized." << std::endl;

	JASPTIMER_FINISH(MainWindowConstructor);
}


void MainWindow::checkForUpdates()
{
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

	_analyses->destroyAllForms();

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

		if (_package->hasDataSet())
			_package->reset(false);

		//delete _engineSync; it will be deleted by Qt!
	}
	catch(...)	{}
}

QString MainWindow::windowTitle() const
{
	return _package->windowTitle();
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
	return "https://jasp-stats.org/request-feature";	
}

const QString MainWindow::contactUrlBugs() const
{
	return "https://jasp-stats.org/report-bug";
}

const QString MainWindow::contactText() const
{
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
		DataSetPackage::pkg()->setSynchingExternally(false);
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
	connect(this,					&MainWindow::screenPPIChanged,						_preferences,			&PreferencesModel::setDefaultPPI							);
	connect(this,					&MainWindow::editImageCancelled,					_resultsJsInterface,	&ResultsJsInterface::cancelImageEdit						);
	connect(this,					&MainWindow::dataAvailableChanged,					_dynamicModules,		&DynamicModules::setDataLoaded								);
	connect(this,					&MainWindow::dataAvailableChanged,					_ribbonModel,			&RibbonModel::dataLoadedChanged								);
	connect(this,					&MainWindow::dataAvailableChanged,					this,					&MainWindow::checkEmptyWorkspace							);
	connect(this,					&MainWindow::analysesAvailableChanged,				this,					&MainWindow::checkEmptyWorkspace							);


	connect(_package,				&DataSetPackage::synchingExternallyChanged,			_ribbonModel,			&RibbonModel::synchronisationChanged						);
	connect(_package,				&DataSetPackage::datasetChanged,					_filterModel,			&FilterModel::datasetChanged,								Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::datasetChanged,					_computedColumnsModel,	&ComputedColumnModel::datasetChanged,						Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::checkForDependentColumnsToBeSent,	_computedColumnsModel,	&ComputedColumnModel::checkForDependentColumnsToBeSentSlot	);
	connect(_package,				&DataSetPackage::datasetChanged,					_columnsModel,			&ColumnsModel::datasetChanged								);
	connect(_package,				&DataSetPackage::isModifiedChanged,					this,					&MainWindow::packageChanged									);
	connect(_package,				&DataSetPackage::windowTitleChanged,				this,					&MainWindow::windowTitleChanged								);
	connect(_package,				&DataSetPackage::columnDataTypeChanged,				_computedColumnsModel,	&ComputedColumnModel::recomputeColumn						);
	connect(_package,				&DataSetPackage::checkDoSync,						_loader,				&AsyncLoader::checkDoSync,									Qt::DirectConnection); //Force DirectConnection because the signal is called from Importer which means it is running in AsyncLoaderThread...
	connect(_package,				&DataSetPackage::synchingIntervalPassed,			this,					&MainWindow::syncKeyPressed									);
	connect(_package,				&DataSetPackage::newDataLoaded,						this,					&MainWindow::populateUIfromDataSet							);
	connect(_package,				&DataSetPackage::newDataLoaded,						_fileMenu,				[&](){ _fileMenu->enableButtonsForOpenedWorkspace(); }		);
	connect(_package,				&DataSetPackage::dataModeChanged,					_analyses,				&Analyses::dataModeChanged									);
	connect(_package,				&DataSetPackage::dataModeChanged,					_engineSync,			&EngineSync::dataModeChanged								);
	connect(_package,				&DataSetPackage::dataModeChanged,					this,					&MainWindow::onDataModeChanged								);
	connect(_package,				&DataSetPackage::askUserForExternalDataFile,		this,					&MainWindow::startDataEditorHandler							);
	connect(_package,				&DataSetPackage::runFilter,							_filterModel,			&FilterModel::sendGeneratedAndRFilter						);
	connect(_package,				&DataSetPackage::showWarning,						_msgForwarder,			&MessageForwarder::showWarningQML,							Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::synchingExternallyChanged,			_fileMenu,				&FileMenu::dataAutoSynchronizationChanged					);
	connect(_package,				&DataSetPackage::workspaceEmptyValuesChanged,		_analyses,				&Analyses::refreshAllAnalyses								);
	
	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_computedColumnsModel,	&ComputedColumnModel::computeColumnSucceeded				);
	connect(_engineSync,			&EngineSync::computeColumnRemoved,					_computedColumnsModel,	&ComputedColumnModel::computeColumnRemoved					);
	connect(_engineSync,			&EngineSync::computeColumnFailed,					_computedColumnsModel,	&ComputedColumnModel::computeColumnFailed					);
	connect(_engineSync,			&EngineSync::engineTerminated,						this,					&MainWindow::fatalError,									Qt::QueuedConnection); //To give the process some time to realize it has crashed or something
	connect(_engineSync,			&EngineSync::columnDataTypeChanged,					_columnsModel,			&ColumnsModel::columnTypeChanged							);
	connect(_engineSync,			&EngineSync::refreshAllPlotsExcept,					_analyses,				&Analyses::refreshAllPlots									);
	connect(_engineSync,			&EngineSync::processNewFilterResult,				_filterModel,			&FilterModel::processFilterResult							);
	connect(_engineSync,			&EngineSync::processFilterErrorMsg,					_filterModel,			&FilterModel::processFilterErrorMsg							);
	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_filterModel,			&FilterModel::computeColumnSucceeded						);
	connect(_engineSync,			&EngineSync::plotEditorRefresh,						_plotEditorModel,		&PlotEditorModel::refresh									);
	connect(_engineSync,			&EngineSync::checkDataSetForUpdates,				_package,				&DataSetPackage::checkDataSetForUpdates,					Qt::QueuedConnection);

	qRegisterMetaType<columnType>();
	qRegisterMetaType<ListModel*>();
	qRegisterMetaType<DbType>();

	connect(_computedColumnsModel,	&ComputedColumnModel::sendComputeCode,				_engineSync,			&EngineSync::computeColumn,									Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnModel::dataColumnAdded,				_fileMenu,				&FileMenu::dataColumnAdded									);
	connect(_computedColumnsModel,	&ComputedColumnModel::showAnalysisForm,				_analyses,				&Analyses::selectAnalysis									);
	connect(_computedColumnsModel,	&ComputedColumnModel::showAnalysisForm,				this,					&MainWindow::showAnalysis									);
	connect(_computedColumnsModel,	&ComputedColumnModel::chooseColumn,					_columnModel,			&ColumnModel::setChosenColumnByName,						Qt::QueuedConnection);
			
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_columnModel,			&ColumnModel::languageChangedHandler,						Qt::QueuedConnection);

	connect(_resultsJsInterface,	&ResultsJsInterface::packageModified,				this,					&MainWindow::setPackageModified								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisChangedDownstream,		this,					&MainWindow::analysisChangedDownstreamHandler				);
	connect(_resultsJsInterface,	&ResultsJsInterface::saveTextToFile,				this,					&MainWindow::saveTextToFileHandler							);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSaveImage,				this,					&MainWindow::analysisSaveImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisResizeImage,			this,					&MainWindow::analysisEditImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsPageLoadedSignal,		this,					&MainWindow::resultsPageLoaded								);
	connect(_resultsJsInterface,	&ResultsJsInterface::refreshAllAnalyses,			this,					&MainWindow::refreshKeyPressed								);
	connect(_resultsJsInterface,	&ResultsJsInterface::removeAllAnalyses,				this,					&MainWindow::removeAllAnalyses								);
	connect(_resultsJsInterface,	&ResultsJsInterface::openFileTab,					_fileMenu,				&FileMenu::showFileOpenMenu									);
	connect(_resultsJsInterface,	&ResultsJsInterface::removeAnalysisRequest,			_analyses,				&Analyses::removeAnalysisById								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSelected,				_analyses,				&Analyses::analysisIdSelectedInResults						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisUnselected,			_analyses,				&Analyses::analysesUnselectedInResults						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisTitleChangedInResults,	_analyses,				&Analyses::analysisTitleChangedInResults					);
	connect(_resultsJsInterface,	&ResultsJsInterface::duplicateAnalysis,				_analyses,				&Analyses::duplicateAnalysis								);
	connect(_resultsJsInterface,	&ResultsJsInterface::showDependenciesInAnalysis,	_analyses,				&Analyses::showDependenciesInAnalysis						);
	connect(_resultsJsInterface,	&ResultsJsInterface::showPlotEditor,				_plotEditorModel,		&PlotEditorModel::showPlotEditor							);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsMetaChanged,			_analyses,				&Analyses::resultsMetaChanged								);
	connect(_resultsJsInterface,	&ResultsJsInterface::allUserDataChanged,			_analyses,				&Analyses::allUserDataChanged								);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsPageLoadedSignal,		_languageModel,			&LanguageModel::resultsPageLoaded							);
	connect(_resultsJsInterface,	&ResultsJsInterface::showRSyntaxInResults,			_analyses,				&Analyses::showRSyntaxInResults								);

	connect(_analyses,				&Analyses::countChanged,							this,					&MainWindow::analysesCountChangedHandler					);
	connect(_analyses,				&Analyses::analysisResultsChanged,					this,					&MainWindow::analysisResultsChangedHandler					);
	connect(_analyses,				&Analyses::analysisImageSaved,						this,					&MainWindow::analysisImageSavedHandler						);
	connect(_analyses,				&Analyses::emptyQMLCache,							this,					&MainWindow::resetQmlCache									);
	connect(_analyses,				&Analyses::analysisAdded,							this,					&MainWindow::analysisAdded									);
	connect(_analyses,				&Analyses::analysisAdded,							_fileMenu,				&FileMenu::analysisAdded									);
	connect(_analyses,				&Analyses::analysesExportResults,					_fileMenu,				&FileMenu::analysesExportResults							);
	connect(_analyses,				&Analyses::analysisStatusChanged,					_resultsJsInterface,	&ResultsJsInterface::setStatus								);
	connect(_analyses,              &Analyses::analysisTitleChanged,                    _resultsJsInterface,    &ResultsJsInterface::changeTitle							);
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
	connect(_fileMenu,				&FileMenu::dataSetIORequest,						this,					&MainWindow::dataSetIORequestHandler						);
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
	connect(_preferences,			&PreferencesModel::normalizedNotationChanged,		_resultsJsInterface,	&ResultsJsInterface::setNormalizedNotationHandler			);
	connect(_preferences,			&PreferencesModel::developerFolderChanged,			_dynamicModules,		&DynamicModules::uninstallJASPDeveloperModule				);
	connect(_preferences,			&PreferencesModel::showRSyntaxInResultsChanged,		_analyses,				&Analyses::showRSyntaxInResults								);
	connect(_preferences,			&PreferencesModel::ALTNavModeActiveChanged,			ALTNavControl::ctrl(),	&ALTNavControl::enableAlTNavigation							);
	connect(_preferences,			&PreferencesModel::orderByValueByDefaultChanged,	[&](){	Column::setAutoSortByValuesByDefault(PreferencesModel::prefs()->orderByValueByDefault()); });
	
	Column::setAutoSortByValuesByDefault(PreferencesModel::prefs()->orderByValueByDefault());
	
	auto * dCSingleton = DesktopCommunicator::singleton();

	//Needed to allow for a hard split between Desktop/QMLComps:
	connect(_preferences,			&PreferencesModel::uiScaleChanged,					dCSingleton,			&DesktopCommunicator::uiScaleChanged			);
	connect(_preferences,			&PreferencesModel::interfaceFontChanged,			dCSingleton,			&DesktopCommunicator::interfaceFontChanged		);
	connect(_preferences,			&PreferencesModel::currentJaspThemeChanged,			dCSingleton,			&DesktopCommunicator::currentJaspThemeChanged	);
	connect(dCSingleton,			&DesktopCommunicator::useNativeFileDialogSignal,	_preferences,			&PreferencesModel::useNativeFileDialog			);

	connect(_filterModel,			&FilterModel::refreshAllAnalyses,					_analyses,				&Analyses::refreshAllAnalyses,								Qt::QueuedConnection);
	connect(_filterModel,			&FilterModel::updateColumnsUsedInConstructedFilter, _package,				&DataSetPackage::setColumnsUsedInEasyFilter					);
	connect(_filterModel,			&FilterModel::filterUpdated,						_package,				&DataSetPackage::refresh									);
	connect(_filterModel,			&FilterModel::filterUpdated,						[&]() { _package->resetFilterCounters(); emit _columnsModel->filterChanged(); }		);
	connect(_filterModel,			&FilterModel::sendFilter,							_engineSync,			&EngineSync::sendFilter										);

	connect(_labelFilterGenerator,	&labelFilterGenerator::setGeneratedFilter,			_filterModel,			&FilterModel::setGeneratedFilter,							Qt::QueuedConnection);

	connect(_ribbonModel,			&RibbonModel::analysisClickedSignal,				_analyses,				&Analyses::analysisClickedHandler							);
	connect(_ribbonModel,			&RibbonModel::showRCommander,						this,					&MainWindow::showRCommander									);
	connect(_ribbonModel,			&RibbonModel::dataModeChanged,						_package,				&DataSetPackage::dataModeChanged							);
	connect(_ribbonModel,			&RibbonModel::setDataSynchronisation,				_package,				&DataSetPackage::setSynchingExternallyFriendly				);

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

	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_fileMenu,				&FileMenu::refresh											);
	connect(_languageModel,			&LanguageModel::aboutToChangeLanguage,				_analyses,				&Analyses::prepareForLanguageChange							);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_analyses,				&Analyses::languageChangedHandler,							Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_helpModel,				&HelpModel::generateJavascript,								Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				this,					&MainWindow::contactTextChanged,							Qt::QueuedConnection); //Probably not necessary but we can check once there actually are translations
	connect(_languageModel,			&LanguageModel::pauseEngines,						_engineSync,			&EngineSync::pauseEngines									);
	connect(_languageModel,			&LanguageModel::stopEngines,						_engineSync,			&EngineSync::stopEngines									);
	connect(_languageModel,			&LanguageModel::resumeEngines,						_engineSync,			&EngineSync::resumeEngines,									Qt::QueuedConnection);

	connect(_qml,					&QQmlApplicationEngine::warnings,					this,					&MainWindow::printQmlWarnings								);

	connect(_plotEditorModel,		&PlotEditorModel::saveImage,						this,					&MainWindow::analysisSaveImageHandler						);
}

void MainWindow::printQmlWarnings(const QList<QQmlError> &warnings)
{
	Log::log()		<< "Received QML warnings:\n";
	for(const QQmlError & warning : warnings)
		Log::log(false)	<< "\t" << warning.toString() << "\n";
	Log::log(false) << std::endl;
}


void MainWindow::loadQML()
{
	Log::log() << "Initializing QML" << std::endl;

	_qml->rootContext()->setContextProperty("mainWindow",								this											);
	_qml->rootContext()->setContextProperty("columnModel",								_columnModel									);
	_qml->rootContext()->setContextProperty("aboutModel",								_aboutModel										);
	_qml->rootContext()->setContextProperty("dataSetModel",								_datasetTableModel								);
	_qml->rootContext()->setContextProperty("columnsModel",								_columnsModel									);
	_qml->rootContext()->setContextProperty("workspaceModel",							_workspaceModel									);
	_qml->rootContext()->setContextProperty("analysesModel",							_analyses										);
	_qml->rootContext()->setContextProperty("dynamicModules",							_dynamicModules									);
	_qml->rootContext()->setContextProperty("plotEditorModel",							_plotEditorModel								);
	_qml->rootContext()->setContextProperty("preferencesModel",							_preferences									);
	_qml->rootContext()->setContextProperty("resultsJsInterface",						_resultsJsInterface								);
	_qml->rootContext()->setContextProperty("ribbonModelFiltered",						_ribbonModelFiltered							);
	_qml->rootContext()->setContextProperty("computedColumnsInterface",					_computedColumnsModel							);
	_qml->rootContext()->setContextProperty("windowsCodePagesHelper",					_windowsWorkaroundCPs							); //is nullptr on not-windows!
	_qml->rootContext()->setContextProperty("ribbonModelUncommon",						_ribbonModelUncommon							);
	_qml->rootContext()->setContextProperty("columnTypesModel",							_columnTypesModel								);
	_qml->rootContext()->setContextProperty("resultMenuModel",							_resultMenuModel								);
	_qml->rootContext()->setContextProperty("fileMenuModel",							_fileMenu										);
	_qml->rootContext()->setContextProperty("filterModel",								_filterModel									);
	_qml->rootContext()->setContextProperty("ribbonModel",								_ribbonModel									);
	_qml->rootContext()->setContextProperty("engineSync",								_engineSync										);
	_qml->rootContext()->setContextProperty("helpModel",								_helpModel										);
	_qml->rootContext()->setContextProperty("jaspTheme",								nullptr											); //Will be set from jaspThemeChanged()!
	_qml->rootContext()->setContextProperty("messages",									MessageForwarder::msgForwarder()				);
	_qml->rootContext()->setContextProperty("qmlUtils",									new QmlUtils(this)								);

	_qml->rootContext()->setContextProperty("baseBlockDim",								20												); //should be taken from Theme
	_qml->rootContext()->setContextProperty("baseFontSize",								16												);
	_qml->rootContext()->setContextProperty("languageModel",							_languageModel									);

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


	bool	debug	= false,
			isMac	= false,
			isLinux = false;

#ifdef JASP_DEBUG
	debug = true;
#endif

#ifdef __APPLE__
	isMac = true;
#endif

#ifdef __linux__
	isLinux = true;
#endif

	bool isWindows = !isMac && !isLinux;

	_qml->rootContext()->setContextProperty("DEBUG_MODE",			debug);
	_qml->rootContext()->setContextProperty("MACOS",				isMac);
	_qml->rootContext()->setContextProperty("LINUX",				isLinux);
	_qml->rootContext()->setContextProperty("WINDOWS",				isWindows);
	_qml->rootContext()->setContextProperty("INTERACTION_SEPARATOR", Term::separator);

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

	Log::log() << "Loading HelpWindow"			<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/HelpWindow.qml"));
	Log::log() << "Loading AboutWindow"			<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/AboutWindow.qml"));
	Log::log() << "Loading ContactWindow"		<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/ContactWindow.qml"));
	Log::log() << "Loading CommunityWindow"		<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/CommunityWindow.qml"));
	Log::log() << "Loading MainWindow"			<< std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/MainWindow.qml"));

	
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

	//connect(DataSetView::lastInstancedDataSetView(), &DataSetView::selectionStartChanged,	_columnModel,	&ColumnModel::changeSelectedColumn);

	Log::log() << "QML Initialized!"  << std::endl;

	Log::log() << "Loading upgrades definitions"  << std::endl;
	_upgrader->loadOldSchoolUpgrades();

	//And now we disconnect the exit on fail lambda because we won't be needing it later
	disconnect(exitOnFailConnection);

	//Load the ribbonmodel modules now because we have an actual qml context to do so in.
	_ribbonModel->loadModules(	
		ActiveModules::getActiveCommonModules(),
		ActiveModules::getActiveExtraModules());
	
	qmlLoaded();	
}


void MainWindow::showEnginesWindow()
{
	Log::log() << "Showing EnginesWindow"  << std::endl;
	_qml->load(QUrl("qrc:///components/JASP/Widgets/EnginesWindow.qml"));
}

void MainWindow::setQmlImportPaths()
{
	static QStringList originalImportPaths = _qml->importPathList();

	QStringList newImportPaths = originalImportPaths;

	newImportPaths.append("qrc:///components");
	newImportPaths.append(_dynamicModules->importPaths());

	if(_qml->importPathList() == newImportPaths)
		return;

	_qml->setImportPathList(newImportPaths);

	if(_preferences->developerMode())
	{
		Log::log() << "QML has the following import paths:\n";

		for(const QString & p : _qml->importPathList())
			Log::log() << "\t" << p << "\n";
		Log::log() << std::endl;
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

void MainWindow::openFolderExternally(QDir folder)
{
	QDesktopServices::openUrl(QUrl::fromLocalFile(folder.absolutePath()));
}

void MainWindow::showLogFolder()
{
	openFolderExternally(AppDirs::logDir());
}


void MainWindow::open(QString filepath)
{
	if(resultXmlCompare::compareResults::theOne()->testMode())
		resultXmlCompare::compareResults::theOne()->setFilePath(filepath);

	_openedUsingArgs = true;
	if (_resultsPageLoaded)	_fileMenu->open(filepath);
	else					_openOnLoadFilename = filepath;
}

void MainWindow::showNewData()
{
	_package->generateEmptyData();
	_ribbonModel->showData();
}

void MainWindow::open(const Json::Value & dbJson)
{
	_openedUsingArgs = true;
	if (_resultsPageLoaded)	_fileMenu->open(dbJson);
	else					_openOnLoadDbJson = dbJson;
}

/*

void MainWindow::dragEnterEvent(QDragEnterEvent *event)
{
	const QMimeData *data = event->mimeData();

	if (data->hasUrls())
	{
		QList<QUrl> urls = data->urls();
		QUrl first = urls.first();
		QFileInfo file(first.path());

		if (file.exists() && (file.completeSuffix() == "csv" || file.completeSuffix() == "jasp"))
			event->accept();
		else
			event->ignore();
	}
	else
	{
		event->ignore();
	}
}


void MainWindow::dropEvent(QDropEvent *event)
{
	const QMimeData *data = event->mimeData();
	QUrl url = data->urls().first();
	open(url.path());

	event->accept();
}


void MainWindow::closeEvent(QCloseEvent *event)
{
	_odm->clearAuthenticationOnExit(OnlineDataManager::OSF);

	if (_applicationExiting)
	{
		// sometimes on osx we get two events
		event->accept();
	}

	_applicationExiting = true;

	if (_package->isModified())
	{
		_fileMenu->close();
		event->ignore();
	}
	else
	{
		event->accept();
	}

	PreferencesDialog *rd = ui->tabBar->getPreferencesDialog();
	if (rd) rd->close();
}*/

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
	DataSetPackage::pkg()->undoStack()->undo();
}

void MainWindow::redo()
{
	DataSetPackage::pkg()->undoStack()->redo();
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
		_fileMenu->close();
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

	_resultsJsInterface->analysisChanged(analysis);

	setPackageModified();

	if(resultXmlCompare::compareResults::theOne()->testMode())
		analysesForComparingDoneAlready();
	
	if(_reporter && _analyses->allFinished())
		_reporter->analysesFinished();
}

void MainWindow::analysisSaveImageHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == nullptr)
		return;

	if (analysis->needsRefresh())
	{
		if(MessageForwarder::showYesNo(tr("Version incompatibility"), tr("This analysis was created in an older version of JASP, to save the image it must be refreshed first.\n\nRefresh the analysis?")))
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
		if (MessageForwarder::showYesNo(tr("Version incompatibility"), tr("This analysis was created in an older version of JASP, to resize the image it must be refreshed first.\n\nRefresh the analysis?")))
			analysis->refresh();
		else
			emit editImageCancelled(id);
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

void MainWindow::connectFileEventCompleted(FileEvent * event)
{
	connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted, Qt::QueuedConnection);
}

void MainWindow::dataSetIORequestHandler(FileEvent *event)
{
	if (event->operation() == FileEvent::FileNew)
	{
		if (_package->isLoaded())
			QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList("--newData"));
		else
			showNewData();
	}
	else if (event->operation() == FileEvent::FileOpen)
	{
		if (_package->isLoaded())
		{
			// If this instance has a valid OSF connection save this setting for a new instance
			_odm->savePasswordFromAuthData(OnlineDataManager::OSF);

			// begin new instance
			
			if(event->isDatabase())		QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(tq(event->databaseStr())));
			else						QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(event->path()));
		}
		else
		{
			connectFileEventCompleted(event);

			setWelcomePageVisible(false);

			_loader->io(event);
			showProgress();
		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		connectFileEventCompleted(event);
		
		_resultsJsInterface->exportPreviewHTML();
		_package->setAnalysesData(_analyses->asJson());

		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		connectFileEventCompleted(event);
		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportData || event->operation() == FileEvent::FileGenerateData)
	{
		connectFileEventCompleted(event);
		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (!_package->hasDataSet())
			return;

		connectFileEventCompleted(event);
		_loader->io(event);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (_package->isModified() && (dataAvailable() || analysesAvailable()))
		{
			QString title = windowTitle();
			title.chop(1);

			switch(MessageForwarder::showSaveDiscardCancel(tr("Would you like to save your changes to %1 file?").arg(title), tr("Your changes will be lost if you don't save them.")))
			{
			default:
			case MessageForwarder::DialogResponse::Cancel:
				event->setComplete(false);
				dataSetIOCompleted(event);
				return;

			case MessageForwarder::DialogResponse::Save:
				event->chain(_fileMenu->save());
				connectFileEventCompleted(event);
				break;

			case MessageForwarder::DialogResponse::Discard:
				event->setComplete(true);
				dataSetIOCompleted(event);
				break;
			}
		}
		else
		{
			event->setComplete();
			dataSetIOCompleted(event);
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

	QString title = windowTitle();
	title.chop(1);

	switch(MessageForwarder::showSaveDiscardCancel(tr("Would you like to save your changes to %1 file?").arg(title), tr("Your changes will be lost if you don't save them.")))
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
	case MessageForwarder::DialogResponse::Discard:			return true;
	}
}

void MainWindow::closeVariablesPage()
{
	_columnModel->setVisible(false);
}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
	hideProgress();

	if (event->operation() == FileEvent::FileNew)
	{
	}
	else if (event->operation() == FileEvent::FileOpen)
	{
		if (event->isSuccessful())
		{
			populateUIfromDataSet();

			_package->setCurrentFile(event->path());

			if(event->osfPath() != "")
				_package->setFolder("OSF://" + event->osfPath()); //It is also set by setCurrentPath, but then we get some weirdlooking OSF path

			if (event->type() == Utils::FileType::jasp)
			{
				if(!_package->dataFilePath().empty() && !_package->dataFileReadOnly() && strncmp("http", _package->dataFilePath().c_str(), 4) != 0)
				{
					QString dataFilePath = QString::fromStdString(_package->dataFilePath());
					if (QFileInfo::exists(dataFilePath))
					{
						uint currentDataFileTimestamp = QFileInfo(dataFilePath).lastModified().toSecsSinceEpoch();
						if (currentDataFileTimestamp > _package->dataFileTimestamp())
						{
							setCheckAutomaticSync(true);
							_fileMenu->syncDataFile(dataFilePath);
						}
					}
					else
					{
						_package->setDataFilePath("");
					}
				}
				
				if(_package->databaseJson() != Json::nullValue)
					_package->databaseStartSynching(true);
			}
			else if(event->isDatabase()) //Not a jasp file, but a direct load from a database, make sure it starts synching if the user wants it to:
				_package->databaseStartSynching(false);

			if (resultXmlCompare::compareResults::theOne()->testMode())
			{				
				//Give it like 3secs to have the ribbon load and the engines to load the data
				QTimer::singleShot(3000, this, &MainWindow::startComparingResults);
			}
			else if(_reporter && !_reporter->isJaspFileNotDabaseOrSynching())
					emit exitSignal(12);
		}
		else
		{
			_package->reset();
			setWelcomePageVisible(true);

			MessageForwarder::showWarning(tr("Unable to open file because:\n%1").arg(event->message()));

			if (_openedUsingArgs)	emit exitSignal(3);

		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		bool testingAndSaving = resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->shouldSave();

		if (event->isSuccessful())
		{
			_package->setCurrentFile(event->path());
			if(event->osfPath() != "")
				_package->setFolder("OSF://" + event->osfPath()); //It is also set by setCurrentPath, but then we get some weirdlooking OSF path

			_package->setModified(false);

			if(testingAndSaving)
				std::cerr << "Tested and saved " << event->path().toStdString() << " succesfully!" << std::endl;

			if(_savingForClose)
				emit exitSignal(0);

		}
		else
		{
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
			_package->reset(false);
			_ribbonModel->showStatistics();

			if(!_applicationExiting)
				_engineSync->cleanRestart();

			if (_applicationExiting)	
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
}


void MainWindow::populateUIfromDataSet()
{
	JASPTIMER_SCOPE(MainWindow::populateUIfromDataSet);
	bool errorFound = false;
	stringstream errorMsg;

	_resultsJsInterface->setScrollAtAll(false);

	_analyses->loadAnalysesFromDatasetPackage(errorFound, errorMsg, _ribbonModel);

	if (_analyses->count() == 1 && !resultXmlCompare::compareResults::theOne()->testMode()) //I do not want to see QML forms in unit test mode to make sure stuff breaks when options are changed
		(*_analyses)[0]->expandAnalysis(); //Show options for only analysis

	bool hasAnalyses = _analyses->count() > 0;

	setDataAvailable(_package->dataSet() && (_package->dataSet()->rowCount() > 0 && _package->dataSet()->columnCount() > 0));

	hideProgress();

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
	_qmlLoaded = true;
	emit qmlLoadedChanged();
	
	handleDeferredFileLoad();
}

void MainWindow::resultsPageLoaded()
{
	Log::log() << "MainWindow::resultsPageLoaded()" << std::endl;
	_resultsPageLoaded = true;
	
	handleDeferredFileLoad();
}

void MainWindow::handleDeferredFileLoad()
{
	if( !(_qmlLoaded && _resultsPageLoaded))
		return;
			
	if (_openOnLoadFilename != "")
		QTimer::singleShot(0, this, &MainWindow::_openFile); // this timer solves a resizing issue with the webengineview (https://github.com/jasp-stats/jasp-test-release/issues/70)
	
	if(!_openOnLoadDbJson.isNull())
		QTimer::singleShot(0, this, &MainWindow::_openDbJson);
}

void MainWindow::_openFile()
{
	_fileMenu->open(_openOnLoadFilename);
	_openOnLoadFilename = "";
}

void MainWindow::_openDbJson()
{
	_fileMenu->open(_openOnLoadDbJson);
	_openOnLoadDbJson = Json::nullValue;
}

void MainWindow::openGitHubBugReport() const
{
	bool openGitHubUserRegistration = false;

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

	std::stringstream fillIt;

	try			{ fillIt << "* JASP version: " << AppInfo::version.asString()	<< std::endl; }
	catch(...)	{ fillIt << "* JASP version: ???\n"; }

	try			{ fillIt <<	"* OS name and version: " << QSysInfo::prettyProductName() << std::endl; }
	catch(...)	{ fillIt << "* OS name and version: ???\n"; }

	fillIt	<<	"<!--- Please fill in the following fields: -->\n"
				"* Analysis: \n"
				"* Bug description:\n"
				"* Expected behaviour:\n"
				"<!--- Steps to reproduce means, what actions should we take in JASP to reproduce the bug you encountered? --->\n"
				"#### Steps to reproduce:\n"
				"1. Go to '...'\n"
				"2. Click on '....'\n"
				"3. Scroll down to '....'\n"
				"4. See error\n";

	fillIt <<	"\n\n\n"
				"-----------------------------------------------------------------------\n"
				"<!--- A note from the developers:\nIf possible please attach your data and/or JASP file to the issue, this makes solving the bug a lot easier."
				" If you would prefer to not make your data publicly available then you could also mail us.\n"
				"Note that github requires you to zip the file to upload it here.\n-->\n\n"
				"### Debug information:\n" << _engineSync->currentStateForDebug();

	try			{ fillIt << "\n[Commit used](" << AboutModel::commitUrl() << ")\n"; }
	catch(...)	{ fillIt << "Commit couldn't be found\n"; }

	try
	{
		QString percentEncodedIssue = QUrl::toPercentEncoding(tq(fillIt.str()));

		const char * baseIssueUrl = "https://github.com/jasp-stats/jasp-issues/issues/new?labels=bug&title=JASP+crashed&body=";

		QUrl issueUrl = baseIssueUrl + percentEncodedIssue;

		QDesktopServices::openUrl(issueUrl);

		if(openGitHubUserRegistration)
			QTimer::singleShot(0, []()
			{
				QDesktopServices::openUrl(QUrl("https://github.com/join"));
			});

		emit exitSignal(1);
	}
	catch(...)
	{
		MessageForwarder::showWarning(tr("GitHub couldn't be openend for you"), tr("Something went wrong with leading you to GitHub..\nYou can still report the bug by going to https://github.com/jasp-stats/jasp-issues/issues"));
		emit exitSignal(1);
	}
}

void MainWindow::fatalError()
{
	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;
		if(MessageForwarder::showYesNo(tr("Error"), tr("JASP has experienced an unexpected internal error:\n%1").arg(_fatalError) + "\n\n" +
			tr("JASP cannot continue and will close.\n\nWe would be grateful if you could report this error to the JASP team."), tr("Report"), tr("Exit")))
		{
			//QDesktopServices::openUrl(QUrl("https://jasp-stats.org/bug-reports/"));
			openGitHubBugReport();
		}
		else
			emit exitSignal(2);
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
		file.open(QIODevice::WriteOnly | QIODevice::Truncate);
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
	QString dataFilePath = QString::fromStdString(_package->dataFilePath());

	if (
			(dataFilePath.isEmpty() || _package->manualEdits())
			|| dataFilePath.startsWith("http")
			|| !QFileInfo::exists(dataFilePath)
			|| Utils::getFileSize(dataFilePath.toStdString()) == 0
			|| _package->dataFileReadOnly()
	)
	{
		QString									message = tr("JASP was started without associated data file (csv, sav or ods file). But to edit the data, JASP starts a spreadsheet editor based on this file and synchronize the data when the file is saved. Does this data file exist already, or do you want to generate it?");
		if (dataFilePath.startsWith("http"))	message = tr("JASP was started with an online data file (csv, sav or ods file). But to edit the data, JASP needs this file on your computer. Does this data file also exist on your computer, or do you want to generate it?");
		else if (_package->dataFileReadOnly())	message = tr("JASP was started with a read-only data file (probably from the examples). But to edit the data, JASP needs to write to the data file. Does the same file also exist on your computer, or do you want to generate it?");

		MessageForwarder::DialogResponse choice;

		const bool manualEditsMode = _package->manualEdits() && !dataFilePath.isEmpty() && !_package->dataFileReadOnly();

		if (manualEditsMode)
		{
			message = tr("JASP has an associated data file, but you edited it. Would you like to reload from the associated data or generate a new file?");
			choice = MessageForwarder::showYesNoCancel(tr("Start Spreadsheet Editor"), message, tr("Generate Data File"), tr("Reload Data File"));
		}
		else
			choice = MessageForwarder::showYesNoCancel(tr("Start Spreadsheet Editor"), message, tr("Generate Data File"), tr("Find Data File"));


		FileEvent *event = nullptr;

		bool justOpenItAlready = false;

		switch(choice)
		{
		case MessageForwarder::DialogResponse::Save:
		case MessageForwarder::DialogResponse::Discard:
		case MessageForwarder::DialogResponse::Cancel:
			return false;


		case MessageForwarder::DialogResponse::Yes:
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

			QFileInfo pkgFile(_package->currentFile()); // dataFilePath might be empty, so take the current file (the file from which the workspace is loaded, that is a jasp or a data file)
			if (pkgFile.dir().exists() && !pkgFile.absolutePath().startsWith(AppDirs::examples())) //If the file was opened from a directory that exists and is not examples we use that as basis to open a csv
				name = pkgFile.dir().absoluteFilePath(_package->name().replace('#', '_') + ".csv");

			dataFilePath = MessageForwarder::browseSaveFile(caption, name, filter);

			if (dataFilePath == "")
				return false;

			if (!dataFilePath.endsWith(".csv", Qt::CaseInsensitive))
				dataFilePath.append(".csv");

			event = new FileEvent(this, FileEvent::FileGenerateData);
			break;
		}

		case MessageForwarder::DialogResponse::No:
		{
			if(manualEditsMode)
				justOpenItAlready = true;
			else
			{
				QString caption = "Find Data File";
				QString filter = "Data File (*.csv *.txt *.tsv *.sav *.ods *.xls *.xlsx)";

				dataFilePath = MessageForwarder::browseOpenFile(caption, "", filter);
				if (dataFilePath == "")
					return false;

				event = new FileEvent(this, FileEvent::FileSyncData);
			}

			break;
		}

		}

		if(!justOpenItAlready)
		{
			connect(event, &FileEvent::completed, this,			&MainWindow::startDataEditorEventCompleted);
			connect(event, &FileEvent::completed, _fileMenu,	&FileMenu::setSyncFile);
			event->setPath(dataFilePath);
			_loader->io(event);
			showProgress();
		}
		else
		{
			startDataEditor(dataFilePath);
			_package->setSynchingExternally(true);
		}
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

}

/* the following does not seem to work: the new process crashes immediately... 
void MainWindow::restartJASP()
{
	QProcess::startDetached(QCoreApplication::applicationFilePath());
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
	hideProgress();

	if (event->isSuccessful())
	{
		_package->setDataFilePath(event->path().toStdString());
		_package->setDataFileReadOnly(false);
		_package->setModified(true);
		startDataEditor(event->path());
	}
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
		if (!QProcess::startDetached(appname, args))
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
					connect(event, &FileEvent::completed, this, &MainWindow::startDataEditorEventCompleted);
					connect(event, &FileEvent::completed, _fileMenu, &FileMenu::setSyncFile);
					event->setPath(path);
					_loader->io(event);
					showProgress();
				}
			}
		}
}

void MainWindow::showProgress()
{
	_fileMenu->setVisible(false);

	setProgressBarVisible(true);
}

void MainWindow::hideProgress()
{
	setProgressBarVisible(false);
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

	dataSetIORequestHandler(saveEvent);
}

bool MainWindow::enginesInitializing()
{
	return _engineSync->allEnginesInitializing();
}

void MainWindow::setProgressBarVisible(bool progressBarVisible)
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

void MainWindow::setDefaultWorkspaceEmptyValues()
{
	DataSetPackage::pkg()->setDefaultWorkspaceEmptyValues();
}

void MainWindow::resetVariableTypes()
{
	DataSetPackage::pkg()->resetVariableTypes();
}
