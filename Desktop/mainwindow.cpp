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
#include <QStringBuilder>
#include <QDesktopServices>
#include <QQmlContext>
#include <QQuickItem>
#include <QQuickStyle>
#include <QtWebEngine>
#include <QAction>
#include <QMenuBar>

#include <boost/filesystem.hpp>

#include "log.h"
#include "dirs.h"
#include "column.h"
#include "timers.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "sharedmemory.h"

#include "mainwindow.h"

#include "analysis/analysisform.h"
#include "analysis/jaspcontrol.h"
#include "widgets/checkboxbase.h"
#include "widgets/comboboxbase.h"
#include "widgets/textinputbase.h"
#include "widgets/componentslistbase.h"
#include "widgets/factorsformbase.h"
#include "widgets/inputlistbase.h"
#include "widgets/textareabase.h"
#include "widgets/sliderbase.h"
#include "widgets/expanderbuttonbase.h"
#include "widgets/variableslistbase.h"
#include "widgets/variablesformbase.h"
#include "widgets/factorlevellistbase.h"
#include "widgets/tableviewbase.h"
#include "widgets/radiobuttonbase.h"
#include "widgets/radiobuttonsgroupbase.h"
#include "analysis/jaspdoublevalidator.h"


#include "gui/jaspversionchecker.h"
#include "gui/preferencesmodel.h"
#include "gui/messageforwarder.h"

#include "modules/dynamicmodules.h"
#include "modules/analysismenumodel.h"
#include "modules/description/entrybase.h"

#include "qquick/datasetview.h"
#include "qquick/rcommander.h"

#include "resultstesting/compareresults.h"

#include "utilities/qutils.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "utilities/qmlutils.h"

#include "widgets/filemenu/filemenu.h"



using namespace std;
using namespace Modules;

MainWindow * MainWindow::_singleton	= nullptr;

MainWindow::MainWindow(QApplication * application) : QObject(application), _application(application)
{
	std::cout << "MainWindow constructor started" << std::endl;
	
	assert(!_singleton);
	_singleton = this;
	JASPTIMER_START(MainWindowConstructor);

	//This is the constructor, so _qml is not set yet and there is no need to check that with an if statement
	QQuickStyle::setStyle("Default");// Because otherwise plasma on kde might mess things up...

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
	_labelModel				= new LabelModel();
	
	initLog(); //initLog needs _preferences and _engineSync!

	Log::log() << "JASP " << AppInfo::version.asString() << " from commit " << AppInfo::gitCommit << " and branch " << AppInfo::gitBranch << " is continuing initialization." << std::endl;

	_resultsJsInterface		= new ResultsJsInterface();
	_odm					= new OnlineDataManager(this);
	_labelFilterGenerator	= new labelFilterGenerator(_labelModel, this);
	_columnsModel			= new ColumnsModel(_datasetTableModel);
	_computedColumnsModel	= new ComputedColumnsModel();
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

	new MessageForwarder(this); //We do not need to store this

	startOnlineDataManager();

	makeConnections();

	qmlRegisterUncreatableType<JASPControl>						("JASP",		1, 0 ,"JASP",				"Impossible to create JASP Object"	); //This is here to keep JASP.enum short I guess?
	qmlRegisterUncreatableType<MessageForwarder>				("JASP",		1, 0, "MessageForwarder",	"You can't touch this"				);

	qmlRegisterType<DataSetView>								("JASP",		1, 0, "DataSetView"						);
	qmlRegisterType<JaspTheme>									("JASP",		1, 0, "JaspTheme"						);
	qmlRegisterType<AnalysisForm>								("JASP",		1, 0, "AnalysisForm"					);
	qmlRegisterType<RCommander>									("JASP", 		1, 0, "RCommander"						);
	qmlRegisterType<JASPControl>								("JASP",		1, 0, "JASPControl"						);
	qmlRegisterType<ExpanderButtonBase>							("JASP",		1, 0, "ExpanderButtonBase"				);
	qmlRegisterType<CheckBoxBase>								("JASP",		1, 0, "CheckBoxBase"					);
	qmlRegisterType<SliderBase>									("JASP",		1, 0, "SliderBase"						);
	qmlRegisterType<TextInputBase>								("JASP",		1, 0, "TextInputBase"					);
	qmlRegisterType<TextAreaBase>								("JASP",		1, 0, "TextAreaBase"					);
	qmlRegisterType<ComboBoxBase>								("JASP",		1, 0, "ComboBoxBase"					);
	qmlRegisterType<RadioButtonBase>							("JASP",		1, 0, "RadioButtonBase"					);
	qmlRegisterType<RadioButtonsGroupBase>						("JASP",		1, 0, "RadioButtonsGroupBase"			);
	qmlRegisterType<ComponentsListBase>							("JASP",		1, 0, "ComponentsListBase"				);
	qmlRegisterType<FactorsFormBase>							("JASP",		1, 0, "FactorsFormBase"					);
	qmlRegisterType<InputListBase>								("JASP",		1, 0, "InputListBase"					);
	qmlRegisterType<FactorLevelListBase>						("JASP",		1, 0, "FactorLevelListBase"				);
	qmlRegisterType<VariablesListBase>							("JASP",		1, 0, "VariablesListBase"				);
	qmlRegisterType<VariablesFormBase>							("JASP",		1, 0, "VariablesFormBase"				);
	qmlRegisterType<TableViewBase>								("JASP",		1, 0, "TableViewBase"					);
	qmlRegisterType<JASPDoubleValidator>						("JASP",		1, 0, "JASPDoubleValidator"				);
	qmlRegisterType<ResultsJsInterface>							("JASP",		1, 0, "ResultsJsInterface"				);
	qmlRegisterType<LabelModel>									("JASP",		1, 0, "LabelModel"						);

	qmlRegisterUncreatableType<PlotEditor::AxisModel>			("JASP.PlotEditor",	1, 0, "AxisModel",					"Can't make it");
	qmlRegisterUncreatableType<PlotEditor::PlotEditorModel>		("JASP.PlotEditor",	1, 0, "PlotEditorModel",			"Can't make it");

	_dynamicModules->registerQMLTypes();

	QTimer::singleShot(0, [&]() { loadQML(); });

	_languageModel->setApplicationEngine(_qml);

	QString missingvaluestring = _settings.value("MissingValueList", "").toString();
	if (missingvaluestring != "")
		Utils::setEmptyValues(fromQstringToStdVector(missingvaluestring, "|"));

	_engineSync->start(_preferences->plotPPI());

	Log::log() << "JASP Desktop started and Engines initalized." << std::endl;

	JASPVersionChecker * jaspVersionChecker = new JASPVersionChecker(this);
	connect(jaspVersionChecker, &JASPVersionChecker::showDownloadButton, this, &MainWindow::setDownloadNewJASPUrl);

	JASPTIMER_FINISH(MainWindowConstructor);
}

MainWindow::~MainWindow()
{
	Log::log() << "MainWindow::~MainWindow()" << std::endl;

	_singleton = nullptr;

	try
	{
		//Clean up all QML to get rid of warnings and hopefully fix https://github.com/jasp-stats/jasp-issues/issues/667
		QList<QObject *> rootObjs = _qml->rootObjects();

		//Going backwards to make sure the theme isnt deleted before everything that depends on it
		for(int i=rootObjs.size() - 1; i >= 0; i--)
			delete rootObjs[i];

		delete _qml;

	}
	catch(...)	{}

	try
	{
		_engineSync->stopEngines();
		_odm->clearAuthenticationOnExit(OnlineDataManager::OSF);

		delete _resultsJsInterface;

		if (_package->hasDataSet())
			_package->reset();

		//delete _engineSync; it will be deleted by Qt!
	}
	catch(...)	{}
}

QString MainWindow::windowTitle() const
{
	return _package->windowTitle();
}

bool MainWindow::checkDoSync()
{
	if (checkAutomaticSync() && !MessageForwarder::showYesNo(tr("Datafile changed"), tr("The datafile that was used by this JASP file was modified. Do you want to reload the analyses with this new data?")))
	{
		_preferences->setDataAutoSynchronization(false);
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

	connect(_package,				&DataSetPackage::datasetChanged,					_filterModel,			&FilterModel::datasetChanged,								Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::datasetChanged,					_computedColumnsModel,	&ComputedColumnsModel::datasetChanged,						Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::datasetChanged,					_columnsModel,			&ColumnsModel::datasetChanged,								Qt::QueuedConnection);
	connect(_package,				&DataSetPackage::isModifiedChanged,					this,					&MainWindow::packageChanged									);
	connect(_package,				&DataSetPackage::windowTitleChanged,				this,					&MainWindow::windowTitleChanged								);
	connect(_package,				&DataSetPackage::columnDataTypeChanged,				_computedColumnsModel,	&ComputedColumnsModel::recomputeColumn						);
	connect(_package,				&DataSetPackage::freeDatasetSignal,					_loader,				&AsyncLoader::free											);
	connect(_package,				&DataSetPackage::checkDoSync,						_loader,				&AsyncLoader::checkDoSync,									Qt::DirectConnection); //Force DirectConnection because the signal is called from Importer which means it is running in AsyncLoaderThread...

	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_computedColumnsModel,	&ComputedColumnsModel::computeColumnSucceeded				);
	connect(_engineSync,			&EngineSync::computeColumnFailed,					_computedColumnsModel,	&ComputedColumnsModel::computeColumnFailed					);
	connect(_engineSync,			&EngineSync::engineTerminated,						this,					&MainWindow::fatalError,									Qt::QueuedConnection); //To give the process some time to realize it has crashed or something
	connect(_engineSync,			&EngineSync::columnDataTypeChanged,					_columnsModel,			&ColumnsModel::columnTypeChanged							);
	connect(_engineSync,			&EngineSync::refreshAllPlotsExcept,					_analyses,				&Analyses::refreshAllPlots									);
	connect(_engineSync,			&EngineSync::processNewFilterResult,				_filterModel,			&FilterModel::processFilterResult							);
	connect(_engineSync,			&EngineSync::processFilterErrorMsg,					_filterModel,			&FilterModel::processFilterErrorMsg							);
	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_filterModel,			&FilterModel::computeColumnSucceeded						);
	connect(_engineSync,			&EngineSync::moduleLoadingSucceeded,				_ribbonModel,			&RibbonModel::moduleLoadingSucceeded						);
	connect(_engineSync,			&EngineSync::plotEditorRefresh,						_plotEditorModel,		&PlotEditorModel::refresh									);

	qRegisterMetaType<columnType>();
	qRegisterMetaType<ListModel*>();

	connect(_computedColumnsModel,	&ComputedColumnsModel::sendComputeCode,				_engineSync,			&EngineSync::computeColumn,									Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::showAnalysisForm,			_analyses,				&Analyses::selectAnalysis									);
	connect(_computedColumnsModel,	&ComputedColumnsModel::dataColumnAdded,				_fileMenu,				&FileMenu::dataColumnAdded									);
	connect(_computedColumnsModel,	&ComputedColumnsModel::showAnalysisForm,			this,					&MainWindow::showResultsPanel								);

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
	connect(_analyses,				&Analyses::analysisImageEdited,						_plotEditorModel,		&PlotEditorModel::updateOptions							);

	connect(_fileMenu,				&FileMenu::exportSelected,							_resultsJsInterface,	&ResultsJsInterface::exportSelected							);
	connect(_fileMenu,				&FileMenu::dataSetIORequest,						this,					&MainWindow::dataSetIORequestHandler						);
	connect(_fileMenu,				&FileMenu::showAbout,								this,					&MainWindow::showAbout										);	

	connect(_odm,					&OnlineDataManager::progress,						this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);

	connect(_loader,				&AsyncLoader::progress,								this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);
	connect(_loader,				&AsyncLoader::checkDoSync,							this,					&MainWindow::checkDoSync,									Qt::BlockingQueuedConnection);

	connect(_preferences,			&PreferencesModel::missingValuesChanged,			_package,				&DataSetPackage::emptyValuesChangedHandler					);
	connect(_preferences,			&PreferencesModel::plotBackgroundChanged,			this,					&MainWindow::setImageBackgroundHandler						);
	connect(_preferences,			&PreferencesModel::plotPPIChanged,					this,					&MainWindow::plotPPIChangedHandler							);
	connect(_preferences,			&PreferencesModel::dataAutoSynchronizationChanged,	_fileMenu,				&FileMenu::dataAutoSynchronizationChanged					);
	connect(_preferences,			&PreferencesModel::exactPValuesChanged,				_resultsJsInterface,	&ResultsJsInterface::setExactPValuesHandler					);
	connect(_preferences,			&PreferencesModel::fixedDecimalsChangedString,		_resultsJsInterface,	&ResultsJsInterface::setFixDecimalsHandler					);
	connect(_preferences,			&PreferencesModel::uiScaleChanged,					_resultsJsInterface,	&ResultsJsInterface::setZoom								);
	connect(_preferences,			&PreferencesModel::developerModeChanged,			_analyses,				&Analyses::refreshAllAnalyses								);
	connect(_preferences,			&PreferencesModel::jaspThemeChanged,				this,					&MainWindow::jaspThemeChanged								);
	connect(_preferences,			&PreferencesModel::currentThemeNameChanged,			_resultsJsInterface,	&ResultsJsInterface::setThemeCss							);
	connect(_preferences,			&PreferencesModel::resultFontChanged,				_resultsJsInterface,	&ResultsJsInterface::setFontFamily							);
	connect(_preferences,			&PreferencesModel::resultFontChanged,				_engineSync,			&EngineSync::refreshAllPlots								);
	connect(_preferences,			&PreferencesModel::restartAllEngines,				_engineSync,			&EngineSync::haveYouTriedTurningItOffAndOnAgain				);

	connect(_filterModel,			&FilterModel::refreshAllAnalyses,					_analyses,				&Analyses::refreshAllAnalyses,								Qt::QueuedConnection);
	connect(_filterModel,			&FilterModel::updateColumnsUsedInConstructedFilter, _package,				&DataSetPackage::setColumnsUsedInEasyFilter					);
	connect(_filterModel,			&FilterModel::filterUpdated,						_package,				&DataSetPackage::refresh									);
	connect(_filterModel,			&FilterModel::sendFilter,							_engineSync,			&EngineSync::sendFilter										);
	connect(_filterModel,			&FilterModel::updateGeneratedFilterWithR,			_labelFilterGenerator,	&labelFilterGenerator::easyFilterConstructorRCodeChanged	);

	connect(_labelFilterGenerator,	&labelFilterGenerator::setGeneratedFilter,			_filterModel,			&FilterModel::setGeneratedFilter							);

	connect(_ribbonModel,			&RibbonModel::analysisClickedSignal,				_analyses,				&Analyses::analysisClickedHandler							);
	connect(_ribbonModel,			&RibbonModel::showRCommander,						this,					&MainWindow::showRCommander									);

	connect(_dynamicModules,		&DynamicModules::dynamicModuleUnloadBegin,			_analyses,				&Analyses::removeAnalysesOfDynamicModule					);
	connect(_dynamicModules,		&DynamicModules::dynamicModuleChanged,				_analyses,				&Analyses::refreshAnalysesOfDynamicModule					);
	connect(_dynamicModules,		&DynamicModules::dynamicModuleReplaced,				_analyses,				&Analyses::replaceAnalysesOfDynamicModule					);
	connect(_dynamicModules,		&DynamicModules::descriptionReloaded,				_analyses,				&Analyses::rescanAnalysisEntriesOfDynamicModule				);
	connect(_dynamicModules,		&DynamicModules::reloadHelpPage,					_helpModel,				&HelpModel::reloadPage										);
	connect(_dynamicModules,		&DynamicModules::moduleEnabledChanged,				_preferences,			&PreferencesModel::moduleEnabledChanged						);
	connect(_dynamicModules,		&DynamicModules::loadModuleTranslationFile,			_languageModel,			&LanguageModel::loadModuleTranslationFiles					);
	connect(_dynamicModules,		&DynamicModules::requestRootContext,				this,					&MainWindow::giveRootQmlContext,							Qt::UniqueConnection);
	connect(_dynamicModules,		&DynamicModules::loadQmlData,						this,					&MainWindow::loadQmlData,									Qt::UniqueConnection);
	connect(_dynamicModules,		&DynamicModules::reloadQmlImportPaths,				this,					&MainWindow::setQmlImportPaths,								Qt::QueuedConnection); //If this is queued this should make the loadingprocess of qml a bit less weird I think.

	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_fileMenu,				&FileMenu::refresh											);
	connect(_languageModel,			&LanguageModel::aboutToChangeLanguage,				_analyses,				&Analyses::prepareForLanguageChange							);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_analyses,				&Analyses::languageChangedHandler,							Qt::QueuedConnection);
	connect(_languageModel,			&LanguageModel::currentLanguageChanged,				_helpModel,				&HelpModel::generateJavascript,								Qt::QueuedConnection);
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

	_qml->rootContext()->setContextProperty("mainWindow",				this					);
	_qml->rootContext()->setContextProperty("labelModel",				_labelModel				);
	_qml->rootContext()->setContextProperty("aboutModel",				_aboutModel				);
	_qml->rootContext()->setContextProperty("dataSetModel",				_datasetTableModel		);
	_qml->rootContext()->setContextProperty("columnsModel",				_columnsModel			);
	_qml->rootContext()->setContextProperty("analysesModel",			_analyses				);
	_qml->rootContext()->setContextProperty("dynamicModules",			_dynamicModules			);
	_qml->rootContext()->setContextProperty("plotEditorModel",			_plotEditorModel		);
	_qml->rootContext()->setContextProperty("preferencesModel",			_preferences			);
	_qml->rootContext()->setContextProperty("resultsJsInterface",		_resultsJsInterface		);
	_qml->rootContext()->setContextProperty("computedColumnsInterface",	_computedColumnsModel	);
	_qml->rootContext()->setContextProperty("ribbonModelFiltered",		_ribbonModelFiltered	);
	_qml->rootContext()->setContextProperty("columnTypesModel",			_columnTypesModel		);
	_qml->rootContext()->setContextProperty("ribbonModelUncommon",		_ribbonModelUncommon	);
	_qml->rootContext()->setContextProperty("resultMenuModel",			_resultMenuModel		);
	_qml->rootContext()->setContextProperty("fileMenuModel",			_fileMenu				);
	_qml->rootContext()->setContextProperty("filterModel",				_filterModel			);
	_qml->rootContext()->setContextProperty("ribbonModel",				_ribbonModel			);
	_qml->rootContext()->setContextProperty("engineSync",				_engineSync				);
	_qml->rootContext()->setContextProperty("helpModel",				_helpModel				);
	_qml->rootContext()->setContextProperty("jaspTheme",				nullptr					); //Will be set from jaspThemeChanged()!
	_qml->rootContext()->setContextProperty("messages",					MessageForwarder::msgForwarder());
	_qml->rootContext()->setContextProperty("qmlUtils",					new QmlUtils(this)		);

	_qml->rootContext()->setContextProperty("baseBlockDim",				20); //should be taken from Theme
	_qml->rootContext()->setContextProperty("baseFontSize",				16);
	_qml->rootContext()->setContextProperty("languageModel",			_languageModel					);

	_qml->rootContext()->setContextProperty("columnTypeScale",			int(columnType::scale)			);
	_qml->rootContext()->setContextProperty("columnTypeOrdinal",		int(columnType::ordinal)		);
	_qml->rootContext()->setContextProperty("columnTypeNominal",		int(columnType::nominal)		);
	_qml->rootContext()->setContextProperty("columnTypeNominalText",	int(columnType::nominalText)	);

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

	_qml->setOutputWarningsToStandardError(true);

	setQmlImportPaths();

	QMetaObject::Connection exitOnFailConnection = connect(_qml, &QQmlApplicationEngine::objectCreated, [&](QObject * obj, QUrl url)
	{
		if(obj == nullptr)
		{
			std::cerr << "Could not load QML: " + url.toString().toStdString() << std::endl;
			exit(10);
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

	Log::log() << "Loading HelpWindow"  << std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/HelpWindow.qml"));
	Log::log() << "Loading AboutWindow" << std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/AboutWindow.qml"));
	Log::log() << "Loading MainWindow"  << std::endl; _qml->load(QUrl("qrc:///components/JASP/Widgets/MainWindow.qml"));

	connect(_preferences, &PreferencesModel::uiScaleChanged,		DataSetView::lastInstancedDataSetView(), &DataSetView::viewportChanged, Qt::QueuedConnection);
	connect(_preferences, &PreferencesModel::interfaceFontChanged,	DataSetView::lastInstancedDataSetView(), &DataSetView::viewportChanged, Qt::QueuedConnection);

	Log::log() << "QML Initialized!"  << std::endl;

	Log::log() << "Loading upgrades definitions"  << std::endl;
	_upgrader->loadOldSchoolUpgrades();

	//And now we disconnect the exit on fail lambda because we won't be needing it later
	disconnect(exitOnFailConnection);

	//Load the ribbonmodel modules now because we have an actual qml context to do so in.
	_ribbonModel->loadModules(	
		{ 	"jaspDescriptives", "jaspTTests", "jaspAnova", "jaspMixedModels", "jaspRegression", "jaspFrequencies", "jaspFactor" },
		{ 	"jaspAudit", "jaspBain", "jaspCircular", "jaspDistributions" , "jaspEquivalenceTTests", "jaspJags", "jaspLearnBayes", "jaspMachineLearning",
			"jaspMetaAnalysis", "jaspNetwork"/*, "jaspProcessControl"*/, "jaspProphet", "jaspReliability", "jaspSem", "jaspSummaryStatistics", "jaspVisualModeling" });

	_engineSync->loadAllActiveModules();
	_dynamicModules->startUpCompleted();
}

void MainWindow::setQmlImportPaths()
{
	static QStringList originalImportPaths = _qml->importPathList();

	QStringList newImportPaths = originalImportPaths;

	newImportPaths.append("qrc:///components");
	newImportPaths.append(_dynamicModules->importPaths());

	_qml->setImportPathList(newImportPaths);

	Log::log() << "QML has the following import paths:\n";

	for(const QString & p : _qml->importPathList())
		Log::log() << "\t" << p << "\n";
	Log::log() << std::endl;
}

QObject * MainWindow::loadQmlData(QString data, QUrl url)
{
	QObject *	createdObject = nullptr;
	bool		lambdaCalled = false;

	QMetaObject::Connection returnTheObjectConn = connect(_qml, &QQmlApplicationEngine::objectCreated, [&](QObject * obj, QUrl url)
	{
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

		_resultsJsInterface->resetResults();//To reload page

		QTimer::singleShot(500, this, &MainWindow::resendResultsToWebEngine);
	}
}

void MainWindow::resendResultsToWebEngine()
{
	//Make sure the result are reloaded after triggering a qml wipe
	_analyses	-> applyToAll([&](Analysis * a){ emit a->resultsChangedSignal(a); });
}

void MainWindow::jaspThemeChanged(JaspTheme * newTheme)
{
	_qml->rootContext()->setContextProperty("jaspTheme",				newTheme);
}

void MainWindow::initLog()
{
	assert(_engineSync != nullptr && _preferences != nullptr);

	Log::logFileNameBase = (AppDirs::logDir() + "JASP "  + getSortableTimestamp()).toStdString();
	Log::initRedirects();
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
	if (_resultsViewLoaded)	_fileMenu->open(filepath);
	else					_openOnLoadFilename = filepath;
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
	if (event->operation() == FileEvent::FileOpen)
	{
		if (_package->isLoaded() || _analyses->count() > 0) //If no data is loaded but we have analyses then we probably want to play with summary stats or something. So lets just open in a new instance.
		{
			// If this instance has a valid OSF connection save this setting for a new instance
			_odm->savePasswordFromAuthData(OnlineDataManager::OSF);

			// begin new instance
			QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(event->path()));
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

		if(!event->path().endsWith(".pdf"))
		{
			if(_preferences->currentThemeName() != "lightTheme")
				_resultsJsInterface->setThemeCss("lightTheme");
			_resultsJsInterface->exportHTML();
		}

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
		if (_package->isModified())
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

		_resultsJsInterface->resetResults();
		setDataPanelVisible(false);
		setDataAvailable(false);
		setWelcomePageVisible(true);

		closeVariablesPage();
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

	switch(MessageForwarder::showSaveDiscardCancel(tr("File has changed"), tr("Save changes to the file %1 before closing?\n\nYour changes will be lost if you don't save them.").arg(title)))
	{
	case MessageForwarder::DialogResponse::Save:
	{
		FileEvent * saveEvent = _fileMenu->save();

		if(saveEvent->isCompleted())	return saveEvent->isSuccessful();
		else							_savingForClose = true;
	}
	[[clang::fallthrough]];

	case MessageForwarder::DialogResponse::Cancel:			return false;

	default:												[[clang::fallthrough]];
	case MessageForwarder::DialogResponse::Discard:			return true;
	}
}

void MainWindow::closeVariablesPage()
{
	_labelModel->setVisible(false);
}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
	hideProgress();

	if (event->operation() == FileEvent::FileOpen)
	{
		if (event->isSuccessful())
		{
			populateUIfromDataSet();

			_package->setCurrentFile(event->path());

			if(event->osfPath() != "")
				_package->setFolder("OSF://" + event->osfPath()); //It is also set by setCurrentPath, but then we get some weirdlooking OSF path

			if (event->type() == Utils::FileType::jasp && !_package->dataFilePath().empty() && !_package->dataFileReadOnly() && strncmp("http", _package->dataFilePath().c_str(), 4) != 0)
			{
				QString dataFilePath = QString::fromStdString(_package->dataFilePath());
				if (QFileInfo::exists(dataFilePath))
				{
					uint currentDataFileTimestamp = QFileInfo(dataFilePath).lastModified().toTime_t();
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

			if (resultXmlCompare::compareResults::theOne()->testMode())
			{
				//Make sure the engine gets enough time to load data
				_engineSync->pauseEngines();
				_engineSync->resumeEngines();

				//Also give it like 3secs to have the ribbon load
				QTimer::singleShot(3000, this, &MainWindow::startComparingResults);
			}

		}
		else
		{
			_package->reset();
			setWelcomePageVisible(true);

			MessageForwarder::showWarning(tr("Unable to open file because:\n%1").arg(event->message()));

			if (_openedUsingArgs)	exit(3);

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
				exit(0);

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
			_analyses->setVisible(false);
			_analyses->clear();
			_package->reset();

			setWelcomePageVisible(true);
			_engineSync->cleanUpAfterClose();

			if (_applicationExiting)	QApplication::exit();
			else
			{
				setDataPanelVisible(false);
				setDataAvailable(false);
			}
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
		emit (*_analyses)[0]->expandAnalysis(); //Show options for only analysis

	bool hasAnalyses = _analyses->count() > 0;

	setDataAvailable((_package->rowCount() > 0 || _package->columnCount() > 0));

	hideProgress();

	if(!_dataAvailable)	setDataPanelVisible(false);
	else				setDataPanelVisible(!hasAnalyses);

	_analyses->setVisible(hasAnalyses && !resultXmlCompare::compareResults::theOne()->testMode());

	if (_package->warningMessage() != "")	MessageForwarder::showWarning(_package->warningMessage());
	else if (errorFound)					MessageForwarder::showWarning(errorMsg.str());

	matchComputedColumnsToAnalyses();

	_package->setLoaded();
	checkUsedModules();

	_resultsJsInterface->setScrollAtAll(true);
}

void MainWindow::checkUsedModules()
{
	_analyses->applyToAll([&](Analysis * analysis)
	{
		if(_ribbonModel->isModuleName(analysis->module()))
			_ribbonModel->ribbonButtonModel(analysis->module())->setEnabled(true);
	});
}

void MainWindow::matchComputedColumnsToAnalyses()
{
	for(ComputedColumn * col : *ComputedColumns::singleton())
		if(col->analysisId() != -1)
			col->setAnalysis(_analyses->get(col->analysisId()));
}


void MainWindow::resultsPageLoaded()
{
	_resultsViewLoaded = true;

	if (_openOnLoadFilename != "")
		QTimer::singleShot(0, this, &MainWindow::_openFile); // this timer solves a resizing issue with the webengineview (https://github.com/jasp-stats/jasp-test-release/issues/70)
}

void MainWindow::_openFile()
{
    _fileMenu->open(_openOnLoadFilename);
    _openOnLoadFilename = "";
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
			QTimer::singleShot(100, []()
			{
				QDesktopServices::openUrl(QUrl("https://github.com/join"));
				exit(1);
			});
		else
			exit(1);
	}
	catch(...)
	{
		MessageForwarder::showWarning(tr("GitHub couldn't be openend for you"), tr("Something went wrong with leading you to GitHub..\nYou can still report the bug by going to https://github.com/jasp-stats/jasp-issues/issues"));
		exit(1);
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
			exit(2);
	}
}

void MainWindow::saveTextToFileHandler(const QString &filename, const QString &data)
{
	if (filename == "%PREVIEW%" || filename == "%EXPORT%")
	{
		_package->setAnalysesHTML(fq(data));
		_package->setAnalysesHTMLReady();

		finishComparingResults();
	}
	else
	{
		QFile file(filename);
		file.open(QIODevice::WriteOnly | QIODevice::Truncate);
		QTextStream stream(&file);
		stream.setCodec("UTF-8");

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

void MainWindow::startDataEditorHandler()
{
	setCheckAutomaticSync(false);
	QString path = QString::fromStdString(_package->dataFilePath());
	if (path.isEmpty() || path.startsWith("http") || !QFileInfo::exists(path) || Utils::getFileSize(path.toStdString()) == 0 || _package->dataFileReadOnly())
	{
		QString									message = tr("JASP was started without associated data file (csv, sav or ods file). But to edit the data, JASP starts a spreadsheet editor based on this file and synchronize the data when the file is saved. Does this data file exist already, or do you want to generate it?");
		if (path.startsWith("http"))			message = tr("JASP was started with an online data file (csv, sav or ods file). But to edit the data, JASP needs this file on your computer. Does this data file also exist on your computer, or do you want to generate it?");
		else if (_package->dataFileReadOnly())	message = tr("JASP was started with a read-only data file (probably from the examples). But to edit the data, JASP needs to write to the data file. Does the same file also exist on your computer, or do you want to generate it?");

		MessageForwarder::DialogResponse choice = MessageForwarder::showYesNoCancel(tr("Start Spreadsheet Editor"), message, tr("Generate Data File"), tr("Find Data File"));

		FileEvent *event = nullptr;

		switch(choice)
		{
		case MessageForwarder::DialogResponse::Cancel:
			return;


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

			QFileInfo pkgFile(_package->currentFile());
			if (pkgFile.dir().exists() && !pkgFile.absolutePath().startsWith(AppDirs::examples())) //If the file was opened from a directory that exists and is not examples we use that as basis to open a csv
				name = pkgFile.dir().absoluteFilePath(_package->name().replace('#', '_') + ".csv");

			path = MessageForwarder::browseSaveFile(caption, name, filter);

			if (path == "")
				return;

			if (!path.endsWith(".csv", Qt::CaseInsensitive))
				path.append(".csv");

			event = new FileEvent(this, FileEvent::FileGenerateData);
			break;
		}

		case MessageForwarder::DialogResponse::No:
		{
			QString caption = "Find Data File";
			QString filter = "Data File (*.csv *.txt *.tsv *.sav *.ods)";

			path = MessageForwarder::browseOpenFile(caption, "", filter);
			if (path == "")
				return;

			event = new FileEvent(this, FileEvent::FileSyncData);
			break;
		}

		}
		connect(event, &FileEvent::completed, this, &MainWindow::startDataEditorEventCompleted);
		connect(event, &FileEvent::completed, _fileMenu, &FileMenu::setSyncFile);
		event->setPath(path);
		_loader->io(event);
		showProgress();
	}
	else
		startDataEditor(path);
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

	QString startProcess;
	if (!useDefaultSpreadsheetEditor)
	{
#ifdef __APPLE__
		appname = appname.mid(appname.lastIndexOf('/') + 1);
		startProcess = "open -a \"" + appname + "\" \"" + path + "\"";
#else
		startProcess = "\"" + appname + "\" \"" + path + "\"";
#endif
		if (!QProcess::startDetached(startProcess))
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

void MainWindow::unitTestTimeOut()
{
	std::cerr << "Time out for unit test!" << std::endl;
	exit(3);
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
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->refreshed())
	{
		bool allCompleted = true;

		_analyses->applyToSome([&](Analysis * analysis)
		{
			if(!analysis->isFinished())
			{
				allCompleted = false;
				return false;
			}
			return true;
		});

		if(allCompleted)
		{
			_resultsJsInterface->exportPreviewHTML();
			resultXmlCompare::compareResults::theOne()->setExportCalled();
		}
	}
}

void MainWindow::finishComparingResults()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->exportCalled() && !resultXmlCompare::compareResults::theOne()->comparedAlready())
	{
		std::string resultHtml = _package->analysesHTML();
		resultXmlCompare::compareResults::theOne()->setRefreshResult(QString::fromStdString(resultHtml));

		resultXmlCompare::compareResults::theOne()->compare();

		if(resultXmlCompare::compareResults::theOne()->shouldSave())
			emit saveJaspFile();
		else
			exit(resultXmlCompare::compareResults::theOne()->compareSucces() ? 0 : 1);
	}
}

void MainWindow::finishSavingComparedResults()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->shouldSave())
	{
		exit(resultXmlCompare::compareResults::theOne()->compareSucces() ? 0 : 1);
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

void MainWindow::setDataPanelVisible(bool dataPanelVisible)
{
	if (_dataPanelVisible == dataPanelVisible)
		return;

	_dataPanelVisible = dataPanelVisible;
	emit dataPanelVisibleChanged(_dataPanelVisible);
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

	if(!_analysesAvailable && !dataPanelVisible())
		setDataPanelVisible(true);
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

	connect(macQuit,		&QAction::triggered, [&](){ if(checkPackageModifiedBeforeClosing()) _application->quit(); });
	connect(macAbout,		&QAction::triggered, [&](){ showAbout(); });
	connect(macPreferences, &QAction::triggered, [&](){ _fileMenu->showPreferences(); });

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
			;
}
