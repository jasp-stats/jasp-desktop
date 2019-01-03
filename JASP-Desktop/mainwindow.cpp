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

#include "mainwindow.h"

#include "analysis/analysisform.h"

#include <QDir>
#include <QDebug>
#include <QFile>
#include <QFileInfo>
#include <QShortcut>
#include <QStringBuilder>
#include <QDesktopServices>
#include <QQmlContext>
#include <QQuickItem>

#include "utilities/qutils.h"
#include "utilities/appdirs.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "appinfo.h"

#include "gui/aboutdialog.h"
#include "gui/preferencesdialog.h"
#include <boost/filesystem.hpp>
#include "dirs.h"
#include "utilities/qutils.h"
#include "column.h"
#include "sharedmemory.h"
#include "utilities/settings.h"

#include "analysis/options/optionvariablesgroups.h"
#include "qquick/datasetview.h"
#include "modules/dynamicmodules.h"
#include "modules/ribbonentry.h"
#include "modules/analysismenumodel.h"

#include "timers.h"
#include "resultstesting/compareresults.h"
#include "widgets/filemenu/filemenu.h"
#include "gui/messageforwarder.h"

using namespace std;

QString MainWindow::iconPath = "qrc:/icons/";
QMap<QString, QVariant> MainWindow::iconFiles {
	{ "nominalText"	, iconPath + "variable-nominal-text.svg" },
	{ "nominal"		, iconPath + "variable-nominal.svg"},
	{ "ordinal"		, iconPath + "variable-ordinal.svg"},
	{ "scale"		, iconPath + "variable-scale.svg"}
};

QMap<QString, QVariant> MainWindow::iconInactiveFiles {
	{ "nominalText"	, iconPath + "variable-nominal-inactive.svg" },
	{ "nominal"		, iconPath + "variable-nominal-inactive.svg"},
	{ "ordinal"		, iconPath + "variable-ordinal-inactive.svg"},
	{ "scale"		, iconPath + "variable-scale-inactive.svg"}
};

QMap<int, QString> MainWindow::columnTypeMap {
	{ Column::ColumnTypeNominalText	, "nominalText" },
	{ Column::ColumnTypeNominal		, "nominal"},
	{ Column::ColumnTypeOrdinal		, "ordinal"},
	{ Column::ColumnTypeScale		, "scale"}
};


MainWindow::MainWindow(QApplication * application) : QObject(application), _application(application)
{
	JASPTIMER_START(MainWindowConstructor);

	TempFiles::init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	_resultsJsInterface		= new ResultsJsInterface();
	_package				= new DataSetPackage();
	_odm					= new OnlineDataManager(this);
	_tableModel				= new DataSetTableModel();
	_levelsTableModel		= new LevelsTableModel(this);
	_labelFilterGenerator	= new labelFilterGenerator(_package, this);
	_columnsModel			= new ColumnsModel(this);
	_analyses				= new Analyses(this);
	_dynamicModules			= new DynamicModules(this);
	_engineSync				= new EngineSync(_analyses, _package, _dynamicModules, this);
	_computedColumnsModel	= new ComputedColumnsModel(_analyses, this);
	_filterModel			= new FilterModel(_package, this);
	_ribbonModel			= new RibbonModel(this);
	_fileMenu				= new FileMenu(this);

	new MessageForwarder(this); //We do not need to store this

	StartOnlineDataManager();

	// Add static modules
	setupRibbonModels(QFileInfo(tq(Dirs::resourcesDir() + "Common/"	)));
	setupRibbonModels(QFileInfo(tq(Dirs::resourcesDir() + "Summary Statistics/" )));
	setupRibbonModels(QFileInfo(tq(Dirs::resourcesDir() + "Network/"			)));
	setupRibbonModels(QFileInfo(tq(Dirs::resourcesDir() + "Meta Analysis/"		)));
	setupRibbonModels(QFileInfo(tq(Dirs::resourcesDir() + "SEM/"				)));

	// Add dynamic modules and connect signals to slots
	_ribbonModel->connectToDynamicModules(_dynamicModules);

	//initQWidgetGUIParts();
	makeConnections();

	// Set the initial tab on Common.
	//showMainPage();

	qmlRegisterType<DataSetView>		("JASP", 1, 0, "DataSetView");
	qmlRegisterType<AnalysisForm>		("JASP", 1, 0, "AnalysisForm");
	qmlRegisterType<ResultsJsInterface>	("JASP", 1, 0, "ResultsJsInterface");

	loadQML();

	QString missingvaluestring = _settings.value("MissingValueList", "").toString();
	if (missingvaluestring != "")
		Utils::setEmptyValues(fromQstringToStdVector(missingvaluestring, "|"));

	JASPTIMER_FINISH(MainWindowConstructor);
}

MainWindow::~MainWindow()
{
	delete _resultsJsInterface;
	delete _engineSync;
	if (_package && _package->dataSet())
	{
		_loader.free(_package->dataSet());
		_package->reset();
	}
}

void MainWindow::StartOnlineDataManager()
{
	_loader.moveToThread(&_loaderThread);
	_loaderThread.start();
	_loader.setOnlineDataManager(_odm);

	_fileMenu->setOnlineDataManager(_odm);

}

#define CONNECT_SHORTCUT(shortcut, method) connect(new QShortcut(QKeySequence(shortcut), this),	&QShortcut::activated,	this,	method);

Q_DECLARE_METATYPE(Column::ColumnType)

void MainWindow::makeConnections()
{
	_package->isModifiedChanged.connect(boost::bind(&MainWindow::packageChanged,		this,	_1));
	_package->dataChanged.connect(		boost::bind(&MainWindow::packageDataChanged,	this,	_1, _2, _3, _4, _5));
	_package->pauseEngines.connect(		boost::bind(&MainWindow::pauseEngines,			this));
	_package->resumeEngines.connect(	boost::bind(&MainWindow::resumeEngines,			this));


	/*CONNECT_SHORTCUT("Ctrl+S",		&MainWindow::saveKeysSelected);
	CONNECT_SHORTCUT("Ctrl+O",		&MainWindow::openKeysSelected);
	CONNECT_SHORTCUT("Ctrl+Y",		&MainWindow::syncKeysSelected);
	CONNECT_SHORTCUT("Ctrl+T",		&MainWindow::refreshKeysSelected);
	CONNECT_SHORTCUT("Ctrl++",		&MainWindow::zoomInKeysSelected);
	CONNECT_SHORTCUT("Ctrl+-",		&MainWindow::zoomOutKeysSelected);
	CONNECT_SHORTCUT("Ctrl+=",		&MainWindow::zoomEqualKeysSelected);*/

	connect(this,					&MainWindow::saveJaspFile,							this,					&MainWindow::saveJaspFileHandler,							Qt::QueuedConnection);
	connect(this,					&MainWindow::refreshAllAnalyses,					_analyses,				&Analyses::refreshAllAnalyses								);

	connect(_levelsTableModel,		&LevelsTableModel::resizeLabelColumn,				this,					&MainWindow::resizeVariablesWindowLabelColumn				);
	connect(_levelsTableModel,		&LevelsTableModel::labelFilterChanged,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged					);
	connect(_levelsTableModel,		&LevelsTableModel::notifyColumnHasFilterChanged,	_tableModel,			&DataSetTableModel::notifyColumnFilterStatusChanged			);
	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModels,			_tableModel,			&DataSetTableModel::refreshColumn							);
	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModelsByName,	_computedColumnsModel,	&ComputedColumnsModel::checkForDependentColumnsToBeSentSlot	);

	connect(_tableModel,			&DataSetTableModel::dataSetChanged,					this,					&MainWindow::dataSetChanged									);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged					);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_levelsTableModel,		&LevelsTableModel::refresh,									Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::modelReset,						_levelsTableModel,		&LevelsTableModel::refresh,									Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::headerDataChanged,				_columnsModel,			&ColumnsModel::datasetHeaderDataChanged						);
	connect(_tableModel,			&DataSetTableModel::modelReset,						_columnsModel,			&ColumnsModel::refresh										);
	connect(_tableModel,			&DataSetTableModel::columnDataTypeChanged,			_computedColumnsModel,	&ComputedColumnsModel::recomputeColumn						);

	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_computedColumnsModel,	&ComputedColumnsModel::computeColumnSucceeded				);
	connect(_engineSync,			&EngineSync::computeColumnFailed,					_computedColumnsModel,	&ComputedColumnsModel::computeColumnFailed					);
	connect(_engineSync,			&EngineSync::processNewFilterResult,				_filterModel,			&FilterModel::processFilterResult							);
	connect(_engineSync,			&EngineSync::processFilterErrorMsg,					_filterModel,			&FilterModel::processFilterErrorMsg							);

	qRegisterMetaType<Column::ColumnType>();

	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshColumn,				_tableModel,			&DataSetTableModel::refreshColumn,							Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::headerDataChanged,			_tableModel,			&DataSetTableModel::headerDataChanged,						Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::sendComputeCode,				_engineSync,			&EngineSync::computeColumn,									Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshColumn,				_levelsTableModel,		&LevelsTableModel::refreshColumn,							Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::dataSetChanged,				_tableModel,			&DataSetTableModel::dataSetChanged							);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshData,					_tableModel,			&DataSetTableModel::refresh,								Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshData,					this,					&MainWindow::updateShownVariablesModel						);
	connect(_computedColumnsModel,	&ComputedColumnsModel::showAnalysisForm,			this,					&MainWindow::showForm										);

	connect(this,					&MainWindow::ppiChanged,							_engineSync,			&EngineSync::ppiChanged										);
	connect(this,					&MainWindow::imageBackgroundChanged,				_engineSync,			&EngineSync::imageBackgroundChanged							);

	connect(_resultsJsInterface,	&ResultsJsInterface::packageModified,				this,					&MainWindow::setPackageModified								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisUnselected,			this,					&MainWindow::analysisUnselectedHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisChangedDownstream,		this,					&MainWindow::analysisChangedDownstreamHandler				);
	connect(_resultsJsInterface,	&ResultsJsInterface::saveTextToFile,				this,					&MainWindow::saveTextToFileHandler							);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSelected,				this,					&MainWindow::analysisSelectedHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSaveImage,				this,					&MainWindow::analysisSaveImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisEditImage,				this,					&MainWindow::analysisEditImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::removeAnalysisRequest,			_analyses,				&Analyses::removeAnalysisById								);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsPageLoadedPpi,			this,					&MainWindow::resultsPageLoaded								);
	connect(_resultsJsInterface,	&ResultsJsInterface::ppiChanged,					this,					&MainWindow::ppiChangedHandler								);
	connect(_resultsJsInterface,	&ResultsJsInterface::openFileTab,					_fileMenu,				&FileMenu::showFileMenu										);

	connect(_analyses,				&Analyses::analysisResultsChanged,					this,					&MainWindow::analysisResultsChangedHandler					);
	connect(_analyses,				&Analyses::analysisImageSaved,						this,					&MainWindow::analysisImageSavedHandler						);
	connect(_analyses,				&Analyses::analysisAdded,							_fileMenu,				&FileMenu::analysisAdded									);
	connect(_analyses,				&Analyses::analysisImageEdited,						_resultsJsInterface,	&ResultsJsInterface::analysisImageEditedHandler				);

	connect(_fileMenu,				&FileMenu::exportSelected,							_resultsJsInterface,	&ResultsJsInterface::exportSelected							);
	connect(_fileMenu,				&FileMenu::dataSetIORequest,						this,					&MainWindow::dataSetIORequestHandler						);

	connect(_odm,					&OnlineDataManager::progress,						this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);
	connect(&_loader,				&AsyncLoader::progress,								this,					&MainWindow::setProgressStatus								);
	connect(_engineSync,			&EngineSync::engineTerminated,						this,					&MainWindow::fatalError										);
	//connect(_okButton,				&QPushButton::clicked,								this,					&MainWindow::analysisOKed									);
//	connect(_runButton,				&QPushButton::clicked,								this,					&MainWindow::analysisRunned									);
	//connect(ui->splitter,			&QSplitter::splitterMoved,							this,					&MainWindow::splitterMovedHandler							);
	//connect(ui->webViewHelp,		&CustomWebEngineView::loadFinished,					this,					&MainWindow::helpFirstLoaded								);

	/*connect(ui->tabBar,				&TabBar::currentChanged,							this,					&MainWindow::tabChanged										);
	connect(ui->tabBar,				&TabBar::helpToggled,								this,					&MainWindow::helpToggled									);
	connect(ui->tabBar,				&TabBar::dataAutoSynchronizationChanged,			_fileMenu,				&FileMenu::dataAutoSynchronizationChanged					);
	connect(ui->tabBar,				&TabBar::setExactPValuesHandler,					_resultsJsInterface,	&ResultsJsInterface::setExactPValuesHandler					);
	connect(ui->tabBar,				&TabBar::setFixDecimalsHandler,						_resultsJsInterface,	&ResultsJsInterface::setFixDecimalsHandler					);
	connect(ui->tabBar,				&TabBar::emptyValuesChangedHandler,					this,					&MainWindow::emptyValuesChangedHandler						);
	connect(ui->tabBar,				&TabBar::useDefaultPPIHandler,						_resultsJsInterface,	&ResultsJsInterface::getDefaultPPI							);*/

	connect(_filterModel,			&FilterModel::refreshAllAnalyses,					_analyses,				&Analyses::refreshAllAnalyses								);
	connect(_filterModel,			&FilterModel::updateColumnsUsedInConstructedFilter, _tableModel,			&DataSetTableModel::setColumnsUsedInEasyFilter				);
	connect(_filterModel,			&FilterModel::filterUpdated,						_tableModel,			&DataSetTableModel::refresh									);
	connect(_filterModel,			&FilterModel::sendFilter,							_engineSync,			&EngineSync::sendFilter										);

	connect(_filterModel,			&FilterModel::updateGeneratedFilterWithR,			_labelFilterGenerator,	&labelFilterGenerator::easyFilterConstructorRCodeChanged	);
	connect(_labelFilterGenerator,	&labelFilterGenerator::setGeneratedFilter,			_filterModel,			&FilterModel::setGeneratedFilter							);
	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_filterModel,			&FilterModel::computeColumnSucceeded						);

	connect(_dynamicModules,		&DynamicModules::showModuleInstallerWindow,			this,					&MainWindow::showQMLWindow									);
	connect(_ribbonModel,			&RibbonModel::analysisClickedSignal,				_analyses,				&Analyses::analysisClickedHandler							);
}

/*
void MainWindow::initQWidgetGUIParts()
{
	JASPTIMER_START(MainWindow::initQWidgetGUIParts());
	updateMenuEnabledDisabledStatus();

	ui->splitter->setSizes(QList<int>({575}));

	ui->tabBar->init(this, _dynamicModules, _ribbonModel);
	ui->tabBar->addModuleInstallerEntryToPlusMenu();

#ifdef __APPLE__
	_scrollbarWidth = 3;
#else
	_scrollbarWidth = qApp->style()->pixelMetric(QStyle::PM_ScrollBarExtent);
#endif

	_buttonPanel		= new QWidget(ui->panel_2_Options);
	_buttonPanelLayout	= new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, _buttonPanelLayout->contentsMargins().top() - 5, _buttonPanelLayout->contentsMargins().right(), 0);

	_buttonPanel->setLayout(_buttonPanelLayout);

	_runButton	= new QPushButton(QString("Run"), _buttonPanel);
	_okButton	= new QPushButton(QString("OK"), _buttonPanel);
	_okButton->setDefault(true);

	QMenuBar *_mMenuBar = new QMenuBar(0);
	QMenu *aboutMenu	= _mMenuBar->addMenu("JASP");
	aboutMenu->addAction("About",ui->tabBar,SLOT(showAbout()));
	_mMenuBar->addMenu(aboutMenu);

	_buttonPanelLayout->addWidget(_okButton);
	//_buttonPanelLayout->addWidget(_runButton);
	//_buttonPanelLayout->addStretch();

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);

	_tableViewWidthBeforeOptionsMadeVisible = -1;

	ui->webViewHelp->load(QUrl::fromLocalFile(AppDirs::help() + "/index.html"));

	ui->panel_4_Help->hide();

	setAcceptDrops(true);

	ui->panel_1_Data->hide();
	ui->panel_2_Options->hide();

	JASPTIMER_FINISH(MainWindow::initQWidgetGUIParts());
}*/

void MainWindow::loadQML()
{
	_qml = new QQmlApplicationEngine(this);

	_qml->rootContext()->setContextProperty("mainWindow",				this);
	_qml->rootContext()->setContextProperty("dataSetModel",				_tableModel);
	_qml->rootContext()->setContextProperty("levelsTableModel",			_levelsTableModel);
	_qml->rootContext()->setContextProperty("columnsModel",				_columnsModel);
	_qml->rootContext()->setContextProperty("computedColumnsInterface",	_computedColumnsModel);
	_qml->rootContext()->setContextProperty("engineSync",				_engineSync);
	_qml->rootContext()->setContextProperty("filterModel",				_filterModel);
	_qml->rootContext()->setContextProperty("ribbonModel",				_ribbonModel);
	_qml->rootContext()->setContextProperty("fileMenuModel",			_fileMenu);
	_qml->rootContext()->setContextProperty("analysesModel",			_analyses);
	_qml->rootContext()->setContextProperty("resultsJsInterface",		_resultsJsInterface);

	_qml->rootContext()->setContextProperty("baseBlockDim",				20); //should be taken from Theme
	_qml->rootContext()->setContextProperty("baseFontSize",				16);
	_qml->rootContext()->setContextProperty("ppiScale",					1);//Settings::value(Settings::UI_SCALE).toFloat());

	std::cerr << "until preferences return ppiScale is set to 1" << std::endl;

	_qml->rootContext()->setContextProperty("columnTypeScale",			int(Column::ColumnType::ColumnTypeScale));
	_qml->rootContext()->setContextProperty("columnTypeOrdinal",		int(Column::ColumnType::ColumnTypeOrdinal));
	_qml->rootContext()->setContextProperty("columnTypeNominal",		int(Column::ColumnType::ColumnTypeNominal));
	_qml->rootContext()->setContextProperty("columnTypeNominalText",	int(Column::ColumnType::ColumnTypeNominalText));
	
	bool debug = false;
#ifdef JASP_DEBUG
	debug = true;
#endif
	_qml->rootContext()->setContextProperty("DEBUG_MODE", debug);
	_qml->rootContext()->setContextProperty("iconPath", iconPath);
	_qml->rootContext()->setContextProperty("iconFiles", iconFiles);
	_qml->rootContext()->setContextProperty("iconInactiveFiles", iconInactiveFiles);

	_qml->addImportPath("qrc:///components");

	_qml->load(QUrl("qrc:///components/JASP/Widgets/MainWindow.qml"));

	std::cout << "Dataview and levelstableview should have their signals connected INSIDE QML" << std::endl;
	QObject * DataView				= _qml->findChild<QObject*>("dataSetTableView");
	QObject * levelsTableView		= _qml->findChild<QObject*>("levelsTableView");

	connect(DataView,				SIGNAL(dataTableDoubleClicked()),			this,					SLOT(startDataEditorHandler()));
	connect(levelsTableView,		SIGNAL(columnChanged(QString)),				_analyses,				SLOT(refreshAnalysesUsingColumn(QString)));
}

/*
void MainWindow::handleRibbonButtonClicked(QVariant analysisMenuModel)
{
	// NOTE: Workaround for now. This will be replaced when mainwindow is made in QML

	AnalysisMenuModel* model			= qvariant_cast<AnalysisMenuModel*>(analysisMenuModel);
	Modules::AnalysisEntries entries	= model->getAnalysisEntries();

	if(entries.size() == 1)
	{
		QAction *action = new QAction();
		action->setText(QString::fromStdString(entries[0]->title()));
		action->setData(QString::fromStdString(entries[0]->buttonMenuString()));
		onMenuClicked(action);
	}
	else
	{

		QMenu *menu = new QMenu(this); //Who cleans this up?
		connect(menu, SIGNAL(triggered(QAction *)), this, SLOT(onMenuClicked(QAction *)));

		for (auto * i : entries) {

			if (i->title() == "???") {
				menu->addSeparator();
				continue;
			}

			QAction *action = new QAction();
			action->setText(QString::fromStdString(i->title()));
			action->setData(QString::fromStdString(i->buttonMenuString()));
			menu->addAction(action);
		}

		menu->exec(QCursor::pos());
	}
}

void MainWindow::analysisFormChangedHandler(Analysis *analysis)
{
	AnalysisForm* form = _analysisFormsMap[analysis];
	if (form != nullptr)
	{
		if (_currentOptionsWidget == form)
			closeCurrentOptionsWidget();
		delete _analysisFormsMap[analysis];
		_analysisFormsMap.erase(analysis);
	}
	showForm(analysis);
}

void MainWindow::onMenuClicked(QAction *action)
{
	std::string possiblyCodedReference = action->data().toString().toStdString();

	try
	{
		addAnalysisFromDynamicModule(_dynamicModules->retrieveCorrespondingAnalysisEntry(possiblyCodedReference));
	}
	catch(Modules::ModuleException &)
	{
		// It probably is a "normal entry"
		ribbonEntrySelected(action->data().toString());
	}
}
*/

void MainWindow::open(QString filepath)
{
	if(resultXmlCompare::compareResults::theOne()->testMode())
		resultXmlCompare::compareResults::theOne()->setFilePath(filepath);

	_openedUsingArgs = true;
	if (_resultsViewLoaded)	_fileMenu->open(filepath);
	else					_openOnLoadFilename = filepath;
}

/*
void MainWindow::resizeEvent(QResizeEvent *event)
{
	QMainWindow::resizeEvent(event);
	adjustOptionsPanelWidth();
}

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

bool MainWindow::filterShortCut()
{
	bool exclude = _excludeKey;
#ifdef __APPLE__
	if (exclude)
		qDebug() << "KEY EXCLUDED!";
	// This is a workaround for Qt Bug https://bugreports.qt.io/browse/QTBUG-67016
	// When we move to a Qt version (probably 5.11) where this bug is solved, we have to remove this workaround!
	_excludeKey = true;
	QTimer *timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(updateExcludeKey()));
	timer->start(100);
#endif

	return exclude;
}

void MainWindow::saveKeysSelected()
{
	if (filterShortCut())
		return;

	if (_package->isModified()) _fileMenu->save();
}


void MainWindow::openKeysSelected()
{
	if (filterShortCut())
		return;
}


void MainWindow::refreshKeysSelected()
{
	if (filterShortCut())
		return;

	refreshAllAnalyses();
}

void MainWindow::zoomInKeysSelected()
{
	if (filterShortCut())
		return;

	_resultsJsInterface->zoomIn();
}

void MainWindow::zoomOutKeysSelected()
{
	if (filterShortCut())
		return;

	_resultsJsInterface->zoomOut();
}

void MainWindow::zoomEqualKeysSelected()
{
	if (filterShortCut())
		return;

	_resultsJsInterface->zoomReset();
}


void MainWindow::syncKeysSelected()
{
	if (filterShortCut())
		return;

	_fileMenu->sync();
}

/*
void MainWindow::illegalOptionStateChanged(AnalysisForm * form)
{
	if (form->hasIllegalValue())
	{
		ui->optionsErrorLabel->setText(form->illegalValueMessage());
		ui->optionsErrorPanel->show();
	}
	else
	{
		ui->optionsErrorPanel->hide();
	}
}*/


void MainWindow::packageChanged(DataSetPackage *package)
{
	QString title = windowTitle();

	if (package->isModified())	title += '*';
	else						title.chop(1);


	setWindowTitle(title);
}


void MainWindow::refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns,	 std::vector<std::string> &missingColumns,	 std::map<std::string, std::string> &changeNameColumns, bool rowCountChanged)
{
	std::vector<std::string> oldColumnNames;

	for (auto & keyval : changeNameColumns)
		oldColumnNames.push_back(keyval.first);

	sort(changedColumns.begin(), changedColumns.end());
	sort(missingColumns.begin(), missingColumns.end());
	sort(oldColumnNames.begin(), oldColumnNames.end());

	_analyses->refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns, oldColumnNames);

	_computedColumnsModel->packageSynchronized(changedColumns, missingColumns, changeNameColumns, rowCountChanged);
}

void MainWindow::dataSetChanged(DataSet * dataSet)
{
	_package->setDataSet(dataSet);
	setDataSetAndPackageInModels(_package);
}

void MainWindow::setPPIHandler(int ppi, bool refreshAllAnalyses)
{
	emit ppiChanged(ppi);

	if(refreshAllAnalyses)
		MainWindow::refreshAllAnalyses();
}

void MainWindow::setImageBackgroundHandler(QString value)
{
	emit imageBackgroundChanged(value);
	refreshAllAnalyses();
}

void MainWindow::setUIScaleHandler(float scale)
{
	_qml->rootContext()->setContextProperty("ppiScale",	scale);
}

void MainWindow::setDataSetAndPackageInModels(DataSetPackage *package)
{
	DataSet * dataSet = package == nullptr ? nullptr : package->dataSet();
	_tableModel->setDataSetPackage(package);
	_levelsTableModel->setDataSet(dataSet);
	_columnsModel->setDataSet(dataSet);
	_computedColumnsModel->setDataSetPackage(package);
	_analyses->setDataSet(dataSet);
}

void MainWindow::packageDataChanged(DataSetPackage *package,
									vector<string> &changedColumns,
									vector<string> &missingColumns,
									map<string, string> &changeNameColumns,
									bool rowCountChanged)
{
	setDataSetAndPackageInModels(package);

	_labelFilterGenerator->regenerateFilter();
	_filterModel->checkForSendFilter();
	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns, rowCountChanged);
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

	if (_package->isLoaded())
		_package->setModified(true);

	if(resultXmlCompare::compareResults::theOne()->testMode())
		analysesForComparingDoneAlready();
}

void MainWindow::analysisSaveImageHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == nullptr)
		return;

	if (analysis->version() != AppInfo::version)
	{
		if(MessageForwarder::showYesNo("Version incompatibility", "This analysis was created in an older version of JASP, to save the image it must be refreshed first.\n\nRefresh the analysis?"))
			analysis->refresh();
	}
	else
		_analysisSaveImageHandler(analysis, options);
}

void MainWindow::_analysisSaveImageHandler(Analysis* analysis, QString options)
{
	string utf8 = fq(options);
	Json::Value root;
	Json::Reader parser;
	parser.parse(utf8, root);

	QString caption = "Save JASP Image";
	QString filter = "Portable Network Graphics (*.png);;Portable Document Format (*.pdf);;Encapsulated PostScript (*.eps);;300 dpi Tagged Image File (*.tiff)";
	QString selectedFilter;

	std::cout << "Replace savefiledialog in _analysisSaveImageHandler" << std::endl;
	QString finalPath = "THIS ISNT WORKING"; //QFileDialog::getSaveFileName(this, caption, QString(), filter, &selectedFilter);

	if (!finalPath.isEmpty())
	{
		if (selectedFilter == "Encapsulated PostScript (*.eps)")
		{
			root["type"] = "eps";
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(analysis, root);
		}
		else if (selectedFilter == "Portable Document Format (*.pdf)")
		{
			root["type"] = "pdf";
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(analysis, root);
		}
		else if (selectedFilter == "300 dpi Tagged Image File (*.tiff)")
		{
			root["type"] = "tiff";
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(analysis, root);
		}
		else
		{
			QString imagePath = QString::fromStdString(TempFiles::sessionDirName()) + "/" + root.get("name", Json::nullValue).asCString();
			if (QFile::exists(finalPath))
			{
				QFile::remove(finalPath);
			}
			QFile::copy(imagePath, finalPath);
		}
	}
}


void MainWindow::analysisImageSavedHandler(Analysis *analysis)
{
	Json::Value results = analysis->getImgResults();
	if (results.isNull())
		return;
	Json::Value inputOptions = results.get("inputOptions", Json::nullValue);

	QString imagePath = QString::fromStdString(TempFiles::sessionDirName()) + "/" + results.get("name", Json::nullValue).asCString();
	QString finalPath = QString::fromStdString(inputOptions.get("finalPath", Json::nullValue).asCString());

	if (!finalPath.isEmpty())
	{
		std::cout << "analysisImageSavedHandler, imagePath: " << imagePath.toStdString() << ", finalPath: " << finalPath.toStdString() << std::endl;
		std::cout.flush();
		if (QFile::exists(finalPath))
		{
			QFile::remove(finalPath);
		}
		QFile::copy(imagePath, finalPath);
	}
}

void MainWindow::analysisEditImageHandler(int id, QString options)
{

    Analysis *analysis = _analyses->get(id);
    if (analysis == nullptr)
        return;

    string utf8 = fq(options);
    Json::Value root;
    Json::Reader parser;
    parser.parse(utf8, root);

    analysis->editImage(analysis, root);

    return;

}

/*AnalysisForm* MainWindow::loadForm(Analysis *analysis)
{
	if (_analysisFormsMap.count(analysis) == 0)
	{
		AnalysisForm * formCreated	= createAnalysisForm(analysis);
		_analysisFormsMap[analysis] = formCreated;
		Options *options			= analysis->options();
		DataSet *dataSet			= _package->dataSet();

		formCreated->bindTo(options, dataSet);
	}

	std::cerr << "_analysisFormsMap[analysis]->show(); " << std::endl;

	return _analysisFormsMap[analysis];
}*/

void MainWindow::updateShownVariablesModel()
{
	//if(_currentOptionsWidget != nullptr)
		//_currentOptionsWidget->connectToAvailableVariablesModel(_package->dataSet());

#ifdef JASP_DEBUG
	std::cout << "void MainWindow::updateShownVariablesModel() does not do anything anymore because connectToAvailableVariablesModel is gone, this however is no problem if any new added columns show up in you analysisform. If they do It is probably also nice to remove this warning and function etc!" << std::endl;
#endif
}

AnalysisForm* MainWindow::createAnalysisForm(Analysis *analysis)
{
	const string name = analysis->name();

	qDebug() << "Form " << QString::fromStdString(name) << " loaded";
	AnalysisForm *form = nullptr;


	std::cout << "How to handle loading of a form? We should probably turn Analyses into some king of model and maybe Analysis as well? We could merge those two." << std::endl;

	form = new AnalysisForm(nullptr, analysis);

	connect(form,			&AnalysisForm::sendRScript, _engineSync,	&EngineSync::sendRCode);
	connect(_engineSync,	&EngineSync::rCodeReturned, form,			&AnalysisForm::runScriptRequestDone);
	connect(form,			&AnalysisForm::formChanged, this,			&MainWindow::analysisFormChangedHandler);

	return form;
}


void MainWindow::showForm(Analysis *analysis)
{


	_analyses->selectAnalysis(analysis);
	setAnalysesVisible(true);

	std::cout << "MainWindow::showForm(Analysis *analysis) is being called but it aint doin' much..." << std::endl;
	//closeCurrentOptionsWidget();
	//_currentOptionsWidget =

	//loadForm(analysis);

	//if (_currentOptionsWidget != NULL)
	{
	/*	QWidget *theWidget = _currentOptionsWidget->getWidget();

		int requiredSize		= theWidget->sizeHint().width();
		int currentOptionSpace	= ui->panel_2_Options->minimumWidth() - _scrollbarWidth;

		if (requiredSize > currentOptionSpace)
		{
			ui->panel_2_Options->setMinimumWidth(requiredSize + _scrollbarWidth);
			_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
		}

		theWidget->setMinimumWidth(ui->panel_2_Options->minimumWidth() - _scrollbarWidth);
		theWidget->show();

		ui->scrollArea->setVerticalScrollBarPolicy(analysis->fromQML() ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAlwaysOn);
		ui->optionsContentAreaLayout->addWidget(theWidget);//, 0, 0, Qt::AlignRight | Qt::AlignTop);

		if (ui->panel_2_Options->isVisible() == false)
			showOptionsPanel();

		_okButton->setVisible(_currentAnalysis->useData());
		_runButton->setVisible(_currentAnalysis->isAutorun() == false);
		_runButton->setEnabled(_currentAnalysis->status() == Analysis::InitedAndWaiting);
		_buttonPanel->raise();
		_buttonPanel->show();*/

		QString helpPage = QString("analyses/") + tq(analysis->name()).toLower();
		requestHelpPage(helpPage);
	}
}


void MainWindow::closeCurrentOptionsWidget()
{
	std::cout << "void MainWindow::closeCurrentOptionsWidget() doesnt do anything" << std::endl;

	//if (_currentOptionsWidget != NULL)
	{
		//no need to disconnect illegalChanged and illegalOptionStateChanged, options shouldn't change if the form isn't visible right?
		//_currentOptionsWidget->getWidget()->hide();
		//_currentOptionsWidget = NULL;
	}
}


void MainWindow::analysisSelectedHandler(int id)
{
	_currentAnalysis = _analyses->get(id);

	if (_currentAnalysis != nullptr)
	{
		showForm(_currentAnalysis);
		//ui->tabBar->setCurrentTab(QString::fromStdString(_currentAnalysis->module()));

		std::cout << "analysisSelectedHandler is not choosing a tabbar, should be done from analyses model or something" << std::endl;
	}
}


void MainWindow::analysisUnselectedHandler()
{
	//hideOptionsPanel();
	std::cout << "should hide options panel now" << std::endl;
}

void MainWindow::helpToggled(bool on)
{
/*	static int helpWidth = 0;

	if (on)
	{
		if (helpWidth < 400)
			helpWidth = 400;

		QList<int> sizes = ui->splitter->sizes();

		int resultsWidth = sizes.at(2) - ui->splitter->handleWidth() - 2 - helpWidth;

		sizes[2] = resultsWidth;
		sizes[3] = helpWidth;

		ui->panel_4_Help->show();
		ui->splitter->setSizes(sizes);
	}
	else
	{
		helpWidth = ui->panel_4_Help->width();
		ui->panel_4_Help->hide();
	}*/
}

void MainWindow::dataSetIORequestHandler(FileEvent *event)
{
	if (event->operation() == FileEvent::FileOpen)
	{
		if (_package->isLoaded())
		{
			//If this instance has a valid OSF connection save this setting for a new instance
			_odm->savePasswordFromAuthData(OnlineDataManager::OSF);

			// begin new instance
			QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(event->path()));
		}
		else
		{
			connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);

			_loader.io(event, _package);
			showProgress();

		}

		std::cout << "ui->tabBar->setCurrentModuleActive(); isnt happening" << std::endl;
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (_analyses->count() > 0)
		{
			_package->setWaitingForReady();

			getAnalysesUserData();
			_resultsJsInterface->exportPreviewHTML();

			Json::Value analysesData(Json::objectValue);

			analysesData["analyses"]	= _analyses->asJson();
			analysesData["meta"]		= _resultsJsInterface->getResultsMeta();

			_package->setAnalysesData(analysesData);
		}

		connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);

		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);

		_resultsJsInterface->exportHTML();

		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportData || event->operation() == FileEvent::FileGenerateData)
	{
		connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);
		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (_package->dataSet() == nullptr)
			return;

		connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);
		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileClose)
	{

		if (_package->isModified())
		{
			QString title = windowTitle();
			title.chop(1);

			switch(MessageForwarder::showSaveDiscardCancel("Save Workspace?", "Save changes to workspace " + title + " before closing?\n\nYour changes will be lost if you don't save them."))
			{
			case MessageForwarder::DialogResponse::Save:
			{
				FileEvent *saveEvent = _fileMenu->save();
				event->chain(saveEvent);
				connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);

				setDataPanelVisible(false);
				break;
			}

			case MessageForwarder::DialogResponse::Cancel:
				event->setComplete(false);
				dataSetIOCompleted(event);
				break;

			case MessageForwarder::DialogResponse::Discard:
				event->setComplete(true);
				dataSetIOCompleted(event);
				setDataPanelVisible(false);
				break;
			}
		}
		else
		{
			event->setComplete();
			dataSetIOCompleted(event);
			setDataPanelVisible(false);
		}

		closeVariablesPage();
	}
}

void MainWindow::resizeVariablesWindowLabelColumn()
{
	std::cout << "resizeVariablesWindowLabelColumn should be triggered by some model in QML" << std::endl;
	QObject * levelsTableView = _qml->findChild<QObject*>("levelsTableView");
	QMetaObject::invokeMethod(levelsTableView, "resizeLabelColumn");
}

void MainWindow::closeVariablesPage()
{
	std::cout << "closeVariablesPage should be triggered by some model in QML" << std::endl;
	QObject * levelsTableView = _qml->findChild<QObject*>("levelsTableView");
	QMetaObject::invokeMethod(levelsTableView, "closeYourself");
}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
	std::cout << "this->analysisOKed();" << std::endl;

	bool showAnalysis = false;
	hideProgress();

	if (event->operation() == FileEvent::FileOpen)
	{
		if (event->successful())
		{
			populateUIfromDataSet();
			QString name = QFileInfo(event->path()).baseName();
			setWindowTitle(name);
			_currentFilePath = event->path();

			if (event->type() == Utils::FileType::jasp && !_package->dataFilePath().empty() && !_package->dataFileReadOnly() && strncmp("http", _package->dataFilePath().c_str(), 4) != 0)
			{
				QString dataFilePath = QString::fromStdString(_package->dataFilePath());
				if (QFileInfo::exists(dataFilePath))
				{
					uint currentDataFileTimestamp = QFileInfo(dataFilePath).lastModified().toTime_t();
					if (currentDataFileTimestamp > _package->dataFileTimestamp())
						emit event->dataFileChanged(event->dataFilePath());
				}
				else
				{
					_package->setDataFilePath("");
					_package->setModified(true);
				}
			}

			if (resultXmlCompare::compareResults::theOne()->testMode())
				startComparingResults();

			//ui->quickWidget_Ribbon->rootContext()->setContextProperty("ribbonIsEnabled", true); //Should really call a function or something or set something in a model!


		}
		else
		{
			if (_package->dataSet() != nullptr)
				_loader.free(_package->dataSet());
			_package->reset();
			setDataSetAndPackageInModels(nullptr);

			if (_openedUsingArgs)	_application->exit(1);
			else					MessageForwarder::showWarning("Unable to open file.\n\n" + event->message());

		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		bool testingAndSaving = resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->shouldSave();

		if (event->successful())
		{
			QString name = QFileInfo(event->path()).baseName();

			_package->setModified(false);
			setWindowTitle(name);
			showAnalysis = true;

			if(testingAndSaving)
				std::cerr << "Tested and saved " << event->path().toStdString() << " succesfully!" << std::endl;

		}
		else
		{
			MessageForwarder::showWarning("Save failed", "Unable to save file.\n\n" + event->message());

			if(testingAndSaving)
				std::cerr << "Tested " << event->path().toStdString() << " but saving failed because of: " << event->message().toStdString() << std::endl;
		}

		if(testingAndSaving)
			finishSavingComparedResults();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		_package->setModified(true);
		showAnalysis = true;
	}
	else if (event->operation() == FileEvent::FileGenerateData || event->operation() == FileEvent::FileExportResults)
	{
		showAnalysis = true;
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (event->successful())
		{
			closeCurrentOptionsWidget();
			_analyses->clear();
			std::cout << "should hide options panel now" << std::endl;
			setDataSetAndPackageInModels(nullptr);
			_loader.free(_package->dataSet());
			_package->reset();
			_filterModel->setDataSetPackage(nullptr);
			updateMenuEnabledDisabledStatus();
			//ui->webViewResults->reload();
			std::cout << "should trigger reload of webengine" << std::endl;

			setWindowTitle("JASP");

			if (_applicationExiting)
				QApplication::exit();
			else
				std::cout << " IM not hiding: ui->panel_1_Data->hide();" << std::endl;

		}
		else
		{
			_applicationExiting = false;
		}
	}

	if (showAnalysis)
	{
		std::cout << " im not doing: ui->tabBar->setCurrentModuleActive(); " <<std::endl;
	}
}


void MainWindow::populateUIfromDataSet()
{
	setDataSetAndPackageInModels(_package);

	if(_package->dataSet()->rowCount() == 0)
		setDataPanelVisible(false);
	else
	{
		_filterModel->setDataSetPackage(_package);
		_filterModel->init();
		setDataPanelVisible(true);
	}

	hideProgress();

	bool errorFound = false;
	stringstream errorMsg;

	if (_package->hasAnalyses())
	{
		int corruptAnalyses = 0;

		stringstream corruptionStrings;

		Json::Value analysesData = _package->analysesData();
		if (analysesData.isNull())
		{
			errorFound = true;
			errorMsg << "An error has been detected and analyses could not be loaded.";
		}
		else
		{
			Json::Value analysesDataList = analysesData;
			if (!analysesData.isArray()) {
				analysesDataList = analysesData.get("analyses", Json::arrayValue);
				Json::Value meta = analysesData.get("meta", Json::nullValue);
				if ( ! meta.isNull())
				{
					QString results = tq(analysesData["meta"].toStyledString());
					_resultsJsInterface->setResultsMeta(results);
				}
			}

			for (Json::Value &analysisData : analysesDataList)
			{
				try
				{
					_analyses->createFromJaspFileEntry(analysisData, _dynamicModules);
				}
				catch (Modules::ModuleException modProb)
				{
					//Maybe show a nicer messagebox?
					errorFound = true;
					corruptionStrings << "\n" << (++corruptAnalyses) << ": " << modProb.what();
				}
				catch (runtime_error e)
				{
					errorFound = true;
					corruptionStrings << "\n" << (++corruptAnalyses) << ": " << e.what();
				}
				catch (exception e)
				{
					errorFound = true;
					corruptionStrings << "\n" << (++corruptAnalyses) << ": " << e.what();
				}
			}
		}

		if (corruptAnalyses == 1)
			errorMsg << "An error was detected in an analysis. This analysis has been removed for the following reason:\n" << corruptionStrings.str();
		else if (corruptAnalyses > 1)
			errorMsg << "Errors were detected in " << corruptAnalyses << " analyses. These analyses have been removed for the following reasons:\n" << corruptionStrings.str();
	}

	if (_package->warningMessage() != "")	MessageForwarder::showWarning(_package->warningMessage());
	else if (errorFound)					MessageForwarder::showWarning(errorMsg.str());

	matchComputedColumnsToAnalyses();

	_package->setLoaded();
	updateMenuEnabledDisabledStatus();
	//checkUsedModules(); //Ought to been done all through RibbonModel + Analyses
}

void MainWindow::matchComputedColumnsToAnalyses()
{
	for(ComputedColumn * col : *_package->computedColumnsPointer())
		if(col->analysisId() != -1)
			col->setAnalysis(_analyses->get(col->analysisId()));
}


void MainWindow::updateMenuEnabledDisabledStatus()
{
	bool loaded = _package->isLoaded();
#ifdef JASP_DEBUG
	std::cout << "updateMenuEnabledDisabledStatus is pointless" << std::endl;
#endif

	// ui->ribbonAnalysis->setDataSetLoaded(loaded);
	// ui->ribbonSEM->setDataSetLoaded(loaded);
	// ui->ribbonReinforcementLearning->setDataSetLoaded(loaded);
	// ui->ribbonMetaAnalysis->setDataSetLoaded(loaded);
	// ui->ribbonNetworkAnalysis->setDataSetLoaded(loaded);
///// 5-ribbon updateMenuEnabledDisabledStatus
}

void MainWindow::resultsPageLoaded(bool success, int ppi)
{
	if (success)
	{
// #ifdef __WIN32__
// 		const int verticalDpi = QApplication::desktop()->screen()->logicalDpiY();
// 		qreal zoom = ((qreal)(verticalDpi) / (qreal)ppi);
// 		ui->webViewResults->setZoomFactor(zoom);
// 		ui->webViewHelp->setZoomFactor(zoom);
// 		ppi = verticalDpi;
// 		_resultsJsInterface->setZoom(zoom);
//
// 		this->resize(this->width() + (ui->webViewResults->width() * (zoom - 1)), this->height() + (ui->webViewResults->height() * (zoom - 1)));
// #endif

		if (_openOnLoadFilename != "")
		{
			_fileMenu->open(_openOnLoadFilename);
			_openOnLoadFilename = "";
		}

		_resultsViewLoaded = true;
	}

	if (_engineSync->engineStarted() == false)
		_engineSync->start();

	//PreferencesDialog *rd = ui->tabBar->getPreferencesDialog();
	//rd->setDefaultPPI(ppi);

	bool useDefaultPPI = Settings::value(Settings::PPI_USE_DEFAULT).toBool();
	if (!useDefaultPPI)
	{
		int customPPI = Settings::value(Settings::PPI_CUSTOM_VALUE).toInt();
		ppi = customPPI;
	}

	setPPIHandler(ppi, false);
}


void MainWindow::fatalError()
{
	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;
		MessageForwarder::showWarning("Error", "JASP has experienced an unexpected internal error.\n\n" + _fatalError.toStdString() + "\n\nIf you could report your experiences to the JASP team that would be appreciated.\n\nJASP cannot continue and will now close.\n\n");
		QApplication::exit(1);
	}
}


void MainWindow::helpFirstLoaded(bool ok)
{
	if (ok)
		requestHelpPage("index");
}

void MainWindow::showHelpFromQML(QString pageName)
{
	std::cout << "showHelpFromQML is ignoring visibility of help panel" << std::endl;
	if(_lastRequestedHelpPage == pageName)//&& ui->panel_4_Help->isVisible())
	{
		helpToggled(false);
	}
	else
	{
	//	if(!ui->panel_4_Help->isVisible())
			helpToggled(true);

		requestHelpPage(pageName);
	}
}

void MainWindow::requestHelpPage(const QString &pageName)
{
	QFile fileMD(AppDirs::help() + "/" + pageName + ".md"), fileHTML(AppDirs::help() + "/" + pageName + ".html");

	QString content, renderFunc = "window.render";


	if (fileHTML.exists())
	{
		fileHTML.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileHTML.readAll());
		fileHTML.close();

		renderFunc = "window.renderHtml";

	}
	else if (fileMD.exists())
	{
		fileMD.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileMD.readAll());
		fileMD.close();
	}
	else
		content = "Coming Soon!\n========\n\nThere is currently no help available for this analysis.\n\nAdditional documentation will be available in future releases of JASP.";

	content.replace("\"", "\\\"");
	content.replace("\r\n", "\\n");
	content.replace("\r", "\\n");
	content.replace("\n", "\\n");

	QString JS = renderFunc + "(\"" + content + "\")";

	std::cout << "Am not running JS: " << JS.toStdString() << std::endl;
	//ui->webViewHelp->page()->runJavaScript(JS);

	_lastRequestedHelpPage = pageName;
}


void MainWindow::emptyValuesChangedHandler()
{
	if (_package->isLoaded())
	{
		vector<string> colChanged;
		vector<string> missingColumns;
		map<string, string> changeNameColumns;

		try
		{
			colChanged = _package->dataSet()->resetEmptyValues(_package->emptyValuesMap());
		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {

				_package->setDataSet(SharedMemory::enlargeDataSet(_package->dataSet()));
				colChanged = _package->dataSet()->resetEmptyValues(_package->emptyValuesMap());
			}
			catch (exception &e)
			{
				throw runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (exception e)	{	cout << "MainWindow::emptyValuesChangedHandler n " << e.what() << std::endl; 	}
		catch (...)			{	cout << "MainWindow::emptyValuesChangedHandler something when wrong...\n" << std::endl; }

		_package->setModified(true);
		packageDataChanged(_package, colChanged, missingColumns, changeNameColumns, false);
	}
}


void MainWindow::addAnalysisFromDynamicModule(Modules::AnalysisEntry * entry)
{
	std::cout << "void MainWindow::addAnalysisFromDynamicModule(Modules::AnalysisEntry * entry) should be a function of RibbonModel that sends a signal to Analyses" << std::endl;
	try
	{
		_currentAnalysis = _analyses->create(entry);

		showForm(_currentAnalysis);
		_analyses->analysisAdded(_currentAnalysis);

		_currentAnalysis->setResults(entry->getDefaultResults());

		_resultsJsInterface->showAnalysis(_currentAnalysis->id());

		//checkUsedModules(); //Ought to been done all through RibbonModel + Analyses
	}
	catch (runtime_error& e)
	{
		_fatalError = tq(e.what());
		fatalError();
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

void MainWindow::analysisRunned()
{
	if (_currentAnalysis == nullptr)
		return;

	if (_currentAnalysis->status() == Analysis::Running)
		_currentAnalysis->abort();
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

	analysis->options()->set(root);
}

void MainWindow::startDataEditorHandler()
{

	QString path = QString::fromStdString(_package->dataFilePath());
	if (path.isEmpty() || path.startsWith("http") || !QFileInfo::exists(path) || Utils::getFileSize(path.toStdString()) == 0 || _package->dataFileReadOnly())
	{
		QString									message = "JASP was started without associated data file (csv, sav or ods file). But to edit the data, JASP starts a spreadsheet editor based on this file and synchronize the data when the file is saved. Does this data file exist already, or do you want to generate it?";
		if (path.startsWith("http"))			message = "JASP was started with an online data file (csv, sav or ods file). But to edit the data, JASP needs this file on your computer. Does this data file also exist on your computer, or do you want to generate it?";
		else if (_package->dataFileReadOnly())	message = "JASP was started with a read-only data file (probably from the examples). But to edit the data, JASP needs to write to the data file. Does the same file also exist on your computer, or do you want to generate it?";

		MessageForwarder::DialogResponse choice = MessageForwarder::showYesNoCancel("Start Spreadsheet Editor", message, "Generate Data File", "Find Data File");

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

			std::cout << "Currently startDataEditorHandler treats title as: " << name.toStdString() << std::endl;

			if (name.endsWith("*"))
			{
				name.truncate(name.length() - 1);
				name = name.replace('#', '_');
			}
			if (!_currentFilePath.isEmpty())
			{
				QFileInfo file(_currentFilePath);
				name = file.absolutePath() + QDir::separator() + file.baseName().replace('#', '_') + ".csv";
			}

			path = MessageForwarder::saveFileBrowse(caption, name, filter);

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
			QString filter = "Data File (*.csv *.txt *.sav *.ods)";

			path = MessageForwarder::openFileBrowse(caption, "", filter);
			if (path == "")
				return;

			event = new FileEvent(this, FileEvent::FileSyncData);
			break;
		}

		}
		connect(event, &FileEvent::completed, this, &MainWindow::startDataEditorEventCompleted);
		//connect(event, SIGNAL(completed(FileEvent*)), _backStage, SLOT(setSyncFile(FileEvent*)));
		event->setPath(path);
		_loader.io(event, _package);
		showProgress();
	}
	else
		startDataEditor(path);
}


void MainWindow::startDataEditorEventCompleted(FileEvent* event)
{
	hideProgress();

	if (event->successful())
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

	int useDefaultSpreadsheetEditor = Settings::value(Settings::USE_DEFAULT_SPREADSHEET_EDITOR).toInt();
	QString appname = Settings::value(Settings::SPREADSHEET_EDITOR_NAME).toString();

	if (QString::compare(fileInfo.suffix(), "sav", Qt::CaseInsensitive) == 0)
	{
		if (useDefaultSpreadsheetEditor == 0 && !appname.contains("SPSS", Qt::CaseInsensitive))
			useDefaultSpreadsheetEditor = 1;
	}

	if (appname.isEmpty())
		useDefaultSpreadsheetEditor = 1;

	QString startProcess;
	if (useDefaultSpreadsheetEditor == 0)
	{
#ifdef __APPLE__
		appname = appname.mid(appname.lastIndexOf('/') + 1);
		startProcess = "open -a \"" + appname + "\" \"" + path + "\"";
#else
		startProcess = "\"" + appname + "\" \"" + path + "\"";
#endif
		if (!QProcess::startDetached(startProcess))
			MessageForwarder::showWarning("Start Editor", "Unable to start the editor : " + appname + ". Please check your editor settings in the preference menu.");
	}
	else
		if (!QDesktopServices::openUrl(QUrl("file:///" + path, QUrl::TolerantMode)))
			MessageForwarder::showWarning("Start Spreadsheet Editor", "No default spreadsheet editor for file " + fileInfo.completeBaseName() + ". Use Preferences to set the right editor.");

}

void MainWindow::showProgress()
{
	_fileMenu->setVisible(false);
	setDataPanelVisible(true);
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


void MainWindow::updateExcludeKey()
{
	_excludeKey = false;
}


void MainWindow::testLoadedJaspFile(int timeOut, bool save)
{
	std::cout << "Enabling testmode for JASP with a timeout of " << timeOut << " minutes!" << std::endl;
	resultXmlCompare::compareResults::theOne()->enableTestMode();

	if(save)
		resultXmlCompare::compareResults::theOne()->enableSaving();

	QTimer::singleShot(60000 * timeOut, this, &MainWindow::unitTestTimeOut);
}

void MainWindow::unitTestTimeOut()
{
	std::cerr << "Time out for unit test!" << std::endl;
	_application->exit(2);
}

void MainWindow::analysisFormChangedHandler(Analysis *analysis)
{
	//if (_currentOptionsWidget == _analysisFormsMap[analysis])
	//	closeCurrentOptionsWidget();

	//delete _analysisFormsMap[analysis];
	//_analysisFormsMap.erase(analysis);
	showForm(analysis);
}

void MainWindow::startComparingResults()
{
	if (resultXmlCompare::compareResults::theOne()->testMode())
	{
		refreshAllAnalyses();
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
			_application->exit(resultXmlCompare::compareResults::theOne()->compareSucces() ? 0 : 1);
	}
}

void MainWindow::saveJaspFileHandler()
{
	std::cerr << "saving file!" << std::endl;

	FileEvent * saveEvent = new FileEvent(this, FileEvent::FileSave);

	saveEvent->setPath(resultXmlCompare::compareResults::theOne()->filePath());

	dataSetIORequestHandler(saveEvent);

}


void MainWindow::finishSavingComparedResults()
{
	if(resultXmlCompare::compareResults::theOne()->testMode() && resultXmlCompare::compareResults::theOne()->shouldSave())
	{
		_application->exit(resultXmlCompare::compareResults::theOne()->compareSucces() ? 0 : 1);
	}
}

void MainWindow::pauseEngines()
{
	_engineSync->pause();
}

void MainWindow::resumeEngines()
{
	_engineSync->resume();
}

void MainWindow::showQMLWindow(QString urlQml)
{
	std::cerr << "showing some qml in a separate window, should be *ALL* QML!" << std::endl;
/*
	QQuickView * newQMLWindow = new QQuickView;

	newQMLWindow->engine()->addImportPath("qrc:///components");
	newQMLWindow->rootContext()->setContextProperty("dynamicModules", _dynamicModules);
	newQMLWindow->rootContext()->setContextProperty("ppiScale", 1);
	newQMLWindow->setSource(urlQml);
	newQMLWindow->setResizeMode(QQuickView::SizeRootObjectToView);
	newQMLWindow->show();

	connect(newQMLWindow->rootObject(), SIGNAL(closeWindow()), newQMLWindow, SLOT(close()));*/

}

void MainWindow::setRunButtonText(QString runButtonText)
{
	if (_runButtonText == runButtonText)
		return;

	_runButtonText = runButtonText;
	emit runButtonTextChanged(_runButtonText);
}

void MainWindow::setRunButtonEnabled(bool runButtonEnabled)
{
	if (_runButtonEnabled == runButtonEnabled)
		return;

	_runButtonEnabled = runButtonEnabled;
	emit runButtonEnabledChanged(_runButtonEnabled);
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


void MainWindow::setWindowTitle(QString windowTitle)
{
	if (_windowTitle == windowTitle)
		return;

	_windowTitle = windowTitle;
	emit windowTitleChanged(_windowTitle);
}

void MainWindow::removeAnalysis(Analysis *analysis)
{
	_analyses->removeAnalysis(analysis);

	if (_package->isLoaded())
		_package->setModified(true);

	_resultsJsInterface->removeAnalysis(analysis);

	//checkUsedModules(); //Ought to been done all through RibbonModel + Analyses
}

void MainWindow::removeAllAnalyses()
{
	if (MessageForwarder::showYesNo("Remove All Analyses", "Do you really want to remove all analyses?"))
		_analyses->clear();
}

void MainWindow::getAnalysesUserData()
{
	QVariant userData = _resultsJsInterface->getAllUserData();

	Json::Value data;
	Json::Reader parser;
	parser.parse(fq(userData.toString()), data);

	_analyses->setAnalysesUserData(data);
}

void  MainWindow::setAnalysesVisible(bool analysesVisible)
{
	if (_analysesVisible == analysesVisible)
		return;

	_analysesVisible = analysesVisible;
	emit analysesVisibleChanged(_analysesVisible);
}
