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
#include <QtWebEngine>

#include "utilities/qutils.h"
#include "utilities/appdirs.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "appinfo.h"

#include "gui/aboutdialog.h"
#include "gui/preferencesmodel.h"
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

QString MainWindow::_iconPath = "qrc:/icons/";

QMap<QString, QVariant> MainWindow::_iconFiles {
	{ "nominalText"	, _iconPath + "variable-nominal-text.svg" },
	{ "nominal"		, _iconPath + "variable-nominal.svg"},
	{ "ordinal"		, _iconPath + "variable-ordinal.svg"},
	{ "scale"		, _iconPath + "variable-scale.svg"}
};

QMap<QString, QVariant> MainWindow::_iconInactiveFiles {
	{ "nominalText"	, _iconPath + "variable-nominal-inactive.svg" },
	{ "nominal"		, _iconPath + "variable-nominal-inactive.svg"},
	{ "ordinal"		, _iconPath + "variable-ordinal-inactive.svg"},
	{ "scale"		, _iconPath + "variable-scale-inactive.svg"}
};

QMap<QString, QVariant> MainWindow::_iconDisabledFiles {
	{ "nominalText"	, _iconPath + "variable-nominal-disabled.svg" },
	{ "nominal"		, _iconPath + "variable-nominal-disabled.svg"},
	{ "ordinal"		, _iconPath + "variable-ordinal-disabled.svg"},
	{ "scale"		, _iconPath + "variable-scale-disabled.svg"}
};

QMap<int, QString> MainWindow::_columnTypeMap {
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
	_dynamicModules			= new DynamicModules(this);
	_analyses				= new Analyses(this, _dynamicModules);
	_engineSync				= new EngineSync(_analyses, _package, _dynamicModules, this);
	_computedColumnsModel	= new ComputedColumnsModel(_analyses, this);
	_filterModel			= new FilterModel(_package, this);
	_ribbonModel			= new RibbonModel(_dynamicModules, { "Common", "Network", "Meta Analysis", "SEM", "Summary Statistics" });
	_ribbonModelFiltered	= new RibbonModelFiltered(this, _ribbonModel);
	_fileMenu				= new FileMenu(this);
	_helpModel				= new HelpModel(this);
	_preferences			= new PreferencesModel(this);

	new MessageForwarder(this); //We do not need to store this

	StartOnlineDataManager();

	makeConnections();

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




	connect(this,					&MainWindow::saveJaspFile,							this,					&MainWindow::saveJaspFileHandler,							Qt::QueuedConnection);

	connect(_levelsTableModel,		&LevelsTableModel::labelFilterChanged,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged					);
	connect(_levelsTableModel,		&LevelsTableModel::notifyColumnHasFilterChanged,	_tableModel,			&DataSetTableModel::notifyColumnFilterStatusChanged			);
	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModels,			_tableModel,			&DataSetTableModel::refreshColumn							);
	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModelsByName,	_computedColumnsModel,	&ComputedColumnsModel::checkForDependentColumnsToBeSentSlot	);

	connect(_tableModel,			&DataSetTableModel::dataSetChanged,					this,					&MainWindow::dataSetChanged									);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged					);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_levelsTableModel,		&LevelsTableModel::refresh,									Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::modelReset,						_levelsTableModel,		&LevelsTableModel::refresh,									Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::headerDataChanged,				_columnsModel,			&ColumnsModel::datasetHeaderDataChanged						);
	connect(_tableModel,			&DataSetTableModel::modelReset,						_columnsModel,			&ColumnsModel::refresh,										Qt::QueuedConnection);
	connect(_tableModel,			&DataSetTableModel::columnDataTypeChanged,			_computedColumnsModel,	&ComputedColumnsModel::recomputeColumn						);

	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_computedColumnsModel,	&ComputedColumnsModel::computeColumnSucceeded				);
	connect(_engineSync,			&EngineSync::computeColumnFailed,					_computedColumnsModel,	&ComputedColumnsModel::computeColumnFailed					);
	connect(_engineSync,			&EngineSync::processNewFilterResult,				_filterModel,			&FilterModel::processFilterResult							);
	connect(_engineSync,			&EngineSync::processFilterErrorMsg,					_filterModel,			&FilterModel::processFilterErrorMsg							);
	connect(_engineSync,			&EngineSync::refreshAllPlotsExcept,					_analyses,				&Analyses::refreshAllPlots									);

	qRegisterMetaType<Column::ColumnType>();

	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshColumn,				_tableModel,			&DataSetTableModel::refreshColumn,							Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshColumn,				_levelsTableModel,		&LevelsTableModel::refreshColumn,							Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::headerDataChanged,			_tableModel,			&DataSetTableModel::headerDataChanged,						Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::sendComputeCode,				_engineSync,			&EngineSync::computeColumn,									Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::dataSetChanged,				_tableModel,			&DataSetTableModel::dataSetChanged							);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshData,					_tableModel,			&DataSetTableModel::refresh,								Qt::QueuedConnection);
	connect(_computedColumnsModel,	&ComputedColumnsModel::refreshData,					this,					&MainWindow::updateShownVariablesModel						);
	connect(_computedColumnsModel,	&ComputedColumnsModel::showAnalysisForm,			_analyses,				&Analyses::selectAnalysis									);

	connect(this,					&MainWindow::imageBackgroundChanged,				_engineSync,			&EngineSync::imageBackgroundChanged							);

	connect(_resultsJsInterface,	&ResultsJsInterface::packageModified,				this,					&MainWindow::setPackageModified								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisChangedDownstream,		this,					&MainWindow::analysisChangedDownstreamHandler				);
	connect(_resultsJsInterface,	&ResultsJsInterface::saveTextToFile,				this,					&MainWindow::saveTextToFileHandler							);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSaveImage,				this,					&MainWindow::analysisSaveImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisEditImage,				this,					&MainWindow::analysisEditImageHandler						);
	connect(_resultsJsInterface,	&ResultsJsInterface::removeAnalysisRequest,			_analyses,				&Analyses::removeAnalysisById								);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisSelected,				_analyses,				&Analyses::analysisIdSelectedInResults						);
	connect(_resultsJsInterface,	&ResultsJsInterface::analysisUnselected,			_analyses,				&Analyses::analysesUnselectedInResults						);
	connect(_resultsJsInterface,	&ResultsJsInterface::resultsPageLoadedSignal,		this,					&MainWindow::resultsPageLoaded								);
	connect(_resultsJsInterface,	&ResultsJsInterface::openFileTab,					_fileMenu,				&FileMenu::showFileMenu										);

	connect(_analyses,				&Analyses::countChanged,							this,					&MainWindow::analysesCountChangedHandler					);
	connect(_analyses,				&Analyses::analysisResultsChanged,					this,					&MainWindow::analysisResultsChangedHandler					);
	connect(_analyses,				&Analyses::analysisImageSaved,						this,					&MainWindow::analysisImageSavedHandler						);
	connect(_analyses,				&Analyses::analysisAdded,							_fileMenu,				&FileMenu::analysisAdded									);
	connect(_analyses,				&Analyses::showAnalysisInResults,					_resultsJsInterface,	&ResultsJsInterface::showAnalysis							);
	connect(_analyses,				&Analyses::unselectAnalysisInResults,				_resultsJsInterface,	&ResultsJsInterface::unselect								);
	connect(_analyses,				&Analyses::analysisImageEdited,						_resultsJsInterface,	&ResultsJsInterface::analysisImageEditedHandler				);
	connect(_analyses,				&Analyses::analysisRemoved,							_resultsJsInterface,	&ResultsJsInterface::removeAnalysis							);
	//connect(_analyses,				&Analyses::analysisNameSelected,					_helpModel,				&HelpModel::setAnalysisPagename								); //The user can click the info-button if they want to see some documentation

	connect(_fileMenu,				&FileMenu::exportSelected,							_resultsJsInterface,	&ResultsJsInterface::exportSelected							);
	connect(_fileMenu,				&FileMenu::dataSetIORequest,						this,					&MainWindow::dataSetIORequestHandler						);

	connect(_odm,					&OnlineDataManager::progress,						this,					&MainWindow::setProgressStatus,								Qt::QueuedConnection);
	connect(&_loader,				&AsyncLoader::progress,								this,					&MainWindow::setProgressStatus								);
	connect(_engineSync,			&EngineSync::engineTerminated,						this,					&MainWindow::fatalError										);


	connect(this,					&MainWindow::screenPPIChanged,						_preferences,			&PreferencesModel::setDefaultPPI							);
	connect(_preferences,			&PreferencesModel::plotPPIChanged,					_engineSync,			&EngineSync::ppiChanged										);
	connect(_preferences,			&PreferencesModel::missingValuesChanged,			this,					&MainWindow::emptyValuesChangedHandler						);
	connect(_preferences,			&PreferencesModel::dataAutoSynchronizationChanged,	_fileMenu,				&FileMenu::dataAutoSynchronizationChanged					);
	connect(_preferences,			&PreferencesModel::exactPValuesChanged,				_resultsJsInterface,	&ResultsJsInterface::setExactPValuesHandler					);
	connect(_preferences,			&PreferencesModel::fixedDecimalsChangedString,		_resultsJsInterface,	&ResultsJsInterface::setFixDecimalsHandler					);
	connect(_preferences,			&PreferencesModel::plotBackgroundChanged,			this,					&MainWindow::setImageBackgroundHandler						);
	connect(_preferences,			&PreferencesModel::uiScaleChanged,					_resultsJsInterface,	&ResultsJsInterface::setZoom								);

	connect(_filterModel,			&FilterModel::refreshAllAnalyses,					_analyses,				&Analyses::refreshAllAnalyses								);
	connect(_filterModel,			&FilterModel::updateColumnsUsedInConstructedFilter, _tableModel,			&DataSetTableModel::setColumnsUsedInEasyFilter				);
	connect(_filterModel,			&FilterModel::filterUpdated,						_tableModel,			&DataSetTableModel::refresh									);
	connect(_filterModel,			&FilterModel::sendFilter,							_engineSync,			&EngineSync::sendFilter										);
	connect(_filterModel,			&FilterModel::updateGeneratedFilterWithR,			_labelFilterGenerator,	&labelFilterGenerator::easyFilterConstructorRCodeChanged	);

	connect(_labelFilterGenerator,	&labelFilterGenerator::setGeneratedFilter,			_filterModel,			&FilterModel::setGeneratedFilter							);
	connect(_engineSync,			&EngineSync::computeColumnSucceeded,				_filterModel,			&FilterModel::computeColumnSucceeded						);

	connect(_ribbonModel,			&RibbonModel::analysisClickedSignal,				_analyses,				&Analyses::analysisClickedHandler							);

	connect(_dynamicModules,		&DynamicModules::dynamicModuleUnloadBegin,			_analyses,				&Analyses::removeAnalysesOfDynamicModule					);
}


void MainWindow::loadQML()
{
	QtWebEngine::initialize();

	_qml = new QQmlApplicationEngine(this);

	_qml->rootContext()->setContextProperty("mainWindow",				this);
	_qml->rootContext()->setContextProperty("dataSetModel",				_tableModel);
	_qml->rootContext()->setContextProperty("levelsTableModel",			_levelsTableModel);
	_qml->rootContext()->setContextProperty("columnsModel",				_columnsModel);
	_qml->rootContext()->setContextProperty("computedColumnsInterface",	_computedColumnsModel);
	_qml->rootContext()->setContextProperty("engineSync",				_engineSync);
	_qml->rootContext()->setContextProperty("filterModel",				_filterModel);
	_qml->rootContext()->setContextProperty("ribbonModel",				_ribbonModel);
	_qml->rootContext()->setContextProperty("ribbonModelFiltered",		_ribbonModelFiltered);
	_qml->rootContext()->setContextProperty("dynamicModules",			_dynamicModules);

	_qml->rootContext()->setContextProperty("fileMenuModel",			_fileMenu);
	_qml->rootContext()->setContextProperty("analysesModel",			_analyses);
	_qml->rootContext()->setContextProperty("resultsJsInterface",		_resultsJsInterface);
	_qml->rootContext()->setContextProperty("helpModel",				_helpModel);
	_qml->rootContext()->setContextProperty("preferencesModel",			_preferences);

	_qml->rootContext()->setContextProperty("baseBlockDim",				20); //should be taken from Theme
	_qml->rootContext()->setContextProperty("baseFontSize",				16);

	_qml->rootContext()->setContextProperty("columnTypeScale",			int(Column::ColumnType::ColumnTypeScale));
	_qml->rootContext()->setContextProperty("columnTypeOrdinal",		int(Column::ColumnType::ColumnTypeOrdinal));
	_qml->rootContext()->setContextProperty("columnTypeNominal",		int(Column::ColumnType::ColumnTypeNominal));
	_qml->rootContext()->setContextProperty("columnTypeNominalText",	int(Column::ColumnType::ColumnTypeNominalText));
	
	bool debug = false;
#ifdef JASP_DEBUG
	debug = true;
#endif

	_qml->rootContext()->setContextProperty("DEBUG_MODE",			debug);
	_qml->rootContext()->setContextProperty("iconPath",				_iconPath);
	_qml->rootContext()->setContextProperty("iconFiles",			_iconFiles);
	_qml->rootContext()->setContextProperty("iconInactiveFiles",	_iconInactiveFiles);
	_qml->rootContext()->setContextProperty("iconDisabledFiles",	_iconDisabledFiles);

	_qml->addImportPath("qrc:///components");

	_qml->load(QUrl("qrc:///components/JASP/Widgets/HelpWindow.qml"));
	_qml->load(QUrl("qrc:///components/JASP/Widgets/MainWindow.qml"));
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

void MainWindow::saveKeysSelected()
{
	if (_package->isModified()) _fileMenu->save();
}


void MainWindow::openKeysSelected()
{
	std::cout << "openKeysSelected does nothing..."<< std::endl;
}


void MainWindow::refreshKeysSelected()
{
	_analyses->refreshAllAnalyses();
}

void MainWindow::zoomInKeysSelected()
{
	_preferences->zoomIn();
}

void MainWindow::zoomOutKeysSelected()
{
	_preferences->zoomOut();
}

void MainWindow::zoomEqualKeysSelected()
{
	_preferences->zoomReset();
}


void MainWindow::syncKeysSelected()
{
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



void MainWindow::setImageBackgroundHandler(QString value)
{
	emit imageBackgroundChanged(value);
	_engineSync->refreshAllPlots();
}


void MainWindow::setDataSetAndPackageInModels(DataSetPackage *package)
{
	DataSet * dataSet = package == nullptr ? nullptr : package->dataSet();

	_tableModel				-> setDataSetPackage(package);
	_levelsTableModel		-> setDataSet(dataSet);
	_columnsModel			-> setDataSet(dataSet);
	_computedColumnsModel	-> setDataSetPackage(package);
	_analyses				-> setDataSet(dataSet);
	_filterModel			-> setDataSetPackage(package);

	setDatasetLoaded(dataSet != nullptr && dataSet->rowCount() > 0);
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

	QString selectedFilter;
	QString finalPath = MessageForwarder::saveFileBrowse("Save JASP Image", "", "Portable Network Graphics (*.png);;Portable Document Format (*.pdf);;Encapsulated PostScript (*.eps);;300 dpi Tagged Image File (*.tiff)", &selectedFilter);

	if (!finalPath.isEmpty())
	{
		root["type"] = "png";

		if (selectedFilter == "Encapsulated PostScript (*.eps)")			root["type"] = "eps";
		else if (selectedFilter == "Portable Document Format (*.pdf)")		root["type"] = "pdf";
		else if (selectedFilter == "300 dpi Tagged Image File (*.tiff)")	root["type"] = "tiff";

		if(root["type"].asString() != "png")
		{
			root["finalPath"] = finalPath.toStdString();
			analysis->saveImage(root);
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

	Json::Value inputOptions	= results.get("inputOptions", Json::nullValue);
	QString		imagePath		= QString::fromStdString(TempFiles::sessionDirName()) + "/" + results.get("name", Json::nullValue).asCString(),
				finalPath		= QString::fromStdString(inputOptions.get("finalPath", Json::nullValue).asCString());

	if (!finalPath.isEmpty())
	{
#ifdef JASP_DEBUG
		std::cout << "analysisImageSavedHandler, imagePath: " << imagePath.toStdString() << ", finalPath: " << finalPath.toStdString() << std::endl;
#endif

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

    string utf8 = fq(options);
    Json::Value root;
	Json::Reader().parse(utf8, root);

	analysis->editImage(root);

    return;

}

void MainWindow::updateShownVariablesModel()
{
	//if(_currentOptionsWidget != nullptr)
		//_currentOptionsWidget->connectToAvailableVariablesModel(_package->dataSet());

#ifdef JASP_DEBUG
	std::cout << "void MainWindow::updateShownVariablesModel() does not do anything anymore because connectToAvailableVariablesModel is gone, this however is no problem if any new added columns show up in you analysisform. If they do It is probably also nice to remove this warning and function etc!" << std::endl;
#endif
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
			case MessageForwarder::DialogResponse::Cancel:
				event->setComplete(false);
				dataSetIOCompleted(event);
				return;

			case MessageForwarder::DialogResponse::Save:
				event->chain(_fileMenu->save());
				connect(event, &FileEvent::completed, this, &MainWindow::dataSetIOCompleted);
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

		setDataPanelVisible(false);
		setDataAvailable(false);
		_resultsJsInterface->resetResults();

		closeVariablesPage();
	}
}


void MainWindow::closeVariablesPage()
{
	_levelsTableModel->setChosenColumn(-1);
}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
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
	else if (event->operation() == FileEvent::FileClose)
	{
		if (event->successful())
		{
			_analyses->setVisible(false);
			_analyses->clear();
			setDataSetAndPackageInModels(nullptr);
			_loader.free(_package->dataSet());
			_package->reset();
			_resultsJsInterface->resetResults();

			setWindowTitle("JASP");

			if (_applicationExiting)	QApplication::exit();
			else
			{
				setDataPanelVisible(false);
				setDataAvailable(false);
			}

		}
		else if (event->operation() == FileEvent::FileSyncData)
			_package->setModified(true);
		else
			_applicationExiting = false;
	}

}


void MainWindow::populateUIfromDataSet()
{
	setDataSetAndPackageInModels(_package);

	if(_package->dataSet()->rowCount() == 0)
	{
		setDataPanelVisible(false);
		setDataAvailable(false);
	}
	else
	{
		_filterModel->init();
		setDataPanelVisible(true);
		setDataAvailable(true);
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
					_analyses->createFromJaspFileEntry(analysisData);
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
	checkUsedModules();
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
	for(ComputedColumn * col : *_package->computedColumnsPointer())
		if(col->analysisId() != -1)
			col->setAnalysis(_analyses->get(col->analysisId()));
}


void MainWindow::resultsPageLoaded()
{
	std::cout << "void MainWindow::resultsPageLoaded(bool success) needs some love for WIN32" << std::endl;
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


	if (!_engineSync->engineStarted())
		_engineSync->start(_preferences->plotPPI());

	if (_openOnLoadFilename != "")
	{
		_fileMenu->open(_openOnLoadFilename);
		_openOnLoadFilename = "";
	}

	_resultsViewLoaded = true;
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


void MainWindow::emptyValuesChangedHandler()
{
	if (_package->isLoaded())
	{
		vector<string> colChanged;
		vector<string> missingColumns;
		map<string, string> changeNameColumns;

		_package->pauseEngines();
		_package->dataSet()->setSynchingData(true);

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

		_package->dataSet()->setSynchingData(false);
		_package->resumeEngines();
		_package->setModified(true);
		packageDataChanged(_package, colChanged, missingColumns, changeNameColumns, false);
	}
}


void MainWindow::addAnalysisFromDynamicModule(Modules::AnalysisEntry * entry)
{
	std::cout << "void MainWindow::addAnalysisFromDynamicModule(Modules::AnalysisEntry * entry) should be a function of RibbonModel that sends a signal to Analyses (or something)" << std::endl;
	try
	{
		_analyses->create(entry);
		checkUsedModules(); //Ought to been done all through RibbonModel + Analyses
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
	setDataAvailable(true);
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

void MainWindow::setAnalysesVisible(bool analysesVisible)
{
	if (_analysesVisible == analysesVisible)
		return;

	_analysesVisible = analysesVisible;
	emit analysesVisibleChanged(_analysesVisible);
}

void MainWindow::setDatasetLoaded(bool datasetLoaded)
{
	if (_datasetLoaded == datasetLoaded)
		return;

	_datasetLoaded = datasetLoaded;
	emit datasetLoadedChanged(_datasetLoaded);
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
#ifdef JASP_DEBUG
	std::cout << "MainWindow::setAnalysesAvailable(" << (analysesAvailable ? "true" : "false") << ")" << std::endl;
#endif

	if (_analysesAvailable == analysesAvailable)
		return;

	_analysesAvailable = analysesAvailable;
	emit analysesAvailableChanged(_analysesAvailable);
}
