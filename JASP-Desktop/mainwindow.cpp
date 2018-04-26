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
#include "ui_mainwindow.h"

#include "analysisforms/Common/ancovabayesianform.h"
#include "analysisforms/Common/ancovaform.h"
#include "analysisforms/Common/ancovamultivariateform.h"
#include "analysisforms/Common/anovabayesianform.h"
#include "analysisforms/Common/anovaform.h"
#include "analysisforms/Common/anovamultivariateform.h"
#include "analysisforms/Common/anovaonewayform.h"
#include "analysisforms/Common/anovarepeatedmeasuresbayesianform.h"
#include "analysisforms/Common/anovarepeatedmeasuresform.h"
#include "analysisforms/Common/binomialtestbayesianform.h"
#include "analysisforms/Common/binomialtestform.h"
#include "analysisforms/Common/contingencytablesbayesianform.h"
#include "analysisforms/Common/contingencytablesform.h"
#include "analysisforms/Common/correlationbayesianform.h"
#include "analysisforms/Common/correlationbayesianpairsform.h"
#include "analysisforms/Common/correlationform.h"
#include "analysisforms/Common/correlationpartialform.h"
#include "analysisforms/Common/descriptivesform.h"
#include "analysisforms/Common/exploratoryfactoranalysisform.h"
#include "analysisforms/Common/principalcomponentanalysisform.h"
#include "analysisforms/Common/regressionlinearbayesianform.h"
#include "analysisforms/Common/regressionlinearform.h"
#include "analysisforms/Common/regressionlogisticform.h"
#include "analysisforms/Common/regressionloglinearbayesianform.h"
#include "analysisforms/Common/regressionloglinearform.h"
#include "analysisforms/Common/reliabilityanalysisform.h"
#include "analysisforms/Common/ttestbayesianindependentsamplesform.h"
#include "analysisforms/Common/ttestbayesianonesampleform.h"
#include "analysisforms/Common/ttestbayesianpairedsamplesform.h"
#include "analysisforms/Common/ttestindependentsamplesform.h"
#include "analysisforms/Common/ttestonesampleform.h"
#include "analysisforms/Common/ttestpairedsamplesform.h"
#include "analysisforms/Common/multinomialtestform.h"

#include "analysisforms/SummaryStatistics/summarystatsbinomialtestbayesianform.h"
#include "analysisforms/SummaryStatistics/summarystatscorrelationbayesianpairsform.h"
#include "analysisforms/SummaryStatistics/summarystatsregressionlinearbayesianform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianindependentsamplesform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianonesampleform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianpairedsamplesform.h"

#include "analysisforms/SEM/semsimpleform.h"

#include "analysisforms/ReinforcementLearning/reinforcementlearningr11tlearningform.h"

#include "analysisforms/Network/networkanalysisform.h"

#include "analysisforms/MetaAnalysis/classicalmetaanalysisform.h"


///// 1-analyses headers

#include <QDebug>
#include <QFile>
#include <QFileInfo>
#include <QToolTip>
#include <QMessageBox>
#include <QStringBuilder>
#include <QDropEvent>
#include <QShortcut>
#include <QDesktopWidget>
#include <QTabBar>
#include <QMenuBar>
#include <QDir>
#include <QFileDialog>
#include <QDesktopServices>
#include <QQmlContext>
#include <QQuickItem>

#include "analysisloader.h"

#include "qutils.h"
#include "appdirs.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "appinfo.h"

#include "activitylog.h"
#include "aboutdialog.h"
#include "preferencesdialog.h"
#include <boost/filesystem.hpp>
#include "dirs.h"
#include "qutils.h"
#include "column.h"
#include "sharedmemory.h"
#include "module.h"

#include "options/optionvariablesgroups.h"


using namespace std;

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	ui->setupUi(this);
	_log = NULL;
	_tableModel = NULL;
	_currentOptionsWidget = NULL;
	_currentAnalysis = NULL;

	_resultsJsInterface = new ResultsJsInterface(this);

	_package = new DataSetPackage();

	_package->isModifiedChanged.connect(boost::bind(&MainWindow::packageChanged, this, _1));
	_package->dataChanged.connect(boost::bind(&MainWindow::packageDataChanged, this, _1, _2, _3, _4));

	QShortcut *saveShortcut = new QShortcut(QKeySequence("Ctrl+S"), this);
	QObject::connect(saveShortcut, SIGNAL(activated()), this, SLOT(saveKeysSelected()));
	QShortcut *openShortcut = new QShortcut(QKeySequence("Ctrl+O"), this);
	QObject::connect(openShortcut, SIGNAL(activated()), this, SLOT(openKeysSelected()));
	QShortcut *syncShortcut = new QShortcut(QKeySequence("Ctrl+Y"), this);
	QObject::connect(syncShortcut, SIGNAL(activated()), this, SLOT(syncKeysSelected()));
	QShortcut *refreshShortcut = new QShortcut(QKeySequence("Ctrl+R"), this);
	QObject::connect(refreshShortcut, SIGNAL(activated()), this, SLOT(refreshKeysSelected()));


	int initalTableWidth = 575;

	QList<int> sizes = QList<int>();
	sizes.append(initalTableWidth);
	ui->splitter->setSizes(sizes);

	connect(ui->tabBar, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));
	connect(ui->tabBar, SIGNAL(helpToggled(bool)), this, SLOT(helpToggled(bool)));
	ui->tabBar->init();

	ui->ribbonAnalysis->setDataSetLoaded(false);
	ui->ribbonSEM->setDataSetLoaded(false);
	ui->ribbonReinforcementLearning->setDataSetLoaded(false);
	ui->ribbonSummaryStatistics->setDataSetLoaded(false);
	ui->ribbonMetaAnalysis->setDataSetLoaded(false);
	ui->ribbonNetworkAnalysis->setDataSetLoaded(false);
///// 2-ribbon setDataSetLoaded

	tempfiles_init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	_odm = new OnlineDataManager(this);
	_odm->initAuthentication(OnlineDataManager::OSF);

	_loader.moveToThread(&_loaderThread);
	_loaderThread.start();
	_loader.setOnlineDataManager(_odm);

	ui->backStage->setOnlineDataManager(_odm);

	_tableModel				= new DataSetTableModel();
	_levelsTableModel		= new LevelsTableModel(this);
	_labelFilterGenerator	= new labelFilterGenerator(_package, this);
	_columnsModel			= new ColumnsModel(this);

	connect(_levelsTableModel,		&LevelsTableModel::refreshConnectedModels,			_tableModel,			&DataSetTableModel::refreshColumn);
	connect(_levelsTableModel,		&LevelsTableModel::notifyColumnHasFilterChanged,	_tableModel,			&DataSetTableModel::notifyColumnFilterStatusChanged);
	connect(_levelsTableModel,		&LevelsTableModel::resizeValueColumn,				this,					&MainWindow::resizeVariablesWindowValueColumn);
	connect(_levelsTableModel,		&LevelsTableModel::labelFilterChanged,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged);
	connect(_labelFilterGenerator,	&labelFilterGenerator::setGeneratedFilter,			this,					&MainWindow::setGeneratedFilterAndSend);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_labelFilterGenerator,	&labelFilterGenerator::labelFilterChanged);
	connect(_tableModel,			&DataSetTableModel::allFiltersReset,				_levelsTableModel,		&LevelsTableModel::refresh);
	connect(_tableModel,			&DataSetTableModel::headerDataChanged,				_columnsModel,			&ColumnsModel::datasetHeaderDataChanged);

	_analyses = new Analyses();
	_engineSync = new EngineSync(_analyses, _package, this);

	connect(_engineSync, &EngineSync::filterUpdated,			_tableModel,	&DataSetTableModel::refresh);
	connect(_engineSync, &EngineSync::filterErrorTextChanged,	this,			&MainWindow::setFilterErrorText);
	connect(_engineSync, &EngineSync::filterUpdated,			this,			&MainWindow::onFilterUpdated);

	loadQML();

	connect(_engineSync, &EngineSync::engineTerminated,		this,					&MainWindow::fatalError);

	connect(_analyses, &Analyses::analysisResultsChanged,	this,					&MainWindow::analysisResultsChangedHandler);
	connect(_analyses, &Analyses::analysisImageSaved,		this,					&MainWindow::analysisImageSavedHandler);
	connect(_analyses, &Analyses::analysisAdded,			ui->backStage,			&BackStageWidget::analysisAdded);
	connect(_analyses, &Analyses::analysisImageEdited,		_resultsJsInterface,	&ResultsJsInterface::analysisImageEditedHandler);

	//connect some ribbonbuttons?
	connect(ui->ribbonAnalysis,					QOverload<QString>::of(&RibbonAnalysis::itemSelected),				this, &MainWindow::itemSelected);
	connect(ui->ribbonSEM,						QOverload<QString>::of(&RibbonSEM::itemSelected),					this, &MainWindow::itemSelected);
	connect(ui->ribbonReinforcementLearning,	QOverload<QString>::of(&RibbonReinforcementLearning::itemSelected),	this, &MainWindow::itemSelected);
	connect(ui->ribbonSummaryStatistics,		QOverload<QString>::of(&RibbonSummaryStatistics::itemSelected),		this, &MainWindow::itemSelected);
	connect(ui->ribbonMetaAnalysis,				QOverload<QString>::of(&RibbonMetaAnalysis::itemSelected),			this, &MainWindow::itemSelected);
	connect(ui->ribbonNetworkAnalysis,			QOverload<QString>::of(&RibbonNetworkAnalysis::itemSelected),		this, &MainWindow::itemSelected);

	connect(ui->backStage,						&BackStageWidget::dataSetIORequest,			this, &MainWindow::dataSetIORequest);
	connect(ui->backStage,						&BackStageWidget::exportSelected, _resultsJsInterface, &ResultsJsInterface::exportSelected);


	connect(ui->tabBar, &TabBar::dataAutoSynchronizationChanged, ui->backStage,			&BackStageWidget::dataAutoSynchronizationChanged);
	connect(ui->tabBar, &TabBar::setExactPValuesHandler,		_resultsJsInterface,	&ResultsJsInterface::setExactPValuesHandler);
	connect(ui->tabBar, &TabBar::setFixDecimalsHandler,			_resultsJsInterface,	&ResultsJsInterface::setFixDecimalsHandler);
	connect(ui->tabBar, &TabBar::emptyValuesChangedHandler,		this,					&MainWindow::emptyValuesChangedHandler);

	connect(&_loader,	&AsyncLoader::progress,					this,					&MainWindow::setProgressStatus);


#ifdef __APPLE__
	_scrollbarWidth = 3;
#else
	_scrollbarWidth = qApp->style()->pixelMetric(QStyle::PM_ScrollBarExtent);
#endif

	_buttonPanel = new QWidget(ui->panel_2_Options);
	_buttonPanelLayout = new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, _buttonPanelLayout->contentsMargins().top(), _buttonPanelLayout->contentsMargins().right(), 0);

	_buttonPanel->setLayout(_buttonPanelLayout);

	_okButton = new QPushButton(QString("OK"), _buttonPanel);
	_okButton->setDefault(true);
	_runButton = new QPushButton(QString("Run"), _buttonPanel);

	QMenuBar *_mMenuBar = new QMenuBar(parent=0);
	QMenu *aboutMenu = _mMenuBar->addMenu("JASP");
	aboutMenu->addAction("About",ui->tabBar,SLOT(showAbout()));
	_mMenuBar->addMenu(aboutMenu);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_runButton);
	_buttonPanelLayout->addStretch();

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);

	connect(_okButton,		&QPushButton::clicked,		this, &MainWindow::analysisOKed);
	connect(_runButton,		&QPushButton::clicked,		this, &MainWindow::analysisRunned);
	connect(ui->splitter,	&QSplitter::splitterMoved,	this, &MainWindow::splitterMovedHandler);

	_tableViewWidthBeforeOptionsMadeVisible = -1;

	QUrl userGuide = QUrl::fromLocalFile(AppDirs::help() + "/index.html");
	ui->webViewHelp->setUrl(userGuide);
	connect(ui->webViewHelp, &CustomWebEngineView::loadFinished, this, &MainWindow::helpFirstLoaded);
	ui->panel_4_Help->hide();

	setAcceptDrops(true);

	ui->panel_1_Data->hide();
	ui->panel_2_Options->hide();

	// init Empty Values
	QString missingvaluestring = _settings.value("MissingValueList", "").toString();
	if (missingvaluestring != "")
	{
		QString delimetor = "|";
		std::vector<std::string> missingvalues = fromQstringToStdVector(missingvaluestring, delimetor);
		Utils::setEmptyValues(missingvalues);
	}
}

void MainWindow::loadQML()
{
	ui->quickWidget_Data->rootContext()->setContextProperty("mainWindow",		this);
	ui->quickWidget_Data->rootContext()->setContextProperty("dataSetModel",		_tableModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("levelsTableModel", _levelsTableModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("columnsModel",		_columnsModel);
	ui->quickWidget_Data->rootContext()->setContextProperty("engineSync",		_engineSync);
	ui->quickWidget_Data->rootContext()->setContextProperty("filterErrorText",	QString(""));
	ui->quickWidget_Data->rootContext()->setContextProperty("generatedFilter",	QString(""));
	ui->quickWidget_Data->rootContext()->setContextProperty("defaultFilter",	QString(DEFAULT_FILTER));

	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeScale",			int(Column::ColumnType::ColumnTypeScale));
	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeOrdinal",		int(Column::ColumnType::ColumnTypeOrdinal));
	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeNominal",		int(Column::ColumnType::ColumnTypeNominal));
	ui->quickWidget_Data->rootContext()->setContextProperty("columnTypeNominalText",	int(Column::ColumnType::ColumnTypeNominalText));

	setFilterConstructorJSON(QString::fromStdString(_package->filterConstructorJSON));

	ui->quickWidget_Data->setSource(QUrl(QString("qrc:///qml/dataset.qml")));

	QObject * DataView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("dataSetTableView");
	connect(DataView, SIGNAL(dataTableDoubleClicked()), this, SLOT(startDataEditorHandler()));

	QObject * levelsTableView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("levelsTableView");
	connect(levelsTableView, SIGNAL(columnChanged(QString)), this, SLOT(refreshAnalysesUsingColumn(QString)));

	QObject * easyFilterConstructor = ui->quickWidget_Data->rootObject()->findChild<QObject*>("filterWindow");
	connect(easyFilterConstructor, SIGNAL(rCodeChanged(QString)), _labelFilterGenerator, SLOT(easyFilterConstructorRCodeChanged(QString)));

	qmlProgressBar	= ui->quickWidget_Data->rootObject()->findChild<QObject*>("progressBarHolder");
	qmlFilterWindow = ui->quickWidget_Data->rootObject()->findChild<QObject*>("filterWindow");
	qmlStatusBar	= ui->quickWidget_Data->rootObject()->findChild<QObject*>("dataStatusBar");
}


void MainWindow::open(QString filepath)
{
	_openedUsingArgs = true;
	if (_resultsViewLoaded)
		ui->backStage->open(filepath);
	else
		_openOnLoadFilename = filepath;
}


MainWindow::~MainWindow()
{
	delete _engineSync;
	if (_package && _package->dataSet)
	{
		_loader.free(_package->dataSet);
		_package->reset();
	}
	delete ui;
}


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
		ui->backStage->close();
		event->ignore();
	}
	else
	{
		event->accept();
	}

	PreferencesDialog *rd = ui->tabBar->getPreferencesDialog();
	if (rd) rd->close();
}

bool MainWindow::filterShortCut()
{
	bool exclude = false;

#ifdef __APPLE__
	// This is a workaround for Qt Bug https://bugreports.qt.io/browse/QTBUG-67016
	// When we move to a Qt version (probably 5.11) where this bug is solved, we have to remove this workaround!
	static int counter = 0;
	counter++;
	
	if (counter % 3 != 1)
		exclude = true;
#endif
	
	return exclude;
}

void MainWindow::saveKeysSelected()
{
	if (filterShortCut())
		return;
	
	if (_package->isModified())
	{
		ui->backStage->save();
	}
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


void MainWindow::syncKeysSelected()
{
	if (filterShortCut())
		return;

	ui->backStage->sync();
}


void MainWindow::illegalOptionStateChanged()
{
	if (_currentOptionsWidget == NULL)
		return;

	if (_currentOptionsWidget->hasIllegalValue())
	{
		ui->optionsErrorLabel->setText(_currentOptionsWidget->illegalValueMessage());
		ui->optionsErrorPanel->show();
	}
	else
	{
		ui->optionsErrorPanel->hide();
	}
}


void MainWindow::packageChanged(DataSetPackage *package)
{
	QString title = windowTitle();
	if (package->isModified())
	{
		setWindowTitle(title.append("*"));
	}
	else
	{
		title.chop(1);
		setWindowTitle(title);
	}
}


void MainWindow::refreshAnalysesUsingColumns(vector<string> &changedColumns,
											 vector<string> &missingColumns,
											 map<string, string> &changeNameColumns)
{
	vector<string> oldColumnNames;
	for (map<string, string>::iterator it = changeNameColumns.begin(); it != changeNameColumns.end(); ++it)
		oldColumnNames.push_back(it->first);
	sort(changedColumns.begin(), changedColumns.end());
	sort(missingColumns.begin(), missingColumns.end());
	sort(oldColumnNames.begin(), oldColumnNames.end());

	set<Analysis *> analysesToRefresh;
	for (Analyses::iterator analysisIt = _analyses->begin(); analysisIt != _analyses->end(); ++analysisIt)
	{
		Analysis* analysis = *analysisIt;
		if (analysis == NULL) continue;

		Options* options = analysis->options();
		for (size_t i = 0; i < options->size(); ++i)
		{
			Option *option = options->get(i);
			OptionVariableI *optionVariables = dynamic_cast<OptionVariableI *>(option);
			if (optionVariables != NULL)
			{
				vector<string> variables = optionVariables->variables();
				if (!variables.empty())
				{
					sort(variables.begin(), variables.end());
					vector<string> interChangecol, interChangename, interMissingcol;
					set_intersection(variables.begin(), variables.end(), changedColumns.begin(), changedColumns.end(), back_inserter(interChangecol));
					set_intersection(variables.begin(), variables.end(), oldColumnNames.begin(), oldColumnNames.end(), back_inserter(interChangename));
					set_intersection(variables.begin(), variables.end(), missingColumns.begin(), missingColumns.end(), back_inserter(interMissingcol));

					if (interChangecol.size() > 0)
					{
						analysesToRefresh.insert(analysis);
					}

					if (interChangename.size() > 0)
					{
						analysis->setRefreshBlocked(true);
						for (vector<string>::iterator varnameIt = interChangename.begin(); varnameIt != interChangename.end(); ++varnameIt)
						{
							string varname = *varnameIt;
							string newname = changeNameColumns[varname];
							optionVariables->replaceName(varname, newname);
						}
						analysesToRefresh.insert(analysis);
					}

					if (interMissingcol.size() > 0)
					{
						analysis->setRefreshBlocked(true);
						for (vector<string>::iterator varnameIt= interMissingcol.begin(); varnameIt!= interMissingcol.end(); ++varnameIt)
						{
							optionVariables->removeName(*varnameIt);
							analysesToRefresh.insert(analysis);
						}
					}
				}
			}
		}
	}

	for (set<Analysis *>::iterator it = analysesToRefresh.begin(); it != analysesToRefresh.end(); ++it)
	{
		Analysis *analysis = *it;
		analysis->setRefreshBlocked(false);
		analysis->refresh();
	}
}


void MainWindow::packageDataChanged(DataSetPackage *package,
									vector<string> &changedColumns,
									vector<string> &missingColumns,
									map<string, string> &changeNameColumns)
{
	_tableModel->setDataSet(_package->dataSet);
	_levelsTableModel->setDataSet(_package->dataSet);
	_columnsModel->setDataSet(_package->dataSet);
	triggerQmlColumnReload();

	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns);
}




void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	static bool showInstructions = true;

	if (showInstructions)
	{
		if (_settings.value("instructionsShown", false).toBool() == false)
		{
			_settings.setValue("instructionsShown", true);
			_resultsJsInterface->showInstruction();
		}

		showInstructions = false;
	}

	if (analysis->status() == Analysis::Running || (analysis->status() == Analysis::Inited && analysis->isAutorun()))
	{
		_runButton->setEnabled(true);
		_runButton->setText("Stop");
	}
	else
	{
		_runButton->setText("Run");
		if (analysis->status() == Analysis::InitedAndWaiting)
			_runButton->setEnabled(true);
		else
			_runButton->setEnabled(false);
	}

	_resultsJsInterface->analysisChanged(analysis);

	if (_package->isLoaded())
		_package->setModified(true);
}


void MainWindow::analysisSaveImageHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == NULL)
		return;

	string utf8 = fq(options);
	Json::Value root;
	Json::Reader parser;
	parser.parse(utf8, root);

	QString caption = "Save JASP Image";
	QString filter = "Portable Network Graphics (*.png);;Portable Document Format (*.pdf);;Encapsulated PostScript (*.eps);;300 dpi Tagged Image File (*.tiff)";
	QString selectedFilter;

	QString finalPath = QFileDialog::getSaveFileName(this, caption, QString(), filter, &selectedFilter);
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
			QString imagePath = QString::fromStdString(tempfiles_sessionDirName()) + "/" + root.get("name", Json::nullValue).asCString();
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

	QString imagePath = QString::fromStdString(tempfiles_sessionDirName()) + "/" + results.get("name", Json::nullValue).asCString();
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
    if (analysis == NULL)
        return;

    string utf8 = fq(options);
    Json::Value root;
    Json::Reader parser;
    parser.parse(utf8, root);

    analysis->editImage(analysis, root);

    return;

}

AnalysisForm* MainWindow::loadForm(Analysis *analysis)
{
	return loadForm(analysis->name());
}


AnalysisForm* MainWindow::loadForm(const string name)
{
	AnalysisForm *form = NULL;

	QWidget *contentArea = ui->optionsContentArea;

	if (name == "Descriptives")
		form = new DescriptivesForm(contentArea);
	else if (name == "TTestBayesianOneSample")
		form = new TTestBayesianOneSampleForm(contentArea);
	else if (name == "TTestBayesianIndependentSamples")
		form = new TTestBayesianIndependentSamplesForm(contentArea);
	else if (name == "TTestBayesianPairedSamples")
		form = new TTestBayesianPairedSamplesForm(contentArea);
	else if (name == "TTestIndependentSamples")
		form = new TTestIndependentSamplesForm(contentArea);
	else if (name == "TTestPairedSamples")
		form = new TTestPairedSamplesForm(contentArea);
	else if (name == "TTestOneSample")
		form = new TTestOneSampleForm(contentArea);
	else if (name == "AnovaBayesian")
		form = new AnovaBayesianForm(contentArea);
	else if (name == "AnovaOneWay")
		form = new AnovaOneWayForm(contentArea);
	else if (name == "Anova")
		form = new AnovaForm(contentArea);
	else if (name == "AnovaRepeatedMeasures")
		form = new AnovaRepeatedMeasuresForm(contentArea);
	else if (name == "Ancova")
		form = new AncovaForm(contentArea);
	else if (name == "AnovaMultivariate")
		form = new AnovaMultivariateForm(contentArea);
	else if (name == "AncovaMultivariate")
		form = new AncovaMultivariateForm(contentArea);
	else if (name == "RegressionLinear")
		form = new RegressionLinearForm(contentArea);
	else if (name == "RegressionLinearBayesian")
		form = new RegressionLinearBayesianForm(contentArea);
	else if (name == "RegressionLogistic")
		form = new RegressionLogisticForm(contentArea);
	else if (name == "RegressionLogLinear")
		form = new RegressionLogLinearForm(contentArea);
	else if (name == "RegressionLogLinearBayesian")
		form = new RegressionLogLinearBayesianForm(contentArea);
	else if (name == "Correlation")
		form = new CorrelationForm(contentArea);
	else if (name == "CorrelationBayesian")
		form = new CorrelationBayesianForm(contentArea);
	else if (name == "CorrelationBayesianPairs")
		form = new CorrelationBayesianPairsForm(contentArea);
	else if (name == "CorrelationPartial")
		form = new CorrelationPartialForm(contentArea);
	else if (name == "ContingencyTables")
		form = new ContingencyTablesForm(contentArea);
	else if (name == "ContingencyTablesBayesian")
		form = new ContingencyTablesBayesianForm(contentArea);
	else if (name == "SEMSimple")
		form = new SEMSimpleForm(contentArea);
	else if (name == "AncovaBayesian")
		form = new AncovaBayesianForm(contentArea);
	else if (name == "AnovaRepeatedMeasuresBayesian")
		form = new AnovaRepeatedMeasuresBayesianForm(contentArea);
	else if (name == "BinomialTest")
		form = new BinomialTestForm(contentArea);
	else if (name == "MultinomialTest")
		form = new MultinomialTestForm(contentArea);
	else if (name == "BinomialTestBayesian")
		form = new BinomialTestBayesianForm(contentArea);
	else if (name == "ReliabilityAnalysis")
		form = new ReliabilityAnalysisForm(contentArea);
	else if (name == "ExploratoryFactorAnalysis")
		form = new ExploratoryFactorAnalysisForm(contentArea);
	else if (name == "PrincipalComponentAnalysis")
		form = new PrincipalComponentAnalysisForm(contentArea);
	else if (name == "SummaryStatsTTestBayesianOneSample")
		form = new SummaryStatsTTestBayesianOneSampleForm(contentArea);
	else if (name == "SummaryStatsTTestBayesianIndependentSamples")
		form = new SummaryStatsTTestBayesianIndependentSamplesForm(contentArea);
	else if (name == "SummaryStatsTTestBayesianPairedSamples")
		form = new SummaryStatsTTestBayesianPairedSamplesForm(contentArea);
	else if (name == "SummaryStatsBinomialTestBayesian")
		form = new SummaryStatsBinomialTestBayesianForm(contentArea);
	else if (name == "SummaryStatsRegressionLinearBayesian")
		form = new SummaryStatsRegressionLinearBayesianForm(contentArea);
	else if (name == "SummaryStatsCorrelationBayesianPairs")
		form = new SummaryStatsCorrelationBayesianPairsForm(contentArea);
	else if (name == "ClassicalMetaAnalysis")
		form = new ClassicalMetaAnalysisForm(contentArea);
	else if (name == "NetworkAnalysis")
		form = new NetworkAnalysisForm(contentArea);
	else if (name == "ReinforcementLearningR11tLearning")
		form = new ReinforcementLearningR11tLearningForm(contentArea);
///// 4-analysis if-else ladder
	else
		qDebug() << "MainWindow::loadForm(); form not found : " << name.c_str();


	return form;
}


void MainWindow::showForm(Analysis *analysis)
{
	closeCurrentOptionsWidget();

	_currentOptionsWidget = loadForm(analysis);

	if (_currentOptionsWidget != NULL)
	{
		//sizing of options widget and panel to fit buttons and conform to largest size for consistency

		QObjectList siblings = _currentOptionsWidget->children();
		for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
			QWidget* w = dynamic_cast<QWidget*>(*itr);
			if (w != NULL && w->objectName() == "topWidget") {
				w->setContentsMargins(0, 0, _buttonPanel->width(), 0);
				break;
			}
		}

		int requiredSize = _currentOptionsWidget->sizeHint().width();
		int currentOptionSpace = ui->panel_2_Options->minimumWidth() - _scrollbarWidth;
		if (requiredSize > currentOptionSpace) {
			ui->panel_2_Options->setMinimumWidth(requiredSize + _scrollbarWidth);
			_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
		}
		_currentOptionsWidget->setMinimumWidth(ui->panel_2_Options->minimumWidth() - _scrollbarWidth);

		Options *options = analysis->options();
		DataSet *dataSet = _package->dataSet;
		_currentOptionsWidget->bindTo(options, dataSet);

		connect(_currentOptionsWidget, SIGNAL(illegalChanged()), this, SLOT(illegalOptionStateChanged()));
		illegalOptionStateChanged();

		_currentOptionsWidget->show();
		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget,0, 0, Qt::AlignRight | Qt::AlignTop);

		if (ui->panel_2_Options->isVisible() == false)
			showOptionsPanel();

		_okButton->setVisible(_currentAnalysis->useData());
		_runButton->setVisible(_currentAnalysis->isAutorun() == false);
		_runButton->setEnabled(_currentAnalysis->status() == Analysis::InitedAndWaiting);
		_buttonPanel->raise();
		_buttonPanel->show();

		QString helpPage = QString("analyses/") + tq(analysis->name()).toLower();
		requestHelpPage(helpPage);
	}
}


void MainWindow::closeCurrentOptionsWidget()
{
	if (_currentOptionsWidget != NULL)
	{
		disconnect(_currentOptionsWidget, SIGNAL(illegalChanged()), this, SLOT(illegalOptionStateChanged()));

		_currentOptionsWidget->hide();
		_currentOptionsWidget->unbind();
		_currentOptionsWidget = NULL;
	}
}


void MainWindow::analysisSelectedHandler(int id)
{
	_currentAnalysis = _analyses->get(id);

	if (_currentAnalysis != NULL)
	{
		showForm(_currentAnalysis);

		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(id);

		if (_log != NULL)
			_log->log("Analysis Selected", info);
	}
}


void MainWindow::analysisUnselectedHandler()
{
	if (_currentAnalysis->useData())
		hideOptionsPanel();

	if (_log != NULL && _currentAnalysis != NULL)
	{
		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		_log->log("Analysis Unselected", info);
	}
}


void MainWindow::tabChanged(int index)
{
	if (index == 0)
	{
		ui->topLevelWidgets->setCurrentIndex(0);
	}
	else
	{
		ui->topLevelWidgets->setCurrentIndex(1); //Should be a reference to the mainPage

		QString currentActiveTab = ui->tabBar->getCurrentActiveTab();
		if (Module::isModuleName(currentActiveTab))
		{
			const Module& module = Module::getModule(currentActiveTab);
			ui->ribbon->setCurrentIndex(module.ribbonIndex());
		}
	}
}


void MainWindow::helpToggled(bool on)
{
	if (_log != NULL)
		_log->log("Help Toggled", on ? "on" : "off");

	static int helpWidth = 0;

	if (on)
	{
		if (helpWidth < 200)
			helpWidth = 200;

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
	}
}


void MainWindow::checkUsedModules()
{
	QStringList usedModules;
	for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
	{
		Analysis *analysis = *itr;
		if (analysis != NULL && analysis->isVisible())
		{
			QString moduleName = QString::fromStdString(analysis->module());
			if (!usedModules.contains(moduleName))
				usedModules.append(moduleName);
		}
	}

	ui->tabBar->setModulePlusMenu(usedModules);
}


void MainWindow::dataSetIORequest(FileEvent *event)
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
			connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

			_loader.io(event, _package);
			showProgress();
		}

		ui->tabBar->setCurrentModuleActive();
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (_analyses->count() > 0)
		{
			_package->setWaitingForReady();

			getAnalysesUserData();
			_resultsJsInterface->exportPreviewHTML();

			Json::Value analysesDataList = Json::arrayValue;
			for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
			{
				Analysis *analysis = *itr;
				if (analysis != NULL && analysis->isVisible())
				{
					Json::Value analysisData = analysis->asJSON();
					analysisData["options"] = analysis->options()->asJSON();
					analysisData["userdata"] = analysis->userData();
					analysesDataList.append(analysisData);
				}
			}

			Json::Value analysesData = Json::objectValue;
			analysesData["analyses"] = analysesDataList;

			analysesData["meta"] = _resultsJsInterface->getResultsMeta();

			_package->analysesData = analysesData;
			_package->hasAnalyses = true;
		}

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

		_resultsJsInterface->exportHTML();

		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileExportData)
	{
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (_package->dataSet == NULL)
			return;

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
		_loader.io(event, _package);
		showProgress();
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		
		if (_package->isModified())
		{
			QString title = windowTitle();
			title.chop(1);
			QMessageBox::StandardButton reply = QMessageBox::warning(this, "Save Workspace?", QString("Save changes to workspace \"") + title + QString("\" before closing?\n\nYour changes will be lost if you don't save them."), QMessageBox::Save|QMessageBox::Discard|QMessageBox::Cancel);

			if (reply == QMessageBox::Save)
			{
				FileEvent *saveEvent = ui->backStage->save();
				event->chain(saveEvent);
				connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
				ui->panel_1_Data->hide();
			}
			else if (reply == QMessageBox::Cancel)
			{
				event->setComplete(false);
				dataSetIOCompleted(event);
			}
			else if (reply == QMessageBox::Discard)
			{
				event->setComplete(true);
				dataSetIOCompleted(event);
				ui->panel_1_Data->hide();
			}
		}
		else
		{
			event->setComplete();
			dataSetIOCompleted(event);
		}

		closeVariablesPage();
	}
}

void MainWindow::resizeVariablesWindowValueColumn()
{
	QObject * levelsTableView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("levelsTableView");
	QMetaObject::invokeMethod(levelsTableView, "resizeValueColumn");
}

void MainWindow::closeVariablesPage()
{
	QObject * levelsTableView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("levelsTableView");
	QMetaObject::invokeMethod(levelsTableView, "closeYourself");
}


void MainWindow::triggerQmlColumnReload()
{
	QObject * DataView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("dataSetTableView");

	if(DataView != NULL)
		QMetaObject::invokeMethod(DataView, "reloadColumns");

}

void MainWindow::triggerQmlColumnClear()
{
	QObject * DataView = ui->quickWidget_Data->rootObject()->findChild<QObject*>("dataSetTableView");

	if(DataView != NULL)
		QMetaObject::invokeMethod(DataView, "clearColumns");

}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
	this->analysisOKed();
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

			if (event->type() == Utils::FileType::jasp && !_package->dataFilePath.empty() && !_package->dataFileReadOnly && strncmp("http", _package->dataFilePath.c_str(), 4) != 0)
			{
				QString dataFilePath = QString::fromStdString(_package->dataFilePath);
				if (QFileInfo::exists(dataFilePath))
				{
					uint currentDataFileTimestamp = QFileInfo(dataFilePath).lastModified().toTime_t();
					if (currentDataFileTimestamp > _package->dataFileTimestamp)
						emit event->dataFileChanged(event->dataFilePath());
				}
				else
				{
					_package->dataFilePath = "";
					_package->setModified(true);
				}
			}
		}
		else
		{
			if (_package->dataSet != NULL)
				_loader.free(_package->dataSet);
			_package->reset();
			_columnsModel->setDataSet(NULL);

			QMessageBox::warning(this, "", "Unable to open file.\n\n" + event->message());

			if (_openedUsingArgs)
				close();
		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (event->successful())
		{
			QString name = QFileInfo(event->path()).baseName();

			_package->setModified(false);
			setWindowTitle(name);
			showAnalysis = true;
		}
		else
		{
			QMessageBox::warning(this, "", "Unable to save file.\n\n" + event->message());
		}
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		_package->setModified(true);
		showAnalysis = true;
	}
	else if (event->operation() == FileEvent::FileExportData || event->operation() == FileEvent::FileExportResults)
	{
		showAnalysis = true;
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (event->successful())
		{
			_analyses->clear();
			closeCurrentOptionsWidget();
			hideOptionsPanel();
			_tableModel->clearDataSet();
			_levelsTableModel->setDataSet(NULL);
			_columnsModel->setDataSet(NULL);
			_loader.free(_package->dataSet);
			_package->reset();
			updateMenuEnabledDisabledStatus();
			ui->webViewResults->reload();
			setWindowTitle("JASP");
			triggerQmlColumnClear();
			setFilterConstructorJSON("");

			if (_applicationExiting)
				QApplication::exit();
			else
				ui->panel_1_Data->hide();
		}
		else
		{
			_applicationExiting = false;
		}
	}

	if (showAnalysis)
	{
		ui->tabBar->setCurrentModuleActive();
	}
}


void MainWindow::populateUIfromDataSet()
{
	_tableModel->setDataSet(_package->dataSet);
	_levelsTableModel->setDataSet(_package->dataSet);
	_columnsModel->setDataSet(_package->dataSet);


	if(_package->dataSet->rowCount() == 0)
		ui->panel_1_Data->hide(); //for summary stats etc we dont want to see an empty data panel
	else
	{
		triggerQmlColumnReload();
		setGeneratedFilter(QString::fromStdString(labelFilterGenerator(_package).generateFilter()));
		setFilterConstructorJSON(QString::fromStdString(_package->filterConstructorJSON));
		applyAndSendFilter(QString::fromStdString(_package->dataFilter));
	}


	
	hideProgress();

	bool errorFound = false;
	stringstream errorMsg;

	if (_package->hasAnalyses)
	{
		int corruptAnalyses = 0;

		stringstream corruptionStrings;

		Json::Value analysesData = _package->analysesData;
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

			for (Json::ValueIterator iter = analysesDataList.begin(); iter != analysesDataList.end(); iter++)
			{
				try
				{
					Json::Value &analysisData = *iter;

					QString name = QString::fromStdString(analysisData["name"].asString());
					QString module = QString::fromStdString(analysisData["module"].asString());
					if (module.isEmpty())
						module = "Common";
					int id = analysisData["id"].asInt();

					Json::Value &optionsJson = analysisData["options"];
					Json::Value &resultsJson = analysisData["results"];
					Json::Value &userDataJson = analysisData["userdata"];
					Json::Value &versionJson = analysisData["version"];

					Version version = versionJson.isNull() ? AppInfo::version : Version(versionJson.asString());

					Analysis::Status status = Analysis::parseStatus(analysisData["status"].asString());

					Analysis *analysis = _analyses->create(module, name, id, version, &optionsJson, status);

					analysis->setUserData(userDataJson);
					analysis->setResults(resultsJson);
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
			errorMsg << "An error was detected in an analyses. This analyses has been removed for the following reason:\n" << corruptionStrings.str();
		else if (corruptAnalyses > 1)
			errorMsg << "Errors were detected in " << corruptAnalyses << " analyses. These analyses have been removed for the following reasons:\n" << corruptionStrings.str();
	}

	if (_package->warningMessage != "")
		QMessageBox::warning(this, "", tq(_package->warningMessage));
	else if (errorFound)
		QMessageBox::warning(this, "", tq(errorMsg.str()));

	_package->setLoaded();
	updateMenuEnabledDisabledStatus();
	checkUsedModules();
}


void MainWindow::updateMenuEnabledDisabledStatus()
{
	bool loaded = _package->isLoaded();

	ui->ribbonAnalysis->setDataSetLoaded(loaded);
	ui->ribbonSEM->setDataSetLoaded(loaded);
	ui->ribbonReinforcementLearning->setDataSetLoaded(loaded);
	ui->ribbonMetaAnalysis->setDataSetLoaded(loaded);
	ui->ribbonNetworkAnalysis->setDataSetLoaded(loaded);
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
		_engineSync->setPPI(ppi);

		if (_openOnLoadFilename != "")
		{
			ui->backStage->open(_openOnLoadFilename);
			_openOnLoadFilename = "";
		}

		_resultsViewLoaded = true;
	}

	if (_engineSync->engineStarted() == false)
		_engineSync->start();
}


void MainWindow::fatalError()
{
	if (_log != NULL)
		_log->log("Terminal Error");

	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;

		QMessageBox::warning(this, "Error", "JASP has experienced an unexpected internal error.\n\n\"" + _fatalError + "\"\n\nIf you could report your experiences to the JASP team that would be appreciated.\n\nJASP cannot continue and will now close.\n\n");
		QApplication::exit(1);
	}
}


void MainWindow::helpFirstLoaded(bool ok)
{
	if (ok)
		requestHelpPage("index");
}


void MainWindow::requestHelpPage(const QString &pageName)
{
	QFile file(AppDirs::help() + "/" + pageName + ".md");

	QString content;

	if (file.exists())
	{
		file.open(QFile::ReadOnly);
		content = QString::fromUtf8(file.readAll());
		file.close();
	}
	else
	{
		content = "Coming Soon!\n========\n\nThere is currently no help available for this analysis.\n\nAdditional documentation will be available in future releases of JASP.";
	}

	content.replace("\"", "\\\"");
	content.replace("\r\n", "\\n");
	content.replace("\r", "\\n");
	content.replace("\n", "\\n");

	QString js = "window.render(\"" + content + "\")";

	ui->webViewHelp->page()->runJavaScript(js);
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
			colChanged =_package->dataSet->resetEmptyValues(_package->emptyValuesMap);
		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {

				_package->dataSet = SharedMemory::enlargeDataSet(_package->dataSet);
				colChanged =_package->dataSet->resetEmptyValues(_package->emptyValuesMap);
			}
			catch (exception &e)
			{
				throw runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (exception e)
		{
			cout << "n " << e.what();
			cout.flush();
		}
		catch (...)
		{
			cout << "something when wrong...\n ";
			cout.flush();
		}

		_package->setModified(true);
		packageDataChanged(_package, colChanged, missingColumns, changeNameColumns);
	}
}

void MainWindow::itemSelected(const QString &item)
{
	try
	{
		QString currentActiveTab = ui->tabBar->getCurrentActiveTab();
		const Module& module = Module::getModule(currentActiveTab);

		_currentAnalysis = _analyses->create(module.name(), item);

		showForm(_currentAnalysis);
		_resultsJsInterface->showAnalysis(_currentAnalysis->id());

		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		checkUsedModules();

		if (_log != NULL)
			_log->log("Analysis Created", info);
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
		_package->analysesHTML = fq(data);
		_package->setAnalysesHTMLReady();
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

void MainWindow::adjustOptionsPanelWidth()
{
	if (ui->panel_2_Options->width() == ui->panel_2_Options->maximumWidth() && ui->panel_1_Data->isHidden())
	{
		showDataPanel();
	}
	else if (ui->panel_1_Data->width() == ui->panel_1_Data->minimumWidth() && ui->panel_1_Data->isVisible() && ui->panel_2_Options->isVisible())
	{
		hideDataPanel();
	}

	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
}


void MainWindow::splitterMovedHandler(int, int)
{
	adjustOptionsPanelWidth();
	_tableViewWidthBeforeOptionsMadeVisible = -1;
}


void MainWindow::hideOptionsPanel()
{
	int newTableWidth = 0;

	QList<int> sizes = ui->splitter->sizes();

	if (_tableViewWidthBeforeOptionsMadeVisible > 0)
	{
		newTableWidth = _tableViewWidthBeforeOptionsMadeVisible;
	}
	else
	{
		newTableWidth += sizes.at(0);

		if (ui->panel_1_Data->isVisible())
			newTableWidth += ui->splitter->handleWidth() + 2;

		newTableWidth += sizes.at(1);
	}

	sizes[0] = newTableWidth;
	sizes[1] = 0;

	ui->panel_2_Options->hide();
	if(_package != NULL && _package->dataSet != NULL && _package->dataSet->rowCount() > 0)
		ui->panel_1_Data->show();
	else
		ui->panel_1_Data->hide();
	ui->splitter->setSizes(sizes);
}


void MainWindow::showOptionsPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	int tableWidth = sizes.at(0);
	int newTableWidth = tableWidth;
	newTableWidth -= ui->panel_2_Options->minimumWidth();
	newTableWidth -= ui->splitter->handleWidth();

	ui->panel_2_Options->show();

	if (newTableWidth < ui->panel_1_Data->minimumWidth())
	{
		int midPanelWidth = tableWidth;
		if (midPanelWidth < ui->panel_2_Options->minimumWidth())
		{
			_tableViewWidthBeforeOptionsMadeVisible = midPanelWidth;
			midPanelWidth = ui->panel_2_Options->minimumWidth();
		}

		int w = 0;
		w += ui->panel_2_Options->minimumWidth();
		w += ui->panel_1_Data->minimumWidth();
		w += ui->splitter->handleWidth();

		ui->panel_2_Options->setMaximumWidth(w+8);
		ui->panel_1_Data->hide();

		sizes[0] = 0;
		sizes[1] = midPanelWidth;

		ui->splitter->setSizes(sizes);
	}
	else
	{
		ui->panel_2_Options->setMaximumWidth(ui->panel_2_Options->minimumWidth());

		sizes[0] = newTableWidth - 2;
		sizes[1] = ui->panel_2_Options->minimumWidth();

		ui->splitter->setSizes(sizes);
	}

	_buttonPanel->move(ui->panel_2_Options->width() - _buttonPanel->width() - _scrollbarWidth, 0);
}


void MainWindow::showDataPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	sizes[0] = ui->panel_1_Data->minimumWidth()+8;
	sizes[1] = ui->panel_2_Options->minimumWidth();

	ui->splitter->setSizes(sizes);

	ui->panel_2_Options->setMaximumWidth(ui->panel_2_Options->minimumWidth());
	ui->panel_1_Data->show();
}


void MainWindow::hideDataPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	int w = 0;
	w += ui->panel_2_Options->minimumWidth();
	w += ui->panel_1_Data->minimumWidth();
	w += ui->splitter->handleWidth();

	ui->panel_2_Options->setMaximumWidth(w+8);
	ui->panel_1_Data->hide();

	sizes[0] = 0;
	sizes[1] = w;

	ui->splitter->setSizes(sizes);
}


void MainWindow::analysisOKed()
{
	if (_currentOptionsWidget != NULL)
	{
		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		if (_log != NULL)
			_log->log("Analysis OKed", info);

		_currentOptionsWidget->hide();
		_currentOptionsWidget->unbind();
		_currentOptionsWidget = NULL;
	}

	_resultsJsInterface->unselect();

	hideOptionsPanel();
}


void MainWindow::analysisRunned()
{
	if (_currentAnalysis == NULL)
		return;

	if (_currentAnalysis->status() == Analysis::Running)
		_currentAnalysis->abort();
	else if (_currentAnalysis->status() == Analysis::InitedAndWaiting)
		_currentAnalysis->scheduleRun();
}


void MainWindow::removeAnalysis(Analysis *analysis)
{
	bool selected = false;
	analysis->abort();

	if (_currentOptionsWidget != NULL && analysis == _currentAnalysis)
	{
		selected = true;
		_currentOptionsWidget->hide();
		_currentOptionsWidget->unbind();
		_currentOptionsWidget = NULL;
	}

	analysis->setVisible(false);

	QString info("%1,%2");
	info = info.arg(tq(analysis->name()));
	info = info.arg(analysis->id());

	if (_package->isLoaded())
		_package->setModified(true);

	if (_log != NULL)
		_log->log("Analysis Removed", info);

	_resultsJsInterface->removeAnalysis(analysis);

	if (selected)
		hideOptionsPanel();
	checkUsedModules();
}


void MainWindow::removeAllAnalyses()
{
	QMessageBox::StandardButton reply;
	reply = QMessageBox::question(this, "Remove All Analyses", " Do you really want to remove all analyses ? ",
								  QMessageBox::Yes|QMessageBox::No,QMessageBox::Yes);
	if (reply == QMessageBox::Yes)
	{
		for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
		{
			Analysis *analysis = *itr;
			if (analysis == NULL) continue;
			removeAnalysis(analysis);
		}
	}
}


void MainWindow::refreshAllAnalyses()
{
	for (Analyses::iterator it = _analyses->begin(); it != _analyses->end(); ++it)
	{
		Analysis *analysis = *it;
		if (analysis == NULL) continue;
		analysis->refresh();
	}
}


void MainWindow::refreshAnalysesUsingColumn(QString col)
{
	std::vector<std::string> changedColumns, missingColumns;
	std::map<std::string, std::string> changeNameColumns;
	changedColumns.push_back(col.toStdString());
	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns);

	_package->setModified(false);
}

void MainWindow::removeAnalysisRequestHandler(int id)
{
	Analysis *analysis = _analyses->get(id);
	removeAnalysis(analysis);
}

void MainWindow::getAnalysesUserData()
{
	QVariant userData = _resultsJsInterface->getAllUserData();

	Json::Value data;
	Json::Reader parser;
	parser.parse(fq(userData.toString()), data);

	for (Json::Value::iterator iter = data.begin(); iter != data.end(); iter++)
	{
		Json::Value &userDataObj = *iter;

		Analysis *analysis = _analyses->get(userDataObj["id"].asInt());

		Json::Value &analysisUserData = userDataObj["userdata"];

		analysis->setUserData(analysisUserData);
	}
}

void MainWindow::setPackageModified()
{
	_package->setModified(true);
}



void MainWindow::analysisChangedDownstreamHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == NULL)
		return;

	QString info("%1,%2");
	info = info.arg(tq(analysis->name()));
	info = info.arg(id);

	if (_log != NULL)
		_log->log("Analysis Changed Downstream", info);

	string utf8 = fq(options);

	Json::Value root;

	Json::Reader parser;
	parser.parse(utf8, root);

	analysis->options()->set(root);
}

void MainWindow::startDataEditorHandler()
{
	QString path = QString::fromStdString(_package->dataFilePath);
	if (path.isEmpty() || path.startsWith("http") || !QFileInfo::exists(path) || Utils::getFileSize(path.toStdString()) == 0 || _package->dataFileReadOnly)
	{
		QString message = "JASP was started without associated data file (csv, sav or ods file). But to edit the data, JASP starts a spreadsheet editor based on this file and synchronize the data when the file is saved. Does this data file exist already, or do you want to generate it?";
		if (path.startsWith("http"))
			message = "JASP was started with an online data file (csv, sav or ods file). But to edit the data, JASP needs this file on your computer. Does this data file also exist on your computer, or do you want to generate it?";
		else if (_package->dataFileReadOnly)
			message = "JASP was started with a read-only data file (probably from the examples). But to edit the data, JASP needs to write to the data file. Does the same file also exist on your computer, or do you want to generate it?";

		QMessageBox msgBox(QMessageBox::Question, QString("Start Spreadsheet Editor"), message,
						   QMessageBox::Yes|QMessageBox::No|QMessageBox::Cancel);
		msgBox.setButtonText(QMessageBox::Yes, QString("Generate Data File"));
		msgBox.setButtonText(QMessageBox::No, QString("Find Data File"));
		int reply = msgBox.exec();
		if (reply == QMessageBox::Cancel)
			return;

		FileEvent *event = NULL;
		if (reply == QMessageBox::Yes)
		{
			QString caption = "Generate Data File as CSV";
			QString filter = "CSV Files (*.csv)";
			QString name = windowTitle();
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

			path = QFileDialog::getSaveFileName(this, caption, name, filter);
			if (path == "")
				return;

			if (!path.endsWith(".csv", Qt::CaseInsensitive))
				path.append(".csv");

			event = new FileEvent(this, FileEvent::FileExportData);
		}
		else
		{
			QString caption = "Find Data File";
			QString filter = "Data File (*.csv *.txt *.sav *.ods)";

			path = QFileDialog::getOpenFileName(this, caption, "", filter);
			if (path == "")
				return;

			event = new FileEvent(this, FileEvent::FileSyncData);
		}

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(startDataEditorEventCompleted(FileEvent*)));
		connect(event, SIGNAL(completed(FileEvent*)), ui->backStage, SLOT(setSyncFile(FileEvent*)));
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
		_package->dataFilePath = event->path().toStdString();
		_package->dataFileReadOnly = false;
		_package->setModified(true);
		startDataEditor(event->path());
	}
}


void MainWindow::startDataEditor(QString path)
{
	QFileInfo fileInfo(path);

	int useDefaultSpreadsheetEditor = _settings.value("useDefaultSpreadsheetEditor", 1).toInt();
	QString appname = _settings.value("spreadsheetEditorName", "").toString();

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
		{
			QMessageBox::warning(this,QString("Start Editor"), QString("Unable to start the editor : ") + appname + QString(". Please check your editor settings in the preference menu."), QMessageBox::Ok);
		}
	}
	else
	{
		if (!QDesktopServices::openUrl(QUrl("file:///" + path, QUrl::TolerantMode)))
		{
			QMessageBox::warning(this, QString("Start Spreadsheet Editor"), QString("No default spreadsheet editor for file ") + fileInfo.completeBaseName() + QString(". Use Preferences to set the right editor."), QMessageBox::Cancel);
		}
	}
}

void MainWindow::showProgress()
{
	ui->panel_1_Data->show();
	QMetaObject::invokeMethod(qmlProgressBar, "show");
}

void MainWindow::hideProgress()
{
	QMetaObject::invokeMethod(qmlProgressBar, "hide");
}

void MainWindow::setProgressStatus(QString status, int progress)
{
	QMetaObject::invokeMethod(qmlProgressBar, "setStatus", Q_ARG(QVariant, QVariant(status)), Q_ARG(QVariant, QVariant(progress)));
}

void MainWindow::setFilterErrorText(QString error)
{
	ui->quickWidget_Data->rootContext()->setContextProperty("filterErrorText", error);
}

void MainWindow::applyAndSendFilter(QString filter)
{
	QMetaObject::invokeMethod(qmlFilterWindow, "applyAndSendFilter", Q_ARG(QVariant, QVariant(filter)));
	if(filter.length() > 0 && filter != "*" && filter != DEFAULT_FILTER)
		QMetaObject::invokeMethod(qmlFilterWindow, "open");

}

void MainWindow::setStatusBarText(QString text)
{
	QMetaObject::invokeMethod(qmlStatusBar, "setText", Q_ARG(QVariant, QVariant(text)));
}

void MainWindow::onFilterUpdated()
{
	int TotalCount = _package->dataSet->rowCount(), TotalThroughFilter = _package->dataSet->filteredRowCount();
	double PercentageThrough = 100.0 * ((double)TotalThroughFilter) / ((double)TotalCount);

	std::stringstream ss;
	if(TotalCount > TotalThroughFilter)
		ss << "Data has " << TotalCount << " rows, " << TotalThroughFilter << " (~" << (int)round(PercentageThrough) << "%)  passed through filter to be used  in analyses";
	else if(_package->dataFilter != "" && _package->dataFilter != "*")
		ss.str("Filter passes everything");
	else
		ss.str("");

	setStatusBarText(QString::fromStdString(ss.str()));
	
	if(_package->refreshAnalysesAfterFilter) //After loading a JASP package we do not want to rerun all analyses because it might take very long
		refreshAllAnalyses(); 
	_package->refreshAnalysesAfterFilter = true;
}

void MainWindow::setGeneratedFilter(QString generatedFilter)
{
	ui->quickWidget_Data->rootContext()->setContextProperty("generatedFilter", generatedFilter);
}

void MainWindow::setGeneratedFilterAndSend(QString generatedFilter)
{
	setGeneratedFilter(generatedFilter);

	if(_package->refreshAnalysesAfterFilter)
		QMetaObject::invokeMethod(qmlFilterWindow, "sendFilter");
}

void MainWindow::setFilterConstructorJSON(QString jsonString)
{
	_package->filterConstructorJSON = jsonString.toStdString();

	ui->quickWidget_Data->rootContext()->setContextProperty("filterConstructorJSONstring", jsonString);
}
