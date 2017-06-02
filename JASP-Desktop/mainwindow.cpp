//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "analysisforms/descriptivesform.h"

#include "analysisforms/ttestbayesianindependentsamplesform.h"
#include "analysisforms/ttestbayesianpairedsamplesform.h"
#include "analysisforms/ttestbayesianonesampleform.h"
#include "analysisforms/ttestindependentsamplesform.h"
#include "analysisforms/ttestpairedsamplesform.h"
#include "analysisforms/ttestonesampleform.h"


#include "analysisforms/anovaonewayform.h"
#include "analysisforms/anovaform.h"
#include "analysisforms/anovarepeatedmeasuresform.h"
#include "analysisforms/ancovaform.h"
#include "analysisforms/anovamultivariateform.h"
#include "analysisforms/ancovamultivariateform.h"

#include "analysisforms/anovabayesianform.h"
#include "analysisforms/ancovabayesianform.h"
#include "analysisforms/anovarepeatedmeasuresbayesianform.h"

#include "analysisforms/regressionlinearform.h"
#include "analysisforms/regressionlinearbayesianform.h"
#include "analysisforms/regressionloglinearform.h"
#include "analysisforms/regressionloglinearbayesianform.h"
#include "analysisforms/correlationform.h"
#include "analysisforms/correlationbayesianform.h"
#include "analysisforms/correlationbayesianpairsform.h"
#include "analysisforms/correlationpartialform.h"
#include "analysisforms/contingencytablesform.h"
#include "analysisforms/contingencytablesbayesianform.h"

#include "analysisforms/binomialtestform.h"
#include "analysisforms/binomialtestbayesianform.h"
#include "analysisforms/bffromtform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianindependentsamplesform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianpairedsamplesform.h"
#include "analysisforms/SummaryStatistics/summarystatsttestbayesianonesampleform.h"
#include "analysisforms/SummaryStatistics/summarystatsbinomialtestbayesianform.h"
#include "analysisforms/SummaryStatistics/summarystatsregressionlinearbayesianform.h"
#include "analysisforms/SummaryStatistics/summarystatscorrelationbayesianpairsform.h"

#ifdef QT_DEBUG
#include "analysisforms/basregressionlinearlinkform.h"
#endif

#include "analysisforms/SEM/semsimpleform.h"
#include "analysisforms/R11tLearn/r11tlearnform.h"

#include "analysisforms/MachineLearning/mlregressionrandomforestform.h"
#include "analysisforms/MachineLearning/mlregressionboostingform.h"
#include "analysisforms/MachineLearning/mlregressionknnform.h"
#include "analysisforms/MachineLearning/mlclassificationknnform.h"
#include "analysisforms/MachineLearning/mlclusteringkmeansform.h"

#include "analysisforms/reliabilityanalysisform.h"
#include "analysisforms/exploratoryfactoranalysisform.h"
#include "analysisforms/principalcomponentanalysisform.h"

#include <QDebug>
#include <QWebFrame>
#include <QFile>
#include <QFileInfo>
#include <QToolTip>
#include <QClipboard>
#include <QWebElement>
#include <QMessageBox>
#include <QTimer>
#include <QStringBuilder>
#include <QWebHistory>
#include <QDropEvent>
#include <QShortcut>
#include <QDesktopWidget>
#include <QTabBar>
#include <QMenuBar>
#include <QDir>
#include <QFileDialog>
#include <QDesktopServices>

#include "analysisloader.h"

#include "qutils.h"
#include "appdirs.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "appinfo.h"


#include "lrnam.h"
#include "activitylog.h"
#include "aboutdialog.h"
#include "preferencesdialog.h"
#include <boost/filesystem.hpp>
#include "dirs.h"

#include "options/optionvariablesgroups.h"

using namespace std;

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	_log = NULL;
	_tableModel = NULL;
	_currentOptionsWidget = NULL;
	_currentAnalysis = NULL;

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

	ui->setupUi(this);

	int initalTableWidth = 575;

	QList<int> sizes = QList<int>();
	sizes.append(initalTableWidth);
	ui->splitter->setSizes(sizes);

	ui->tabBar->setFocusPolicy(Qt::NoFocus);
	ui->tabBar->addTab("File");
	ui->tabBar->addTab("Common");

	ui->tabBar->addHelpTab();

	connect(ui->tabBar, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));
	connect(ui->tabBar, SIGNAL(helpToggled(bool)), this, SLOT(helpToggled(bool)));

	ui->ribbonAnalysis->setDataSetLoaded(false);
	ui->ribbonSEM->setDataSetLoaded(false);
	ui->ribbonR11tLearn->setDataSetLoaded(false);
	ui->ribbonSummaryStatistics->setDataSetLoaded(false);
	ui->ribbonMachineLearning->setDataSetLoaded(false);

#ifdef QT_DEBUG
	ui->webViewResults->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
	ui->webViewHelp->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
#else
	ui->webViewResults->setContextMenuPolicy(Qt::NoContextMenu);
	ui->webViewHelp->setContextMenuPolicy(Qt::NoContextMenu);
#endif

	tempfiles_init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	_odm = new OnlineDataManager(this);
	_odm->initAuthentication(OnlineDataManager::OSF);

	_loader.setOnlineDataManager(_odm);
	ui->backStage->setOnlineDataManager(_odm);

	// the LRNAM adds mime types to local resources; important for SVGs
	ui->webViewResults->page()->setNetworkAccessManager(new LRNAM(tq(tempfiles_sessionDirName()), this));
	ui->webViewResults->setUrl(QUrl(QString("qrc:///core/index.html")));
	connect(ui->webViewResults, SIGNAL(loadFinished(bool)), this, SLOT(resultsPageLoaded(bool)));
	connect(ui->webViewResults, SIGNAL(scrollValueChanged()), this, SLOT(scrollValueChangedHandle()));

	_tableModel = new DataSetTableModel();
	ui->tableView->setModel(_tableModel);
	ui->tableView->setVariablesView(ui->variablesPage);
	ui->variablesPage->hide();

	ui->tabBar->setCurrentIndex(1);

	ui->tableView->setVerticalScrollMode(QTableView::ScrollPerPixel);
	ui->tableView->setHorizontalScrollMode(QTableView::ScrollPerPixel);

	_analyses = new Analyses();
	_engineSync = new EngineSync(_analyses, this);
	connect(_engineSync, SIGNAL(engineTerminated()), this, SLOT(fatalError()));

	connect(_analyses, SIGNAL(analysisResultsChanged(Analysis*)), this, SLOT(analysisResultsChangedHandler(Analysis*)));
	connect(_analyses, SIGNAL(analysisImageSaved(Analysis*)), this, SLOT(analysisImageSavedHandler(Analysis*)));
	connect(_analyses, SIGNAL(analysisUserDataLoaded(Analysis*)), this, SLOT(analysisUserDataLoadedHandler(Analysis*)));
	connect(_analyses, SIGNAL(analysisAdded(Analysis*)), ui->backStage, SLOT(analysisAdded(Analysis*)));

	connect(ui->ribbonAnalysis, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonSEM, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonR11tLearn, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonSummaryStatistics, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonMachineLearning, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->backStage, SIGNAL(dataSetIORequest(FileEvent*)), this, SLOT(dataSetIORequest(FileEvent*)));
	connect(ui->backStage, SIGNAL(exportSelected(QString)), this, SLOT(exportSelected(QString)));
	connect(ui->variablesPage, SIGNAL(columnChanged(QString)), this, SLOT(refreshAnalysesUsingColumn(QString)));
	connect(ui->variablesPage, SIGNAL(resetTableView()), this, SLOT(resetTableView()));
	connect(ui->tableView, SIGNAL(dataTableColumnSelected()), this, SLOT(showVariablesPage()));
	connect(ui->tableView, SIGNAL(dataTableDoubleClicked()), this, SLOT(startDataEditorHandler()));
	connect(ui->tabBar, SIGNAL(dataAutoSynchronizationChanged(bool)), ui->backStage, SLOT(dataAutoSynchronizationChanged(bool)));

	_progressIndicator = new ProgressWidget(ui->tableView);
	_progressIndicator->setAutoFillBackground(true);
	_progressIndicator->resize(400, 100);
	_progressIndicator->move(100, 80);
	_progressIndicator->hide();

	connect(&_loader, SIGNAL(progress(QString,int)), _progressIndicator, SLOT(setStatus(QString,int)));

	connect(this, SIGNAL(analysisSelected(int)), this, SLOT(analysisSelectedHandler(int)));
	connect(this, SIGNAL(analysisUnselected()), this, SLOT(analysisUnselectedHandler()));
	connect(this, SIGNAL(saveTempImage(int, QString, QByteArray)), this, SLOT(saveTempImageHandler(int, QString, QByteArray)));
	connect(this, SIGNAL(displayMessageFromResults(QString)),  this, SLOT(displayMessageFromResultsHandler(QString)));
	connect(this, SIGNAL(pushToClipboard(QString, QString, QString)), this, SLOT(pushToClipboardHandler(QString, QString, QString)));
	connect(this, SIGNAL(pushImageToClipboard(QByteArray, QString)), this, SLOT(pushImageToClipboardHandler(QByteArray, QString)));
	connect(this, SIGNAL(saveTextToFile(QString, QString)), this, SLOT(saveTextToFileHandler(QString, QString)));
	connect(this, SIGNAL(analysisChangedDownstream(int, QString)), this, SLOT(analysisChangedDownstreamHandler(int, QString)));
	connect(this, SIGNAL(analysisSaveImage(int, QString)), this, SLOT(analysisSaveImageHandler(int, QString)));
	connect(this, SIGNAL(showAnalysesMenu(QString)), this, SLOT(showAnalysesMenuHandler(QString)));
	connect(this, SIGNAL(removeAnalysisRequest(int)), this, SLOT(removeAnalysisRequestHandler(int)));
	connect(this, SIGNAL(updateUserData(int, QString)), this, SLOT(updateUserDataHandler(int, QString)));
	connect(this, SIGNAL(simulatedMouseClick(int, int, int)), this, SLOT(simulatedMouseClickHandler(int, int, int)));
	connect(this, SIGNAL(resultsDocumentChanged()), this, SLOT(resultsDocumentChangedHandler()));

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

	connect(_okButton, SIGNAL(clicked()), this, SLOT(analysisOKed()));
	connect(_runButton, SIGNAL(clicked()), this, SLOT(analysisRunned()));

	connect(ui->splitter, SIGNAL(splitterMoved(int,int)), this, SLOT(splitterMovedHandler(int,int)));

	_analysisMenu = new QMenu(this);
	connect(_analysisMenu, SIGNAL(aboutToHide()), this, SLOT(menuHidding()));

	updateUIFromOptions();

	_tableViewWidthBeforeOptionsMadeVisible = -1;

	QUrl userGuide = QUrl::fromLocalFile(AppDirs::help() + "/index.html");
	ui->webViewHelp->setUrl(userGuide);
	connect(ui->webViewHelp, SIGNAL(loadFinished(bool)), this, SLOT(helpFirstLoaded(bool)));
	ui->panel_4_Help->hide();

	setAcceptDrops(true);

#ifdef __WIN32__
	QApplication::setFont(ui->tableView->font());
#endif

	ui->panel_1_Data->show();
	ui->panel_2_Options->hide();

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
}

void MainWindow::saveKeysSelected()
{
	if (_package->isModified())
	{
		ui->backStage->save();
	}
}

void MainWindow::openKeysSelected()
{

}

void MainWindow::refreshKeysSelected()
{
	refreshAllAnalyses();
}

void MainWindow::syncKeysSelected()
{
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

void MainWindow::refreshAnalysesUsingColumns(vector<string> &changedColumns
											, vector<string> &missingColumns
											, map<string, string> &changeNameColumns)
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

void MainWindow::packageDataChanged(DataSetPackage *package
									, std::vector<std::string> &changedColumns
									, std::vector<std::string> &missingColumns
									, std::map<std::string, std::string> &changeNameColumns)
{
	_tableModel->setDataSet(_package->dataSet);
	ui->variablesPage->setDataSet(_package->dataSet);

	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns);
}


QString MainWindow::escapeJavascriptString(const QString &str)
{
	QString out;
	QRegExp rx("(\\r|\\n|\\\\|\"|\')");
	int pos = 0, lastPos = 0;

	while ((pos = rx.indexIn(str, pos)) != -1)
	{
		out += str.mid(lastPos, pos - lastPos);

		switch (rx.cap(1).at(0).unicode())
		{
		case '\r':
			out += "\\r";
			break;
		case '\n':
			out += "\\n";
			break;
		case '"':
			out += "\\\"";
			break;
		case '\'':
			out += "\\'";
			break;
		case '\\':
			out += "\\\\";
			break;
		}
		pos++;
		lastPos = pos;
	}
	out += str.mid(lastPos);
	return out;
}

void MainWindow::analysisUserDataLoadedHandler(Analysis *analysis)
{
	QString results = tq(analysis->userData().toStyledString());

	results = escapeJavascriptString(results);
	results = "window.loadUserData(" + QString::number(analysis->id()) + ", JSON.parse('" + results + "'));";
}

void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	static bool showInstructions = true;

	if (showInstructions)
	{
		if (_settings.value("instructionsShown", false).toBool() == false)
		{
			_settings.setValue("instructionsShown", true);
			ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.showInstructions()");
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

	Json::Value analysisJson = analysis->asJSON();
	analysisJson["userdata"] = analysis->userData();
	QString results = tq(analysisJson.toStyledString());

	results = escapeJavascriptString(results);
	results = "window.analysisChanged(JSON.parse('" + results + "'));";

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(results);

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
	QString filter = "Portable Network Graphics (*.png);;Encapsulated PostScript (*.eps)";
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

AnalysisForm* MainWindow::loadForm(Analysis *analysis)
{
	return loadForm(analysis->name());
}

AnalysisForm* MainWindow::loadForm(const string name)
{
	if (_analysisForms.find(name) != _analysisForms.end())
		return _analysisForms[name];

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
	else if (name == "R11tLearn")
		form = new R11tLearnForm(contentArea);
	else if (name == "BinomialTest")
		form = new BinomialTestForm(contentArea);
	else if (name == "BinomialTestBayesian")
		form = new BinomialTestBayesianForm(contentArea);
	else if (name == "BFFromT")
		form = new BFFromTForm(contentArea);
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
#ifdef QT_DEBUG
	else if (name == "BASRegressionLinearLink")
		form = new BASRegressionLinearLinkForm(contentArea);

	else if (name == "MLRegressionRandomForest")
		form = new MLRegressionRandomForestForm(contentArea);
	else if (name == "MLRegressionBoosting")
		form = new MLRegressionBoostingForm(contentArea);
	else if (name == "MLRegressionKNN")
		form = new MLRegressionKNNForm(contentArea);
		else if (name == "MLClassificationKNN")
			form = new MLClassificationKNNForm(contentArea);
	else if (name == "MLClusteringKMeans")
		form = new MLClusteringKMeansForm(contentArea);
#endif
	else
		qDebug() << "MainWindow::loadForm(); form not found : " << name.c_str();

	if (form != NULL)
		_analysisForms[name] = form;

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

		//#########################

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
		if(currentActiveTab == "Common")
		{
			ui->ribbon->setCurrentIndex(0);
		}
		else if(currentActiveTab == "SEM")
		{
			ui->ribbon->setCurrentIndex(1);
		}
		else if(currentActiveTab == "R11t Learn")
		{
			ui->ribbon->setCurrentIndex(2);
		}
		else if(currentActiveTab == "Summary Stats")
		{
			ui->ribbon->setCurrentIndex(3);
		}
		else if(currentActiveTab == "Machine Learning")
		{
			ui->ribbon->setCurrentIndex(4);
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
			_progressIndicator->show();
		}

		ui->tabBar->setCurrentIndex(1);

	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (_analyses->count() > 0)
		{
			_package->setWaitingForReady();

			getAnalysesUserData();
			ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.exportHTML('%PREVIEW%');");

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

			analysesData["meta"] = getResultsMeta();

			_package->analysesData = analysesData;
			_package->hasAnalyses = true;
		}

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

		_loader.io(event, _package);
		_progressIndicator->show();
	}
	else if (event->operation() == FileEvent::FileExportResults)
	{
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));

		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.exportHTML('%EXPORT%');");

		_loader.io(event, _package);
		_progressIndicator->show();

	}
	else if (event->operation() == FileEvent::FileExportData)
	{
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
		_loader.io(event, _package);
		_progressIndicator->show();
	}
	else if (event->operation() == FileEvent::FileSyncData)
	{
		if (_package->dataSet == NULL)
			return;

		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
		_loader.io(event, _package);
		_progressIndicator->show();
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		if (_package->isModified())
		{
			QString title = windowTitle();
			title.chop(1);
			QMessageBox::StandardButton reply = QMessageBox::warning(this, "Save Workspace?", QString("Save changes to workspace \"") + title +  QString("\" before closing?\n\nYour changes will be lost if you don't save them."), QMessageBox::Save|QMessageBox::Discard|QMessageBox::Cancel);

			if (reply == QMessageBox::Save)
			{
				FileEvent *saveEvent = ui->backStage->save();
				event->chain(saveEvent);
				connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(dataSetIOCompleted(FileEvent*)));
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
			}
		}
		else
		{
			event->setComplete();
			dataSetIOCompleted(event);
		}

		ui->variablesPage->close();
	}
}

void MainWindow::dataSetIOCompleted(FileEvent *event)
{
	this->analysisOKed();
	bool showAnalysis = false;
	_progressIndicator->hide();

	if (event->operation() == FileEvent::FileOpen)
	{
		if (event->successful())
		{
			populateUIfromDataSet();
			QString name =  QFileInfo(event->path()).baseName();
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

			QMessageBox::warning(this, "", "Unable to open file.\n\n" + event->message());

			if (_openedUsingArgs)
				close();
		}
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		if (event->successful())
		{
			QString name =  QFileInfo(event->path()).baseName();

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
			closeCurrentOptionsWidget();
			hideOptionsPanel();
			_tableModel->clearDataSet();
			ui->variablesPage->clearDataSet();
			_loader.free(_package->dataSet);
			_package->reset();
			updateMenuEnabledDisabledStatus();
			ui->webViewResults->reload();
			setWindowTitle("JASP");
			ui->tableView->adjustAfterDataLoad(false);

			if (_applicationExiting)
				QApplication::exit();
		}
		else
		{
			_applicationExiting = false;
		}
	}

	if (showAnalysis)
	{
		ui->tabBar->setCurrentIndex(1);

	}
}

void MainWindow::populateUIfromDataSet()
{
	_tableModel->setDataSet(_package->dataSet);
	ui->variablesPage->setDataSet(_package->dataSet);


	_analyses->clear();

	ui->tableView->adjustAfterDataLoad(true);

	_progressIndicator->hide();

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
					results = escapeJavascriptString(results);
					results = "window.setResultsMeta(JSON.parse('" + results + "'));";
					ui->webViewResults->page()->mainFrame()->evaluateJavaScript(results);
				}
			}

			for (Json::ValueIterator iter = analysesDataList.begin(); iter != analysesDataList.end(); iter++)
			{
				try
				{
					QString name = QString();
					Json::Value &analysisData = *iter;

					name = QString::fromStdString(analysisData["name"].asString());
					int id = analysisData["id"].asInt();

					Json::Value &optionsJson = analysisData["options"];
					Json::Value &resultsJson = analysisData["results"];
					Json::Value &userDataJson = analysisData["userdata"];

					Analysis::Status status = Analysis::parseStatus(analysisData["status"].asString());

					Analysis *analysis = _analyses->create(name, id, &optionsJson, status);

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
}

void MainWindow::updateMenuEnabledDisabledStatus()
{
	bool loaded = _package->isLoaded();

	ui->ribbonAnalysis->setDataSetLoaded(loaded);
	ui->ribbonSEM->setDataSetLoaded(loaded);
	ui->ribbonR11tLearn->setDataSetLoaded(loaded);
	ui->ribbonMachineLearning->setDataSetLoaded(loaded);
}

void MainWindow::updateUIFromOptions()
{
#ifdef __linux__
	ui->tabBar->removeTab("SEM");
#else
	QVariant sem = _settings.value("plugins/sem", false);
	if (sem.canConvert(QVariant::Bool) && sem.toBool())
		ui->tabBar->addTab("SEM");
	else
		ui->tabBar->removeTab("SEM");
#endif

	QVariant rl = _settings.value("toolboxes/r11tLearn", false);
	if (rl.canConvert(QVariant::Bool) && rl.toBool())
		ui->tabBar->addTab("R11t Learn");
	else
		ui->tabBar->removeTab("R11t Learn");

	QVariant sumStats = _settings.value("toolboxes/summaryStatistics", false);
	if (sumStats.canConvert(QVariant::Bool) && sumStats.toBool())
		ui->tabBar->addTab("Summary Stats");
	else
		ui->tabBar->removeTab("Summary Stats");

	QVariant machineLearning = _settings.value("toolboxes/machineLearning", false);
	if (machineLearning.canConvert(QVariant::Bool) && machineLearning.toBool())
		ui->tabBar->addTab("Machine Learning");
	else
	ui->tabBar->removeTab("Machine Learning");
}

void MainWindow::resultsPageLoaded(bool success)
{
	// clear history, to prevent backspace from going 'back'
	ui->webViewResults->history()->clear();
	ui->webViewResults->page()->mainFrame()->addToJavaScriptWindowObject("jasp", this);

	if (success)
	{
		QString version = tq(AppInfo::version.asString());
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.setAppVersion('" + version + "')");

		QVariant ppiv = ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.getPPI()");

		bool success;
		int ppi = ppiv.toInt(&success);
		if (success == false)
			ppi = 96;

		_webViewZoom = 1;
#ifdef __WIN32__
		const int verticalDpi = QApplication::desktop()->screen()->logicalDpiY();
		qreal zoom = ((qreal)(verticalDpi) / (qreal)ppi);
		ui->webViewResults->setZoomFactor(zoom);
		ui->webViewHelp->setZoomFactor(zoom);
		ppi = verticalDpi;
		_webViewZoom = zoom;

		this->resize(this->width() + (ui->webViewResults->width() * (zoom - 1)), this->height() + (ui->webViewResults->height() * (zoom - 1)));
#endif

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

	ui->webViewHelp->page()->mainFrame()->evaluateJavaScript(js);
}

void MainWindow::itemSelected(const QString &item)
{
	try
	{
		_currentAnalysis = _analyses->create(item);

		showForm(_currentAnalysis);
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.select(" % QString::number(_currentAnalysis->id()) % ")");

		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

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

void MainWindow::exportSelected(const QString &filename)
{
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.exportHTML('" + filename + "');");
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
	ui->panel_1_Data->show();
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

void MainWindow::showVariablesPage()
{
	QList<int> datacurrentSizes = ui->data_splitter->sizes();

	ui->variablesPage->show();

	if (datacurrentSizes[0] < 1)
	{
		datacurrentSizes[0]+=250;
		datacurrentSizes[1]-=250;
		ui->data_splitter->setSizes(datacurrentSizes);
	}
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

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.unselect()");

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

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.remove(" % QString::number(analysis->id()) % ")");

	if (selected)
		hideOptionsPanel();
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

void MainWindow::resetTableView()
{
	ui->tableView->reset();
}

void MainWindow::pushToClipboardHandler(const QString &mimeType, const QString &data, const QString &html)
{
	if (_log != NULL)
		_log->log("Copy");

	QMimeData *mimeData = new QMimeData();

	if (mimeType == "text/plain")
		mimeData->setText(data);

	if ( ! html.isEmpty())
		mimeData->setHtml(html);

	QClipboard *clipboard = QApplication::clipboard();
	clipboard->setMimeData(mimeData, QClipboard::Clipboard);

	//qDebug() << clipboard->mimeData(QClipboard::Clipboard)->data("text/html");
}

void MainWindow::pushImageToClipboardHandler(const QByteArray &base64, const QString &html)
{
	if (_log != NULL)
		_log->log("Copy");

	QMimeData *mimeData = new QMimeData();

	QByteArray byteArray = QByteArray::fromBase64(base64);

	QImage pm;
	if(pm.loadFromData(byteArray))
	{
#ifdef __WIN32__ //needed because jpegs/clipboard doesn't support transparency in windows
		QImage image2(pm.size(), QImage::Format_ARGB32);
		image2.fill(Qt::white);
		QPainter painter(&image2);
		painter.drawImage(0, 0, pm);

		mimeData->setImageData(image2);
#else
		mimeData->setImageData(pm);
#endif

	}

	if ( ! html.isEmpty())
		mimeData->setHtml(html);

	if (mimeData->hasImage() || mimeData->hasHtml())
	{
		QClipboard *clipboard = QApplication::clipboard();
		clipboard->setMimeData(mimeData, QClipboard::Clipboard);
	}

	//qDebug() << clipboard->mimeData(QClipboard::Clipboard)->data("text/html");
}

void MainWindow::saveTempImageHandler(int id, QString path, QByteArray data)
{
	QByteArray byteArray = QByteArray::fromBase64(data);

	QString fullpath = tq(tempfiles_createSpecific_clipboard(fq(path)));

	QFile file(fullpath);
	file.open(QIODevice::WriteOnly);
	file.write(byteArray);
	file.close();

	QString eval = QString("window.imageSaved({ id: %1, fullPath: '%2'});").arg(id).arg(fullpath);
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(eval);
}

void MainWindow::displayMessageFromResultsHandler(QString msg)
{
	QMessageBox::warning(this, "Results Warning", msg);
}

void MainWindow::showAnalysesMenuHandler(QString options)
{
	Json::Value menuOptions;

	Json::Reader parser;
	parser.parse(fq(options), menuOptions);

	QIcon _copyIcon = QIcon(":/icons/copy.png");
	QIcon _citeIcon = QIcon(":/icons/cite.png");
	QIcon _collapseIcon = QIcon(":/icons/collapse.png");
	QIcon _expandIcon = QIcon(":/icons/expand.png");
	QIcon _saveImageIcon = QIcon(":/icons/document-save-as.png");

	_analysisMenu->clear();

	QString objName = tq(menuOptions["objectName"].asString());

	if (menuOptions["hasCollapse"].asBool())
	{
		Json::Value collapseOptions = menuOptions["collapseOptions"];
		QIcon icon = collapseOptions["collapsed"].asBool() ? _expandIcon : _collapseIcon;
		_analysisMenu->addAction(icon, tq(collapseOptions["menuText"].asString()), this, SLOT(collapseSelected()));
		_analysisMenu->addSeparator();
	}

	if (menuOptions["hasEditTitle"].asBool())
	{
		_analysisMenu->addAction("Edit Title", this, SLOT(editTitleSelected()));
		_analysisMenu->addSeparator();
	}

	if (menuOptions["hasCopy"].asBool())
		_analysisMenu->addAction(_copyIcon, "Copy", this, SLOT(copySelected()));

	if (menuOptions["hasCite"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction(_citeIcon, "Copy Citations", this, SLOT(citeSelected()));
	}

	if (menuOptions["hasSaveImg"].asBool())
	{
		_analysisMenu->addAction(_saveImageIcon, "Save Image As", this, SLOT(saveImage()));
	}

	if (menuOptions["hasNotes"].asBool())
	{
		_analysisMenu->addSeparator();

		Json::Value noteOptions = menuOptions["noteOptions"];

		for (Json::ValueIterator iter = noteOptions.begin(); iter != noteOptions.end(); iter++)
		{
			Json::Value noteOption = *iter;
			QAction *a1 = _analysisMenu->addAction(tq(noteOption["menuText"].asString()), this, SLOT(noteSelected()));

			a1->setDisabled(noteOption["visible"].asBool());


			QString call = QString("window.notesMenuClicked('%1', %2);").arg(tq(noteOption["key"].asString())).arg(noteOption["visible"].asBool() ? "false" : "true");

			a1->setData(call);
		}
	}


	if (menuOptions["hasRemove"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Remove " + objName, this, SLOT(removeSelected()));
	}

	if (menuOptions["hasRemoveAllAnalyses"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Remove All ", this, SLOT(removeAllAnalyses()));
	}

	if (menuOptions["hasRefreshAllAnalyses"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Refresh All ", this, SLOT(refreshAllAnalyses()));
	}

	QPoint point = ui->webViewResults->mapToGlobal(QPoint(round(menuOptions["rX"].asInt() * _webViewZoom), round(menuOptions["rY"].asInt() * _webViewZoom)));

	_analysisMenu->move(point);
	_analysisMenu->show();
}

void MainWindow::removeAnalysisRequestHandler(int id)
{
	Analysis *analysis = _analyses->get(id);
	removeAnalysis(analysis);
}

Json::Value MainWindow::getResultsMeta()
{
	QVariant metaData = ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.getResultsMeta();");

	Json::Value meta;
	Json::Reader parser;
	parser.parse(fq(metaData.toString()), meta);

	return meta;
}

void MainWindow::getAnalysesUserData()
{
	QVariant userData = ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.getAllUserData();");

	Json::Value data;
	Json::Reader parser;
	parser.parse(fq(userData.toString()), data);

	for (Json::Value::iterator iter = data.begin(); iter != data.end(); iter++)
	{
		Json::Value &userDataObj = *iter;

		Analysis *analysis = _analyses->get(userDataObj["id"].asInt());

		Json::Value &analysisUserData = userDataObj["userdata"];

		analysis->setUserData(analysisUserData, true);
	}
}

void MainWindow::resultsDocumentChangedHandler()
{
	_package->setModified(true);
}

void MainWindow::simulatedMouseClickHandler(int x, int y, int count) {

	int diff = count;
	while (diff >= 2)
	{
		QMouseEvent * clickEvent = new QMouseEvent ((QEvent::MouseButtonDblClick), QPoint(x * _webViewZoom, y * _webViewZoom),
			Qt::LeftButton,
			Qt::LeftButton,
			Qt::NoModifier   );

		qApp->postEvent((QObject*)ui->webViewResults,(QEvent *)clickEvent);

		diff -= 2;
	}

	if (diff != 0)
	{
		QMouseEvent * clickEvent1 = new QMouseEvent ((QEvent::MouseButtonPress), QPoint(x * _webViewZoom, y * _webViewZoom),
			Qt::LeftButton,
			Qt::LeftButton,
			Qt::NoModifier   );

		qApp->postEvent((QObject*)ui->webViewResults,(QEvent *)clickEvent1);

		QMouseEvent * clickEvent2 = new QMouseEvent ((QEvent::MouseButtonRelease), QPoint(x * _webViewZoom, y * _webViewZoom),
			Qt::LeftButton,
			Qt::LeftButton,
			Qt::NoModifier   );

		qApp->postEvent((QObject*)ui->webViewResults,(QEvent *)clickEvent2);
	}
}

void MainWindow::updateUserDataHandler(int id, QString key)
{
	_package->setModified(true);
}


void MainWindow::collapseSelected()
{
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.collapseMenuClicked();");
}

void MainWindow::removeSelected()
{
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.removeMenuClicked();");
}

void MainWindow::editTitleSelected()
{
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.editTitleMenuClicked();");
	_package->setModified(true);
}

void MainWindow::copySelected()
{
	tempfiles_purgeClipboard();
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.copyMenuClicked();");
}

void MainWindow::citeSelected()
{
	tempfiles_purgeClipboard();
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.citeMenuClicked();");
}

void MainWindow::saveImage()
{
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.saveImageClicked();");
}

void MainWindow::noteSelected()
{
	QAction *action = (QAction *)this->sender();
	QString call = action->data().toString();

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(call);

	_package->setModified(true);
}

void MainWindow::menuHidding()
{
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.analysisMenuHidden();");
}

void MainWindow::scrollValueChangedHandle()
{
	if ( ! _analysisMenu->isHidden())
		_analysisMenu->hide();
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
		_progressIndicator->show();
	}
	else
		startDataEditor(path);

}

void MainWindow::startDataEditorEventCompleted(FileEvent* event)
{
	_progressIndicator->hide();

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
		startProcess = appname.mid(appname.lastIndexOf('/') + 1);
		startProcess = "open -a \"" + startProcess + "\" \"" + path + "\"";
#else
		startProcess = "\"" + appname + "\" \"" + path + "\"";
#endif
		QProcess::startDetached(startProcess);
	}
	else
	{
		if (!QDesktopServices::openUrl(QUrl("file:///" + path, QUrl::TolerantMode)))
		{
			QMessageBox::warning(this, QString("Start Spreadsheet Editor"), QString("No default spreadsheet editor for file ") + fileInfo.completeBaseName() + QString(". Use Preferences to set the right editor."), QMessageBox::Cancel);
		}
	}
}
