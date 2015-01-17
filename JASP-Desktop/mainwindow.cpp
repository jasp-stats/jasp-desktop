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
#include "analysisforms/anovarepeatedmeasuresshortform.h"
#include "analysisforms/ancovaform.h"
#include "analysisforms/anovamultivariateform.h"
#include "analysisforms/ancovamultivariateform.h"

#include "analysisforms/anovabayesianform.h"
#include "analysisforms/ancovabayesianform.h"
#include "analysisforms/anovarepeatedmeasuresbayesianform.h"

#include "analysisforms/regressionlinearform.h"
#include "analysisforms/regressionlinearbayesianform.h"
#include "analysisforms/correlationform.h"
#include "analysisforms/correlationbayesianform.h"
#include "analysisforms/correlationpartialform.h"
#include "analysisforms/crosstabsform.h"
#include "analysisforms/crosstabsbayesianform.h"

#include "analysisforms/semsimpleform.h"

#include <QDebug>
#include <QWebFrame>
#include <QFile>
#include <QToolTip>
#include <QClipboard>
#include <QWebElement>
#include <QMessageBox>
#include <QTimer>
#include <QStringBuilder>

#include "analysisloader.h"
#include "qutils.h"
#include "appdirs.h"

#include "lrnam.h"
#include "activitylog.h"

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	_inited = false;
	_dataSet = NULL;
	_tableModel = NULL;
	_currentOptionsWidget = NULL;
	_currentAnalysis = NULL;

	_optionsForm = NULL;

    ui->setupUi(this);

	QList<int> sizes = QList<int>();
	sizes.append(590);
	ui->splitter->setSizes(sizes);

    ui->tabBar->setFocusPolicy(Qt::NoFocus);
	ui->tabBar->addTab("File");
	ui->tabBar->addTab("Common");
	ui->tabBar->addOptionsTab();
	ui->tabBar->addHelpTab();
	connect(ui->tabBar, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));
	connect(ui->tabBar, SIGNAL(helpToggled(bool)), this, SLOT(helpToggled(bool)));

#ifdef __WIN32__
    QFont font = ui->tabBar->font();
    font.setPointSize(10);
    ui->tabBar->setFont(font);
#endif

	ui->ribbonAnalysis->setEnabled(false);
	ui->ribbonSEM->setEnabled(false);

#ifdef QT_DEBUG
	ui->webViewResults->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
	ui->webViewHelp->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
#else
	ui->webViewResults->setContextMenuPolicy(Qt::NoContextMenu);
	ui->webViewHelp->setContextMenuPolicy(Qt::NoContextMenu);
#endif

	// the LRNAM adds mime types to local resources; important for SVGs
	ui->webViewResults->page()->setNetworkAccessManager(new LRNAM(this));
	ui->webViewResults->setUrl(QUrl(QString("qrc:///core/index.html")));
	connect(ui->webViewResults, SIGNAL(loadFinished(bool)), this, SLOT(assignPPIFromWebView(bool)));

	_tableModel = new DataSetTableModel();
	ui->tableView->setModel(_tableModel);
	ui->tabBar->setCurrentIndex(1);

	ui->tableView->setVerticalScrollMode(QTableView::ScrollPerPixel);
	ui->tableView->setHorizontalScrollMode(QTableView::ScrollPerPixel);

	_analyses = new Analyses();
	_engineSync = new EngineSync(_analyses, this);
	connect(_engineSync, SIGNAL(engineTerminated()), this, SLOT(engineCrashed()));

	_analyses->analysisResultsChanged.connect(boost::bind(&MainWindow::analysisResultsChangedHandler, this, _1));

	connect(ui->ribbonAnalysis, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonSEM, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->backStage, SIGNAL(dataSetSelected(QString)), this, SLOT(dataSetSelected(QString)));
	connect(ui->backStage, SIGNAL(closeDataSetSelected()), this, SLOT(dataSetCloseRequested()));
	connect(ui->backStage, SIGNAL(exportSelected(QString)), this, SLOT(exportSelected(QString)));

	_alert = new ProgressWidget(ui->tableView);
	_alert->setAutoFillBackground(true);
	_alert->resize(400, 100);
	_alert->move(100, 80);
	_alert->hide();

	connect(&_loader, SIGNAL(complete(const QString&, DataSet*)), this, SLOT(dataSetLoaded(const QString&, DataSet*)));
	connect(&_loader, SIGNAL(progress(QString,int)), _alert, SLOT(setStatus(QString,int)));
	connect(&_loader, SIGNAL(fail(QString)), this, SLOT(dataSetLoadFailed(QString)));

	connect(this, SIGNAL(analysisSelected(int)), this, SLOT(analysisSelectedHandler(int)));
	connect(this, SIGNAL(analysisUnselected()), this, SLOT(analysisUnselectedHandler()));
	connect(this, SIGNAL(pushToClipboard(QString, QString)), this, SLOT(pushToClipboardHandler(QString, QString)));
	connect(this, SIGNAL(analysisChangedDownstream(int, QString)), this, SLOT(analysisChangedDownstreamHandler(int, QString)));

	_buttonPanel = new QWidget(ui->pageOptions);
	_buttonPanelLayout = new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, 12, 24, 0);
	_buttonPanel->setLayout(_buttonPanelLayout);

	_okButton = new QPushButton(QString("OK"), _buttonPanel);
	_okButton->setDefault(true);
	_menuButton = new QPushButton(QString("..."), _buttonPanel);

	QMenu *menu = new QMenu(_menuButton);
	menu->addAction("Remove Analysis", this, SLOT(analysisRemoved()));
	_menuButton->setMenu(menu);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_menuButton);

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->move(ui->panelMid->minimumWidth() - _buttonPanel->width(), 0);

	connect(_okButton, SIGNAL(clicked()), this, SLOT(analysisOKed()));


	connect(ui->splitter, SIGNAL(splitterMoved(int,int)), this, SLOT(splitterMovedHandler(int,int)));

	updateUIFromOptions();

	ui->panelMid->hide();

	_tableViewWidthBeforeOptionsMadeVisible = -1;

	QUrl userGuide = QUrl::fromLocalFile(AppDirs::help() + "/index.html");
	ui->webViewHelp->setUrl(userGuide);
	connect(ui->webViewHelp, SIGNAL(loadFinished(bool)), this, SLOT(helpFirstLoaded(bool)));
	ui->panelHelp->hide();

	log.log("Application Start");

	ui->backStage->setLog(&log);
	_engineSync->setLog(&log);

	log.flushLogToServer();

	QTimer *timer = new QTimer(this);
	timer->setInterval(30000);
	connect(timer, SIGNAL(timeout()), &log, SLOT(flushLogToServer()));
	timer->start();
}

void MainWindow::open(QString filename)
{
	dataSetSelected(filename);
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

void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	QString results = tq(analysis->asJSON().toStyledString());

	results = results.replace("'", "\\'");
	results = results.replace("\n", "");
	results = "window.analysisChanged(JSON.parse('" + results + "'));";

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(results);
}

AnalysisForm* MainWindow::loadForm(Analysis *analysis)
{
	string name = analysis->name();

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
	else if (name == "AnovaRepeatedMeasuresShort")
		form = new AnovaRepeatedMeasuresShortForm(contentArea);
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
	else if (name == "Correlation")
		form = new CorrelationForm(contentArea);
	else if (name == "CorrelationBayesian")
		form = new CorrelationBayesianForm(contentArea);
	else if (name == "CorrelationPartial")
		form = new CorrelationPartialForm(contentArea);
	else if (name == "Crosstabs")
		form = new CrosstabsForm(contentArea);
	else if (name == "CrosstabsBayesian")
		form = new CrosstabsBayesianForm(contentArea);
	else if (name == "SEMSimple")
		form = new SEMSimpleForm(contentArea);
	else if (name == "AncovaBayesian")
		form = new AncovaBayesianForm(contentArea);
	else if (name == "AnovaRepeatedMeasuresBayesian")
		form = new AnovaRepeatedMeasuresBayesianForm(contentArea);
	else
		qDebug() << "MainWidget::loadForm(); form not found : " << name.c_str();

	if (form != NULL)
		_analysisForms[name] = form;

	return form;
}

void MainWindow::showForm(Analysis *analysis)
{
	if (_currentOptionsWidget != NULL)
	{
		_currentOptionsWidget->hide();
		_currentOptionsWidget->unbind();
		_currentOptionsWidget = NULL;
	}

	_currentOptionsWidget = loadForm(analysis);

	if (_currentOptionsWidget != NULL)
	{
		Options *options = analysis->options();
		_currentOptionsWidget->bindTo(options, _dataSet);

		_currentOptionsWidget->show();
		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget, 0, 0, Qt::AlignLeft | Qt::AlignTop);

		if (ui->panelMid->isVisible() == false)
			showOptionsPanel();

		_buttonPanel->raise();
		_buttonPanel->show();

		QString helpPage = QString("analyses/") + tq(analysis->name()).toLower();
		requestHelpPage(helpPage);
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

		log.log("Analysis Selected", info);
	}
}

void MainWindow::analysisUnselectedHandler()
{
	hideOptionsPanel();

	if (_currentAnalysis != NULL)
	{
		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		log.log("Analysis Unselected", info);
	}
}

void MainWindow::tabChanged(int index)
{
	if (index == 0)
	{
		ui->topLevelWidgets->setCurrentIndex(0);
	}
	else if (index == ui->tabBar->count() - 1)
	{
		if (_optionsForm == NULL)
		{
			_optionsForm = new OptionsForm(this);
			ui->topLevelWidgets->addWidget(_optionsForm);
			connect(_optionsForm, SIGNAL(optionsChanged()), this, SLOT(updateUIFromOptions()));
		}

		ui->topLevelWidgets->setCurrentWidget(_optionsForm);
	}
	else
	{
		ui->topLevelWidgets->setCurrentIndex(1);
		ui->ribbon->setCurrentIndex(index - 1);
	}
}

void MainWindow::helpToggled(bool on)
{
	log.log("Help Toggled", on ? "on" : "off");

	static int helpWidth = 0;

	if (on)
	{
		if (helpWidth < 200)
			helpWidth = 200;

		QList<int> sizes = ui->splitter->sizes();

		int resultsWidth = sizes.at(2) - ui->splitter->handleWidth() - 2 - helpWidth;

		sizes[2] = resultsWidth;
		sizes[3] = helpWidth;

		ui->panelHelp->show();
		ui->splitter->setSizes(sizes);
	}
	else
	{
		helpWidth = ui->panelHelp->width();
		ui->panelHelp->hide();
	}
}

void MainWindow::dataSetSelected(const QString &filename)
{
	if (_dataSet != NULL)
	{
		// begin new instance
		QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(filename));
	}
	else
	{
		_loader.load(filename);
		_alert->show();
	}

	ui->tabBar->setCurrentIndex(1);
}

void MainWindow::dataSetCloseRequested()
{
	_tableModel->clearDataSet();
	_loader.free(_dataSet);
	_dataSet = NULL;
	updateMenuEnabledDisabledStatus();
	ui->backStage->setFileLoaded(false);
	ui->webViewResults->reload();
	_inited = false;
}

void MainWindow::dataSetLoaded(const QString &dataSetName, DataSet *dataSet)
{
	setWindowTitle(dataSetName);

	_dataSet = dataSet;
	_tableModel->setDataSet(dataSet);
	updateMenuEnabledDisabledStatus();
	ui->backStage->setFileLoaded(true);
	_analyses->clear();

	ui->tableView->horizontalHeader()->resizeSections(QHeaderView::ResizeToContents);

	_alert->hide();

	if (_inited == false)
	{
		ui->webViewResults->page()->mainFrame()->addToJavaScriptWindowObject("jasp", this);
		_inited = true;
	}

	if (_engineSync->engineStarted() == false)
		_engineSync->start();

}

void MainWindow::dataSetLoadFailed(const QString &message)
{
	_alert->hide();
	QMessageBox::warning(this, "", "The Data set could not be loaded\n\n" + message);
}

void MainWindow::updateMenuEnabledDisabledStatus()
{
	bool enable = _dataSet != NULL;

	ui->ribbonAnalysis->setEnabled(enable);
	ui->ribbonSEM->setEnabled(enable);
}

void MainWindow::updateUIFromOptions()
{
	QSettings settings;
	QVariant sem = settings.value("plugins/sem", false);
	if (sem.canConvert(QVariant::Bool) && sem.toBool())
	{
		ui->tabBar->addTab("SEM");
	}
	else
	{
		if (ui->tabBar->count() >= 4)
			ui->tabBar->removeTab(2);
	}

}

void MainWindow::assignPPIFromWebView(bool success)
{
	if (success)
	{
		QVariant ppiv = ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.getPPI()");

		bool success;
		int ppi = ppiv.toInt(&success);

		if (success)
			_engineSync->setPPI(ppi);
	}
}

void MainWindow::engineCrashed()
{
	log.log("Engine Crashed");

	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;

		QMessageBox::warning(this, "Error", "The JASP Statistics Engine has terminated unexpectedly.\n\nIf you could report your experiences to the JASP team that would be appreciated.\n\nJASP cannot continue and will now close.");
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
		content = "404\n===\n\n**" + pageName + "** could not be found";
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
	string name = item.toStdString();
	_currentAnalysis = _analyses->create(name);
	if (_currentAnalysis != NULL)
	{
		showForm(_currentAnalysis);
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.select(" % QString::number(_currentAnalysis->id()) % ")");

		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		log.log("Analysis Created", info);
	}
}

void MainWindow::exportSelected(const QString &filename)
{
	QWebElement element = ui->webViewResults->page()->mainFrame()->documentElement().clone();

	QWebElementCollection nodes;

	nodes = element.findAll("html > head > script, html > head > link, #spacer, #intro, #templates");
	foreach (QWebElement node, nodes)
		node.removeFromDocument();

	nodes = element.findAll(".selected, .unselected");
	foreach (QWebElement node, nodes)
	{
		node.removeClass("selected");
		node.removeClass("unselected");
	}

	QWebElement head = element.findFirst("html > head");

	QFile ss(":/core/css/theme-jasp.css");
	ss.open(QIODevice::ReadOnly);
	QByteArray bytes = ss.readAll();
	QString css = QString::fromUtf8(bytes);

	head.setInnerXml(QString("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><title>JASP Results</title><style type=\"text/css\">") + css + QString("</style>"));

	QFile file(filename);
	file.open(QIODevice::WriteOnly | QIODevice::Truncate);
	QTextStream stream(&file);
	stream.setCodec("UTF-8");

	stream << element.toOuterXml();
	stream.flush();
	file.close();

	ui->tabBar->setCurrentIndex(1);
}

void MainWindow::adjustOptionsPanelWidth()
{
	if (ui->panelMid->width() == ui->panelMid->maximumWidth() && ui->tableView->isHidden())
	{
		showTableView();
	}
	else if (ui->tableView->width() == ui->tableView->minimumWidth() && ui->tableView->isVisible() && ui->panelMid->isVisible())
	{
		hideTableView();
	}
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

		if (ui->tableView->isVisible())
			newTableWidth += ui->splitter->handleWidth() + 2;

		newTableWidth += sizes.at(1);
	}

	sizes[0] = newTableWidth;
	sizes[1] = 0;

	ui->panelMid->hide();
	ui->tableView->show();
	ui->splitter->setSizes(sizes);
}

void MainWindow::showOptionsPanel()
{
	QList<int> sizes = ui->splitter->sizes();

	int tableWidth = sizes.at(0);
	int newTableWidth = tableWidth;
	newTableWidth -= ui->panelMid->minimumWidth();
	newTableWidth -= ui->splitter->handleWidth();

	ui->panelMid->show();

	if (newTableWidth < ui->tableView->minimumWidth())
	{
		int midPanelWidth = tableWidth;
		if (midPanelWidth < ui->panelMid->minimumWidth())
		{
			_tableViewWidthBeforeOptionsMadeVisible = midPanelWidth;
			midPanelWidth = ui->panelMid->minimumWidth();
		}

		int w = 0;
		w += ui->panelMid->minimumWidth();
		w += ui->tableView->minimumWidth();
		w += ui->splitter->handleWidth();

		ui->panelMid->setMaximumWidth(w+8);
		ui->tableView->hide();

		sizes[0] = 0;
		sizes[1] = midPanelWidth;

		ui->splitter->setSizes(sizes);
	}
	else
	{
		ui->panelMid->setMaximumWidth(ui->panelMid->minimumWidth());

		sizes[0] = newTableWidth - 2;
		sizes[1] = ui->panelMid->minimumWidth();

		ui->splitter->setSizes(sizes);
	}
}

void MainWindow::showTableView()
{
	QList<int> sizes = ui->splitter->sizes();

	sizes[0] = ui->tableView->minimumWidth()+8;
	sizes[1] = ui->panelMid->minimumWidth();

	ui->splitter->setSizes(sizes);

	ui->panelMid->setMaximumWidth(ui->panelMid->minimumWidth());
	ui->tableView->show();
}

void MainWindow::hideTableView()
{
	QList<int> sizes = ui->splitter->sizes();

	int w = 0;
	w += ui->panelMid->minimumWidth();
	w += ui->tableView->minimumWidth();
	w += ui->splitter->handleWidth();

	ui->panelMid->setMaximumWidth(w+8);
	ui->tableView->hide();

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

		log.log("Analysis OKed", info);

		_currentOptionsWidget->hide();
		_currentOptionsWidget->unbind();
		_currentOptionsWidget = NULL;
	}

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.unselect()");

	hideOptionsPanel();
}

void MainWindow::analysisRemoved()
{
	if (_currentOptionsWidget != NULL)
	{
		_currentOptionsWidget->hide();
		_currentOptionsWidget->unbind();
		_currentOptionsWidget = NULL;

		QString info("%1,%2");
		info = info.arg(tq(_currentAnalysis->name()));
		info = info.arg(_currentAnalysis->id());

		log.log("Analysis Removed", info);
	}

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.remove(" % QString::number(_currentAnalysis->id()) % ")");

	hideOptionsPanel();
}

void MainWindow::pushToClipboardHandler(const QString &mimeType, const QString &data)
{
	log.log("Copy");

	QMimeData *mimeData = new QMimeData();

	if (mimeType == "text/html")
	{
		QString toClipboard;
		toClipboard += "<!DOCTYPE HTML>\n"
					   "<html>\n"
					   "	<head>\n"
					   "		<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />\n"
					   "		<title>JASP</title>"
					   "	</head>\n"
					   "	<body>\n";
		toClipboard += data;
		toClipboard += "	</body>\n"
					   "</html>";

		mimeData->setData("text/html", toClipboard.toUtf8());
	}
	else
	{
		mimeData->setData(mimeType, data.toUtf8());
	}

	QClipboard *clipboard = QApplication::clipboard();
	clipboard->setMimeData(mimeData, QClipboard::Clipboard);

	//qDebug() << clipboard->mimeData(QClipboard::Clipboard)->data("text/html");
}

void MainWindow::analysisChangedDownstreamHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == NULL)
		return;

	QString info("%1,%2");
	info = info.arg(tq(analysis->name()));
	info = info.arg(id);

	log.log("Analysis Changed Downstream", info);

	string utf8 = fq(options);

	Json::Value root;

	Json::Reader parser;
	parser.parse(utf8, root);

	analysis->options()->set(root);


}
