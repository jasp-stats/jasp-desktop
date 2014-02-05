#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "analysisforms/descriptivesform.h"

#include "analysisforms/ttestbayesianonesampleform.h"
#include "analysisforms/ttestpairedsamplesform.h"
#include "analysisforms/ttestindependentsamplesform.h"
#include "analysisforms/ttestonesampleform.h"

#include "analysisforms/anovabayesianform.h"
#include "analysisforms/anovaonewayform.h"
#include "analysisforms/anovaform.h"
#include "analysisforms/anovamultivariateform.h"
#include "analysisforms/ancovaform.h"
#include "analysisforms/ancovamultivariateform.h"
#include "analysisforms/regressionlinearform.h"
#include "analysisforms/correlationform.h"
#include "analysisforms/correlationpartialform.h"
#include "analysisforms/crosstabsform.h"

#include "analysisforms/semsimpleform.h"

#include <QDebug>
#include <QWebFrame>
#include <QFile>
#include <QToolTip>
#include <QClipboard>

#include <QStringBuilder>

#include "analysisloader.h"
#include "utils.h"

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	_inited = false;
	_dataSet = NULL;
	_tableModel = NULL;
	_currentOptionsWidget = NULL;
	_currentAnalysis = NULL;

    ui->setupUi(this);

    QList<int> sizes = QList<int>();
    sizes.append(600);
    sizes.append(1);
    ui->splitter->setSizes(sizes);

    ui->tabBar->setFocusPolicy(Qt::NoFocus);
    ui->tabBar->addTab(QString("File"));
	ui->tabBar->addTab(QString("Home"));
	ui->tabBar->addTab(QString("Common"));
	ui->tabBar->addTab(QString("SEM"));

	QFile indexPageResource(QString(":/core/analyses.html"));
    indexPageResource.open(QFile::ReadOnly);
    QString indexPage(indexPageResource.readAll());

#ifndef QT_NO_DEBUG
    ui->webViewOptions->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
    ui->webViewOptions->page()->settings()->setAttribute(QWebSettings::PluginsEnabled, true);
#endif

	ui->webViewOptions->setHtml(indexPage, QUrl("qrc:/core/"));

	ui->ribbonHome->setEnabled(false);
	ui->ribbonAnalysis->setEnabled(false);
	ui->ribbonSEM->setEnabled(false);

	ui->webViewResults->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
	ui->webViewResults->setUrl(QUrl(QString("qrc:///core/index.html")));

	QFile a(QString(":/core/index.html"));
	a.open(QFile::ReadOnly);

	_tableModel = new DataSetTableModel();
	ui->tableView->setModel(_tableModel);
	ui->stackedLHS->setCurrentWidget(ui->pageData);
	ui->tabBar->setCurrentIndex(1);

	ui->tableView->setVerticalScrollMode(QTableView::ScrollPerPixel);
	ui->tableView->setHorizontalScrollMode(QTableView::ScrollPerPixel);

	_analyses = new Analyses();
	_engineSync = new EngineSync(_analyses, this);

	_analyses->analysisResultsChanged.connect(boost::bind(&MainWindow::analysisResultsChangedHandler, this, _1));

	connect(ui->ribbonAnalysis, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonSEM, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->backStage, SIGNAL(dataSetSelected(QString)), this, SLOT(dataSetSelected(QString)));

	_alert = new ProgressWidget(ui->pageData);
	_alert->setAutoFillBackground(true);
	_alert->resize(400, 100);
	_alert->move(100, 80);
	_alert->hide();

	connect(&_loader, SIGNAL(complete(DataSet*)), this, SLOT(dataSetLoaded(DataSet*)));
	connect(&_loader, SIGNAL(progress(QString,int)), _alert, SLOT(setStatus(QString,int)));
	connect(this, SIGNAL(analysisSelected(int)), this, SLOT(analysisSelectedHandler(int)));
	connect(this, SIGNAL(analysisUnselected()), this, SLOT(analysisUnselectedHandler()));
	connect(this, SIGNAL(pushToClipboard(QString)), this, SLOT(pushToClipboardHandler(QString)));

	_buttonPanel = new QWidget(this);
	_buttonPanelLayout = new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, 12, 24, 0);
	_buttonPanel->setLayout(_buttonPanelLayout);

	_okButton = new QPushButton(QString("OK"), _buttonPanel);
	_removeButton = new QPushButton(QString("Remove"), _buttonPanel);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_removeButton);

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->hide();

	QTimer::singleShot(0, this, SLOT(repositionButtonPanel()));
	connect(_okButton, SIGNAL(clicked()), this, SLOT(analysisOKed()));
	connect(_removeButton, SIGNAL(clicked()), this, SLOT(analysisRemoved()));

	connect(ui->splitter, SIGNAL(splitterMoved(int,int)), this, SLOT(repositionButtonPanel()));
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
	repositionButtonPanel();
}

void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	string results = analysis->asJSON().toStyledString();
	string render = analysis->js();

	QString eval = tq("window.analysisChanged(" + render + ", " + results + ")");

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(eval);
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
	else if (name == "Ancova")
		form = new AncovaForm(contentArea);
	else if (name == "AnovaMultivariate")
		form = new AnovaMultivariateForm(contentArea);
	else if (name == "AncovaMultivariate")
		form = new AncovaMultivariateForm(contentArea);
	else if (name == "RegressionLinear")
		form = new RegressionLinearForm(contentArea);
	else if (name == "Correlation")
		form = new CorrelationForm(contentArea);
	else if (name == "CorrelationPartial")
		form = new CorrelationPartialForm(contentArea);
	else if (name == "Crosstabs")
		form = new CrosstabsForm(contentArea);
	else if (name == "SEMSimple")
		form = new SEMSimpleForm(contentArea);
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
		_currentOptionsWidget = NULL;
	}

	_currentOptionsWidget = loadForm(analysis);

	if (_currentOptionsWidget != NULL)
	{
		Options *options = analysis->options();
		_currentOptionsWidget->set(options, _dataSet);

		_currentOptionsWidget->show();
		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget, 0, 0, Qt::AlignLeft | Qt::AlignTop);
		ui->stackedLHS->setCurrentWidget(ui->pageOptions2);

		_buttonPanel->raise();
		_buttonPanel->show();
	}
}

void MainWindow::analysisSelectedHandler(int id)
{
	_currentAnalysis = _analyses->get(id);
	if (_currentAnalysis != NULL)
		showForm(_currentAnalysis);
}

void MainWindow::analysisUnselectedHandler()
{
	ui->stackedLHS->setCurrentWidget(ui->pageData);
	_buttonPanel->hide();
}

void MainWindow::tabChanged(int index)
{
	if (index == 0)
	{
		ui->topLevelWidgets->setCurrentIndex(0);
	}
	else
	{
		ui->topLevelWidgets->setCurrentIndex(1);
		ui->ribbon->setCurrentIndex(index - 1);
	}
}

void MainWindow::dataSetSelected(const QString &filename)
{
	_tableModel->clearDataSet();

	if (_dataSet != NULL)
	{
		_loader.free(_dataSet);
		_dataSet = NULL;
	}

	_loader.load(filename);
	_alert->show();
	ui->tabBar->setCurrentIndex(1);
}

void MainWindow::dataSetLoaded(DataSet *dataSet)
{
	_dataSet = dataSet;

	_tableModel->setDataSet(dataSet);

	ui->ribbonHome->setEnabled(true);
	ui->ribbonAnalysis->setEnabled(true);
	ui->ribbonSEM->setEnabled(true);

	_alert->hide();

	if (_inited == false)
	{
		ui->webViewResults->page()->mainFrame()->addToJavaScriptWindowObject("jasp", this);
		_inited = true;
	}

}

void MainWindow::itemSelected(const QString item)
{
	string name = item.toStdString();
	_currentAnalysis = _analyses->create(name);
	if (_currentAnalysis != NULL)
	{
		showForm(_currentAnalysis);
		repositionButtonPanel();
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.select(" % QString::number(_currentAnalysis->id()) % ")");
	}
}

void MainWindow::repositionButtonPanel()
{
	int overallWidth = ui->splitter->sizes().at(0);
	int panelWidth = _buttonPanel->width();

	QPoint pos = ui->splitter->mapTo(this, QPoint());

	_buttonPanel->move(overallWidth - panelWidth, pos.y());
	_buttonPanel->raise();
}

void MainWindow::analysisOKed()
{
	ui->stackedLHS->setCurrentWidget(ui->pageData);
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.unselect()");
	_buttonPanel->hide();
}

void MainWindow::analysisRemoved()
{
	ui->stackedLHS->setCurrentWidget(ui->pageData);
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.remove(" % QString::number(_currentAnalysis->id()) % ")");
	_buttonPanel->hide();
}

void MainWindow::pushToClipboardHandler(QString data)
{
	QString toClipboard;
	toClipboard += "<html>\n"
			"	<head>\n"
			"		<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />\n"
			"	</head>\n"
			"	<body>\n";
	toClipboard += data;
	toClipboard += "	</body>\n"
			"</html>";

	QMimeData *mimeData = new QMimeData();
	mimeData->setData("text/html", toClipboard.toUtf8());

	QClipboard *clipboard = QApplication::clipboard();
	clipboard->setMimeData(mimeData, QClipboard::Clipboard);

	//qDebug() << clipboard->mimeData(QClipboard::Clipboard)->data("text/html");
}
