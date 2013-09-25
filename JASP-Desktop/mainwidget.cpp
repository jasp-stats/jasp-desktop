#include "mainwidget.h"
#include "ui_mainwidget.h"

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

#include <QDebug>
#include <QWebFrame>
#include <QFile>
#include <QToolTip>

#include <QStringBuilder>

#include "analysisloader.h"

MainWidget::MainWidget(QWidget *parent) :
    QWidget(parent),
	ui(new Ui::MainWidget)
{
	_inited = false;
	_dataSet = NULL;
	_tableModel = NULL;
	_currentOptionsWidget = NULL;

    ui->setupUi(this);

    QList<int> sizes = QList<int>();
    sizes.append(600);
    sizes.append(1);
    ui->splitter->setSizes(sizes);

    ui->tabBar->setFocusPolicy(Qt::NoFocus);
    ui->tabBar->addTab(QString("File"));
	ui->tabBar->addTab(QString("Home"));
	ui->tabBar->addTab(QString("Analysis"));

	QFile indexPageResource(QString(":/core/analyses.html"));
    indexPageResource.open(QFile::ReadOnly);
    QString indexPage(indexPageResource.readAll());

    ui->webViewOptions->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
    ui->webViewOptions->page()->settings()->setAttribute(QWebSettings::PluginsEnabled, true);
	ui->webViewOptions->setHtml(indexPage, QUrl("qrc:/core/"));

    ui->homeRibbon->setEnabled(false);
	ui->ribbonAnalysis->setEnabled(false);

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

	_analyses->analysisResultsChanged.connect(boost::bind(&MainWidget::analysisResultsChangedHandler, this, _1));

	connect(ui->ribbonAnalysis, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->backStage, SIGNAL(dataSetSelected(QString)), this, SLOT(dataSetSelected(QString)));

	_alert = new ProgressWidget(ui->pageData);
	_alert->setAutoFillBackground(true);
	_alert->resize(400, 100);
	_alert->move(100, 80);
	_alert->hide();

	connect(&_loader, SIGNAL(complete(DataSet*)), this, SLOT(dataSetLoaded(DataSet*)));
	connect(&_loader, SIGNAL(progress(QString,int)), _alert, SLOT(setStatus(QString,int)));
	connect(this, SIGNAL(analysisSelected(int)), this, SLOT(analysisSelectedHandler(int)));

}

MainWidget::~MainWidget()
{
	delete ui;
}

void MainWidget::analysisResultsChangedHandler(Analysis *analysis)
{
	string eval = "window.analysisChanged(" + analysis->asJSON().toStyledString() + ")";
	QString evalQString = QString::fromUtf8(eval.c_str(), eval.length());

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(evalQString);
}

AnalysisForm* MainWidget::loadForm(Analysis *analysis)
{
	string name = analysis->name();

	if (name == "Descriptives")
		return new DescriptivesForm(ui->optionsContentArea);
	else if (name == "TTestBayesianOneSample")
		return new TTestBayesianOneSampleForm(ui->optionsContentArea);
	else if (name == "TTestIndependentSamples")
		return new TTestIndependentSamplesForm(ui->optionsContentArea);
	else if (name == "TTestPairedSamples")
		return new TTestPairedSamplesForm(ui->optionsContentArea);
	else if (name == "TTestOneSample")
		return new TTestOneSampleForm(ui->optionsContentArea);
	else if (name == "AnovaBayesian")
		return new AnovaBayesianForm(ui->optionsContentArea);
	else if (name == "AnovaOneWay")
		return new AnovaOneWayForm(ui->optionsContentArea);
	else if (name == "Anova")
		return new AnovaForm(ui->optionsContentArea);
	else if (name == "Ancova")
		return new AncovaForm(ui->optionsContentArea);
	else if (name == "AnovaMultivariate")
		return new AnovaMultivariateForm(ui->optionsContentArea);
	else if (name == "AncovaMultivariate")
		return new AncovaMultivariateForm(ui->optionsContentArea);
	else
		return NULL;
}

void MainWidget::showForm(Analysis *analysis)
{
	if (_currentOptionsWidget != NULL)
	{
		delete _currentOptionsWidget;
		_currentOptionsWidget = NULL;
	}

	_currentOptionsWidget = loadForm(analysis);

	if (_currentOptionsWidget != NULL)
	{
		Options *options = analysis->options();
		_currentOptionsWidget->set(options, _dataSet);

		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget, 0, 0, Qt::AlignLeft | Qt::AlignTop);
		ui->stackedLHS->setCurrentWidget(ui->pageOptions2);

		connect(_currentOptionsWidget, SIGNAL(accepted()), this, SLOT(analysisOKed()));
	}
}

void MainWidget::analysisSelectedHandler(int id)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis != NULL)
		showForm(analysis);
}

void MainWidget::tabChanged(int index)
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

void MainWidget::dataSetSelected(const QString &filename)
{
	if (_inited == false)
	{
		ui->webViewResults->page()->mainFrame()->addToJavaScriptWindowObject("jasp", this);
		_inited = true;
	}

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

void MainWidget::dataSetLoaded(DataSet *dataSet)
{
	_dataSet = dataSet;

	_tableModel->setDataSet(dataSet);

    ui->homeRibbon->setEnabled(true);
	ui->ribbonAnalysis->setEnabled(true);

	_alert->hide();

	//QToolTip::showText(ui->tableView->pos(), QString("bruce"), ui->tableView, QRect(100, 100, 200, 200));

}

void MainWidget::itemSelected(const QString item)
{
	string name = item.toStdString();
	Analysis *analysis = _analyses->create(name);
	if (analysis != NULL)
		showForm(analysis);
}

void MainWidget::messageReceived(const QString message)
{
	QString eval = "window.receiveMessage(" % message % ")";
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(eval);
}

void MainWidget::analysisOKed()
{
	ui->stackedLHS->setCurrentWidget(ui->pageData);
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.unselect()");
}
