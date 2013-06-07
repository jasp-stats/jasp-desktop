#include "mainwidget.h"
#include "ui_mainwidget.h"

#include "analysisforms/descriptives.h"
#include "analysisforms/ttestonesample.h"

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

	_analyses = new Analyses();
	_engineSync = new EngineSync(_analyses, this);

	_analyses->analysisResultsChanged.connect(boost::bind(&MainWidget::analysisResultsChangedHandler, this, _1));

	connect(ui->ribbonAnalysis, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));

}

MainWidget::~MainWidget()
{
	delete ui;
}

void MainWidget::analysisResultsChangedHandler(Analysis *analysis)
{
	string eval = "window.analysisChanged(" + analysis->results() + ")";
	QString evalQString = QString::fromUtf8(eval.c_str(), eval.length());

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(evalQString);
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

void MainWidget::dataSetLoaded(DataSet *dataSet)
{
	_dataSet = dataSet;

	_tableModel->setDataSet(dataSet);

    ui->homeRibbon->setEnabled(true);
	ui->ribbonAnalysis->setEnabled(true);
	ui->tabBar->setCurrentIndex(1);

	QToolTip::showText(ui->tableView->pos(), QString("bruce"), ui->tableView, QRect(100, 100, 200, 200));

}

void MainWidget::itemSelected(const QString item)
{
	if (_currentOptionsWidget != NULL)
	{
		delete _currentOptionsWidget;
		_currentOptionsWidget = NULL;
	}

	Analysis *analysis;

	if (item == "Descriptives")
    {
		analysis = _analyses->create("Descriptives");
		_currentOptionsWidget = new Descriptives(ui->optionsContentArea);
    }
	else if (item == "TTestOneSample")
	{
		analysis = _analyses->create("TTestOneSample");
		_currentOptionsWidget = new TTestOneSample(ui->optionsContentArea);
	}


	if (_currentOptionsWidget != NULL)
	{
		Options *options = analysis->options();
		_currentOptionsWidget->set(options, _dataSet);

		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget, 0, 0, Qt::AlignLeft | Qt::AlignTop);
		ui->stackedLHS->setCurrentWidget(ui->pageOptions2);

		connect(_currentOptionsWidget, SIGNAL(accepted()), this, SLOT(analysisOKed()));
	}

}

void MainWidget::messageReceived(const QString message)
{
	QString eval = "window.receiveMessage(" % message % ")";
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(eval);
}

void MainWidget::analysisOKed()
{
	ui->stackedLHS->setCurrentWidget(ui->pageData);
}
