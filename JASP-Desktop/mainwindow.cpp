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
#include <QWebElement>
#include <QMessageBox>

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

	_optionsForm = NULL;

    ui->setupUi(this);

	ui->pageOptions->hide();

	QList<int> sizes = QList<int>();
	sizes.append(590);
	ui->splitter->setSizes(sizes);

    ui->tabBar->setFocusPolicy(Qt::NoFocus);
	ui->tabBar->addTab("File");
	ui->tabBar->addTab("Common");
	ui->tabBar->addLastTab("Options");
	connect(ui->tabBar, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));

#ifdef __WIN32__
    QFont font = ui->tabBar->font();
    font.setPointSize(10);
    ui->tabBar->setFont(font);
#endif

	ui->ribbonAnalysis->setEnabled(false);
	ui->ribbonSEM->setEnabled(false);

#ifdef QT_DEBUG
	ui->webViewResults->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
#else
	ui->webViewResults->setContextMenuPolicy(Qt::NoContextMenu);
#endif

	ui->webViewResults->setUrl(QUrl(QString("qrc:///core/index.html")));

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

	connect(&_loader, SIGNAL(complete(DataSet*)), this, SLOT(dataSetLoaded(DataSet*)));
	connect(&_loader, SIGNAL(progress(QString,int)), _alert, SLOT(setStatus(QString,int)));
	connect(this, SIGNAL(analysisSelected(int)), this, SLOT(analysisSelectedHandler(int)));
	connect(this, SIGNAL(analysisUnselected()), this, SLOT(analysisUnselectedHandler()));
	connect(this, SIGNAL(pushToClipboard(QString)), this, SLOT(pushToClipboardHandler(QString)));
	connect(this, SIGNAL(analysisChangedDownstream(int, QString)), this, SLOT(analysisChangedDownstreamHandler(int, QString)));

	_buttonPanel = new QWidget(ui->pageOptions);
	_buttonPanelLayout = new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, 12, 24, 0);
	_buttonPanel->setLayout(_buttonPanelLayout);

	_okButton = new QPushButton(QString("OK"), _buttonPanel);
	_removeButton = new QPushButton(QString("Remove"), _buttonPanel);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_removeButton);

	_buttonPanel->resize(_buttonPanel->sizeHint());

	QTimer::singleShot(0, this, SLOT(repositionButtonPanel()));
	connect(_okButton, SIGNAL(clicked()), this, SLOT(analysisOKed()));
	connect(_removeButton, SIGNAL(clicked()), this, SLOT(analysisRemoved()));

	connect(ui->splitter, SIGNAL(splitterMoved(int,int)), this, SLOT(splitterMovedHandler(int,int)));

	updateUIFromOptions();
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
	adjustOptionsPanelWidth();
}

void MainWindow::analysisResultsChangedHandler(Analysis *analysis)
{
	string results = analysis->asJSON().toStyledString();

	QString eval = tq("window.analysisChanged(" + results + ")");

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
		ui->pageOptions->show();

		_buttonPanel->raise();
		_buttonPanel->show();

		adjustOptionsPanelWidth();
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
	ui->pageOptions->hide();
	ui->tableView->show();
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

void MainWindow::dataSetSelected(const QString &filename)
{
	if (_dataSet != NULL)
	{
		// begin new instance
		QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(filename));
	}
	else
	{
		/*if (_dataSet != NULL)
		{
			_loader.free(_dataSet);
			_dataSet = NULL;
		}*/

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

void MainWindow::dataSetLoaded(DataSet *dataSet)
{
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

void MainWindow::engineCrashed()
{
	static bool exiting = false;

	if (exiting == false)
	{
		exiting = true;

		QMessageBox::warning(this, "Error", "The JASP Statistics Engine has terminated unexpectedly.\n\nThis sometimes happens under older versions of Operating Systems that have not yet been tested.\n\nThe JASP team is actively working on this, and if you could report your experiences that would be appreciated.\n\nJASP cannot continue and will now close.");
		QApplication::exit(1);
	}
}

void MainWindow::itemSelected(const QString item)
{
	string name = item.toStdString();
	_currentAnalysis = _analyses->create(name);
	if (_currentAnalysis != NULL)
	{
		showForm(_currentAnalysis);
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.select(" % QString::number(_currentAnalysis->id()) % ")");
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
	stream << element.toOuterXml();
	stream.flush();
	file.close();

	ui->tabBar->setCurrentIndex(1);
}

void MainWindow::adjustOptionsPanelWidth()
{
	int splitterPos = ui->splitter->sizes()[0];

	if (splitterPos < ui->pageOptions->minimumWidth())
		splitterPos = ui->pageOptions->minimumWidth();

	if (ui->pageOptions->width() == ui->pageOptions->maximumWidth() && ui->tableView->isHidden())
	{
		ui->tableView->show();
		repositionButtonPanel(ui->pageOptions->minimumWidth());
	}
	else if (ui->tableView->width() == ui->tableView->minimumWidth() && ui->tableView->isVisible() && ui->pageOptions->isVisible())
	{
		ui->tableView->hide();
		repositionButtonPanel(splitterPos);
	}
	else if (splitterPos > ui->pageOptions->maximumWidth())
	{
		repositionButtonPanel(ui->pageOptions->minimumWidth());
	}
	else
	{
		repositionButtonPanel();
	}

}

void MainWindow::splitterMovedHandler(int, int)
{
	repositionButtonPanel();
	adjustOptionsPanelWidth();
}

void MainWindow::repositionButtonPanel(int parentWidth)
{
	int overallWidth = parentWidth;
	if (parentWidth == -1)
		overallWidth = ui->pageOptions->width();
	int panelWidth = _buttonPanel->width();

	_buttonPanel->move(overallWidth - panelWidth, 0);
	_buttonPanel->raise();
}

void MainWindow::analysisOKed()
{
	ui->pageOptions->hide();
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.unselect()");
	ui->tableView->show();
}

void MainWindow::analysisRemoved()
{
	ui->pageOptions->hide();
	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.remove(" % QString::number(_currentAnalysis->id()) % ")");
	ui->tableView->show();
}

void MainWindow::pushToClipboardHandler(QString data)
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

	QMimeData *mimeData = new QMimeData();
	mimeData->setData("text/html", toClipboard.toUtf8());

	QClipboard *clipboard = QApplication::clipboard();
	clipboard->setMimeData(mimeData, QClipboard::Clipboard);

	//qDebug() << clipboard->mimeData(QClipboard::Clipboard)->data("text/html");
}

void MainWindow::analysisChangedDownstreamHandler(int id, QString options)
{
	Analysis *analysis = _analyses->get(id);
	if (analysis == NULL)
		return;

	string utf8 = fq(options);

	Json::Value root;

	Json::Reader parser;
	parser.parse(utf8, root);

	analysis->options()->set(root);


}
