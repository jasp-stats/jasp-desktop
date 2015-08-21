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
#include "analysisforms/correlationform.h"
#include "analysisforms/correlationbayesianform.h"
#include "analysisforms/correlationbayesianpairsform.h"
#include "analysisforms/correlationpartialform.h"
#include "analysisforms/contingencytablesform.h"
#include "analysisforms/contingencytablesbayesianform.h"

#include "analysisforms/semsimpleform.h"
#include "analysisforms/r11tlearnform.h"

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
#include <QFileInfo>
#include <QShortcut>

#include "analysisloader.h"
#include "qutils.h"
#include "appdirs.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "appinfo.h"

#include "lrnam.h"
#include "activitylog.h"

MainWindow::MainWindow(QWidget *parent) :
	QMainWindow(parent),
	ui(new Ui::MainWindow)
{
	_inited = false;
	_tableModel = NULL;
	_currentOptionsWidget = NULL;
	_currentAnalysis = NULL;

	_optionsForm = NULL;

	_package = new DataSetPackage();

	_package->isModifiedChanged.connect(boost::bind(&MainWindow::packageChanged, this, _1));
	QShortcut *saveShortcut = new QShortcut(QKeySequence("Ctrl+S"), this);
	QObject::connect(saveShortcut, SIGNAL(activated()), this, SLOT(saveKeysSelected()));
	QShortcut *openShortcut = new QShortcut(QKeySequence("Ctrl+O"), this);
	QObject::connect(openShortcut, SIGNAL(activated()), this, SLOT(openKeysSelected()));

	ui->setupUi(this);

	QList<int> sizes = QList<int>();
	sizes.append(590);
	ui->splitter->setSizes(sizes);

	ui->tabBar->setFocusPolicy(Qt::NoFocus);
	ui->tabBar->addTab("File");
	ui->tabBar->addTab("Common");
#ifndef __linux__
	ui->tabBar->addOptionsTab(); // no SEM under linux for now
#endif
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
	ui->ribbonR11tLearn->setEnabled(false);

#ifdef QT_DEBUG
	ui->webViewResults->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
	ui->webViewHelp->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
#else
	ui->webViewResults->setContextMenuPolicy(Qt::NoContextMenu);
	ui->webViewHelp->setContextMenuPolicy(Qt::NoContextMenu);
#endif

	tempfiles_init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	// the LRNAM adds mime types to local resources; important for SVGs
	ui->webViewResults->page()->setNetworkAccessManager(new LRNAM(tq(tempfiles_sessionDirName()), this));
	ui->webViewResults->setUrl(QUrl(QString("qrc:///core/index.html")));
	connect(ui->webViewResults, SIGNAL(loadFinished(bool)), this, SLOT(resultsPageLoaded(bool)));
	connect(ui->webViewResults, SIGNAL(scrollValueChanged()), this, SLOT(scrollValueChangedHandle()));

	_tableModel = new DataSetTableModel();
	ui->tableView->setModel(_tableModel);
	ui->tabBar->setCurrentIndex(1);

	ui->tableView->setVerticalScrollMode(QTableView::ScrollPerPixel);
	ui->tableView->setHorizontalScrollMode(QTableView::ScrollPerPixel);

	_analyses = new Analyses();
	_engineSync = new EngineSync(_analyses, this);
	connect(_engineSync, SIGNAL(engineTerminated()), this, SLOT(fatalError()));

	connect(_analyses, SIGNAL(analysisResultsChanged(Analysis*)), this, SLOT(analysisResultsChangedHandler(Analysis*)));
	connect(_analyses, SIGNAL(analysisNotesLoaded(Analysis*)), this, SLOT(analysisNotesLoadedHandler(Analysis*)));

	connect(ui->ribbonAnalysis, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonSEM, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->ribbonR11tLearn, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));
	connect(ui->backStage, SIGNAL(dataSetSelected(QString)), this, SLOT(dataSetSelected(QString)));
	connect(ui->backStage, SIGNAL(closeDataSetSelected()), this, SLOT(dataSetCloseRequested()));
	connect(ui->backStage, SIGNAL(exportSelected(QString)), this, SLOT(exportSelected(QString)));
	connect(ui->backStage, SIGNAL(saveSelected(QString)), this, SLOT(saveSelected(QString)));

	_alert = new ProgressWidget(ui->tableView);
	_alert->setAutoFillBackground(true);
	_alert->resize(400, 100);
	_alert->move(100, 80);
	_alert->hide();

	connect(&_loader, SIGNAL(complete(const QString&, DataSetPackage*, const QString&)), this, SLOT(dataSetLoaded(const QString&, DataSetPackage*, const QString&)));
	connect(&_loader, SIGNAL(saveComplete(const QString&)), this, SLOT(saveComplete(const QString&)));
	connect(&_loader, SIGNAL(progress(QString,int)), _alert, SLOT(setStatus(QString,int)));
	connect(&_loader, SIGNAL(fail(QString)), this, SLOT(dataSetLoadFailed(QString)));
	connect(&_loader, SIGNAL(saveFail(QString)), this, SLOT(saveFailed(QString)));

	connect(this, SIGNAL(analysisSelected(int)), this, SLOT(analysisSelectedHandler(int)));
	connect(this, SIGNAL(analysisUnselected()), this, SLOT(analysisUnselectedHandler()));
	connect(this, SIGNAL(saveTempImage(int, QString, QByteArray)), this, SLOT(saveTempImageHandler(int, QString, QByteArray)));
	connect(this, SIGNAL(pushToClipboard(QString, QString, QString)), this, SLOT(pushToClipboardHandler(QString, QString, QString)));
	connect(this, SIGNAL(pushImageToClipboard(QByteArray, QString)), this, SLOT(pushImageToClipboardHandler(QByteArray, QString)));
	connect(this, SIGNAL(saveTextToFile(QString, QString)), this, SLOT(saveTextToFileHandler(QString, QString)));
	connect(this, SIGNAL(analysisChangedDownstream(int, QString)), this, SLOT(analysisChangedDownstreamHandler(int, QString)));
	connect(this, SIGNAL(showAnalysesMenu(QString)), this, SLOT(showAnalysesMenuHandler(QString)));
	connect(this, SIGNAL(removeAnalysisRequest(int)), this, SLOT(removeAnalysisRequestHandler(int)));
	connect(this, SIGNAL(updateNote(int, QString)), this, SLOT(updateNoteHandler(int, QString)));


	_buttonPanel = new QWidget(ui->pageOptions);
	_buttonPanelLayout = new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanelLayout->setContentsMargins(0, 12, 24, 0);
	_buttonPanel->setLayout(_buttonPanelLayout);

	_okButton = new QPushButton(QString("OK"), _buttonPanel);
	_okButton->setDefault(true);
	_runButton = new QPushButton(QString("Run"), _buttonPanel);
	_menuButton = new QPushButton(QString("..."), _buttonPanel);

	QMenu *menu = new QMenu(_menuButton);
	menu->addAction("Remove Analysis", this, SLOT(analysisRemoved()));
	_menuButton->setMenu(menu);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_runButton);
	_buttonPanelLayout->addWidget(_menuButton);
	_buttonPanelLayout->addStretch();

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->move(ui->panelMid->minimumWidth() - _buttonPanel->width(), 0);

	connect(_okButton, SIGNAL(clicked()), this, SLOT(analysisOKed()));
	connect(_runButton, SIGNAL(clicked()), this, SLOT(analysisRunned()));

	connect(ui->splitter, SIGNAL(splitterMoved(int,int)), this, SLOT(splitterMovedHandler(int,int)));

	_analysisMenu = new QMenu(this);
	connect(_analysisMenu, SIGNAL(aboutToHide()), this, SLOT(menuHidding()));

	updateUIFromOptions();

	ui->panelMid->hide();

	_tableViewWidthBeforeOptionsMadeVisible = -1;

	QUrl userGuide = QUrl::fromLocalFile(AppDirs::help() + "/index.html");
	ui->webViewHelp->setUrl(userGuide);
	connect(ui->webViewHelp, SIGNAL(loadFinished(bool)), this, SLOT(helpFirstLoaded(bool)));
	ui->panelHelp->hide();

	try {

		_log = new ActivityLog();
		_log->log("Application Start");

		ui->backStage->setLog(_log);
		_engineSync->setLog(_log);

		_log->flushLogToServer();

		QTimer *timer = new QTimer(this);
		timer->setInterval(30000);
		connect(timer, SIGNAL(timeout()), _log, SLOT(flushLogToServer()));
		timer->start();
	}
	catch (std::runtime_error &e)
	{
		_log = NULL;
		_fatalError = tq(e.what());
		QTimer::singleShot(0, this, SLOT(fatalError()));
	}

	setAcceptDrops(true);
}

void MainWindow::open(QString filename)
{
	_openedUsingArgs = true;
	if (_resultsViewLoaded)
		dataSetSelected(filename);
	else
		_openOnLoadFilename = filename;
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
	if (_isClosed)
	{
		// some times on osx we get two events
		event->accept();
		return;
	}

	bool isSaving = false;
	bool cancel = closeRequestCheck(isSaving);
	if (cancel)
	{
		event->ignore();
	}
	else
	{
		if (isSaving)
			event->ignore(); // to circumvent a race condition between the saving thread and the closing of the main window cleaning up temp files
		else
			event->accept();

		_isClosed = true;
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
	ui->backStage->openFile();
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

QString escapeJavascriptString(const QString &str)
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

void MainWindow::analysisNotesLoadedHandler(Analysis *analysis)
{
	QString results = tq(analysis->notes().toStyledString());

	results = escapeJavascriptString(results);
	results = "window.loadNotes(" + QString::number(analysis->id()) + ", JSON.parse('" + results + "'));";
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
	analysisJson["notes"] = analysis->notes();
	QString results = tq(analysisJson.toStyledString());

	results = escapeJavascriptString(results);
	results = "window.analysisChanged(JSON.parse('" + results + "'));";

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript(results);

	if (_package->isLoaded())
		_package->setModified(true);
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
		Options *options = analysis->options();
		DataSet *dataSet = _package->dataSet;
		_currentOptionsWidget->bindTo(options, dataSet);

		connect(_currentOptionsWidget, SIGNAL(illegalChanged()), this, SLOT(illegalOptionStateChanged()));
		illegalOptionStateChanged();

		_currentOptionsWidget->show();
		ui->optionsContentAreaLayout->addWidget(_currentOptionsWidget, 0, 0, Qt::AlignLeft | Qt::AlignTop);

		if (ui->panelMid->isVisible() == false)
			showOptionsPanel();

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
#ifndef __linux__
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
#endif
	else
	{
		ui->topLevelWidgets->setCurrentIndex(1);
		ui->ribbon->setCurrentIndex(index - 1);
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
	if (_package->isLoaded())
	{
		// begin new instance
		QProcess::startDetached(QCoreApplication::applicationFilePath(), QStringList(filename));
	}
	else
	{
		_loader.load(_package, filename);
		_alert->show();
	}

	ui->tabBar->setCurrentIndex(1);
}

bool MainWindow::closeRequestCheck(bool &isSaving)
{
	bool cancel = false;
	isSaving = false;
	if (_package->isModified())
	{
		QString title = windowTitle();
		title.chop(1);
		QMessageBox::StandardButton reply = QMessageBox::warning(this, "Save Workspace?", QString("Save changes to workspace \"") + title +  QString("\" before closing?\n\nYour changes will be lost if you don't save them."), QMessageBox::Save|QMessageBox::Discard|QMessageBox::Cancel);

		if (reply == QMessageBox::Save)
		{
			isSaving = ui->backStage->save();
			cancel = !isSaving;
		}
		else if (reply == QMessageBox::Cancel)
			cancel = true;
	}
	return cancel;
}

void MainWindow::dataSetCloseRequested()
{
	bool isSaving = false;
	if (_dataSetClosing || !closeRequestCheck(isSaving))
	{
		if (isSaving)
		{
			_dataSetClosing = true;
			return;
		}

		closeCurrentOptionsWidget();
		hideOptionsPanel();
		_tableModel->clearDataSet();
		_loader.free(_package->dataSet);
		_package->reset();
		updateMenuEnabledDisabledStatus();
		ui->backStage->setFileLoaded(false, NULL);
		ui->webViewResults->reload();
		_dataSetClosing = false;
		setWindowTitle("JASP");
		_inited = false;
	}
}

void MainWindow::saveComplete(const QString &name)
{
	_alert->hide();

	_package->setModified(false);

	setWindowTitle(name);

	if (_isClosed)
		this->close();
	else if (_dataSetClosing)
		dataSetCloseRequested();
	else
		ui->tabBar->setCurrentIndex(1);
}

void MainWindow::dataSetLoaded(const QString &dataSetName, DataSetPackage *package, const QString &filename)
{
	setWindowTitle(dataSetName);

	_tableModel->setDataSet(package->dataSet);
	ui->backStage->setFileLoaded(true, filename);
	_analyses->clear();

	ui->tableView->horizontalHeader()->setResizeContentsPrecision(50);
	ui->tableView->horizontalHeader()->resizeSections(QHeaderView::ResizeToContents);

	_alert->hide();

	if (_inited == false)
	{
		ui->webViewResults->page()->mainFrame()->addToJavaScriptWindowObject("jasp", this);
		_inited = true;
	}

	if (package->hasAnalyses)
	{
		bool errorFound = false;

		int corruptAnalyses = 0;
		stringstream errorMsg;
		stringstream corruptionStrings;
		Json::Value analysesData = package->analysesData;
		if (!analysesData.isArray() || analysesData.isNull())
		{
			errorFound = true;
			errorMsg << "An error has been detected and analyses could not be loaded.";
		}
		else
		{
			for (Json::ValueIterator iter = analysesData.begin(); iter != analysesData.end(); iter++)
			{
				try
				{
					QString name = QString();
					Json::Value &analysisData = *iter;

					name = QString::fromStdString(analysisData["name"].asString());
					int id = analysisData["id"].asInt();

					Json::Value &optionsJson = analysisData["options"];
					Json::Value &resultsJson = analysisData["results"];
					Json::Value &notesJson = analysisData["notes"];

					Analysis::Status status = Analysis::parseStatus(analysisData["status"].asString());

					Analysis *analysis = _analyses->create(name, id, &optionsJson, status);

					analysis->setNotes(notesJson);
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

		if (errorFound)
			QMessageBox::warning(this, "", tq(errorMsg.str()));
	}

	if ( ! package->warningMessage.empty())
		QMessageBox::warning(this, "", tq(package->warningMessage));

	package->setLoaded();
	updateMenuEnabledDisabledStatus();

	if (_engineSync->engineStarted() == false)
		_engineSync->start();
}


void MainWindow::dataSetLoadFailed(const QString &message)
{
	_alert->hide();

	if (_package->dataSet != NULL)
		_loader.free(_package->dataSet);
	_package->reset();

	QMessageBox::warning(this, "", "Unable to open file.\n\n" + message);

	if (_openedUsingArgs)
		close();
}

void MainWindow::saveFailed(const QString &message)
{
	_alert->hide();
	_isClosed = false;
	_dataSetClosing = false;

	QMessageBox::warning(this, "", "Unable to save file.\n\n" + message);
}

void MainWindow::updateMenuEnabledDisabledStatus()
{
	bool enable = _package->isLoaded();

	ui->ribbonAnalysis->setEnabled(enable);
	ui->ribbonSEM->setEnabled(enable);
	ui->ribbonR11tLearn->setEnabled(enable);
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

}

void MainWindow::resultsPageLoaded(bool success)
{
	// clear history, to prevent backspace from going 'back'
	ui->webViewResults->history()->clear();

	if (success)
	{
		QVariant ppiv = ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.getPPI()");

		bool success;
		int ppi = ppiv.toInt(&success);

		if (success)
			_engineSync->setPPI(ppi);

		if (!_openOnLoadFilename.isEmpty())
			dataSetSelected(_openOnLoadFilename);

		QString version = tq(AppInfo::version.asString());
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.setAppVersion('" + version + "')");

		_resultsViewLoaded = true;
	}
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
	catch (std::runtime_error& e)
	{
		_fatalError = tq(e.what());
		fatalError();
	}
}

void MainWindow::saveSelected(const QString &filename)
{
	if (_analyses->count() > 0)
	{
		_package->setWaitingForReady();

		getAnalysesNotes();
		ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.exportHTML('%PREVIEW%');");

		Json::Value analysesData = Json::arrayValue;
		for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
		{
			Analysis *analysis = *itr;
			if (analysis != NULL && analysis->isVisible())
			{
				Json::Value analysisData = analysis->asJSON();
				analysisData["options"] = analysis->options()->asJSON();
				analysisData["notes"] = analysis->notes();
				analysesData.append(analysisData);
			}
		}

		_package->analysesData = analysesData;


		_package->hasAnalyses = true;
	}

	_loader.save(filename, _package);
	_alert->show();
}

void MainWindow::saveTextToFileHandler(const QString &filename, const QString &data)
{
	if (filename == "%PREVIEW%")
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

void MainWindow::analysisRemoved()
{
	removeAnalysis(_currentAnalysis);
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

void MainWindow::showAnalysesMenuHandler(QString options)
{
	Json::Value menuOptions;

	Json::Reader parser;
	parser.parse(fq(options), menuOptions);

	QIcon _copyIcon = QIcon(":/icons/copy.png");
	QIcon _citeIcon = QIcon(":/icons/cite.png");

	_analysisMenu->clear();

	QString objName = tq(menuOptions["objectName"].asString());

	if (menuOptions["hasCopy"].asBool())
		_analysisMenu->addAction(_copyIcon, "Copy " + objName, this, SLOT(copySelected()));

	if (menuOptions["hasCite"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction(_citeIcon, "Copy Citations", this, SLOT(citeSelected()));
	}

	if (menuOptions["hasNotes"].asBool())
	{
		_analysisMenu->addSeparator();

		Json::Value noteOptions = menuOptions["noteOptions"];

		for (Json::ValueIterator iter = noteOptions.begin(); iter != noteOptions.end(); iter++)
		{
			Json::Value noteOption = *iter;
			QAction *a1 = _analysisMenu->addAction(tq("Notes " + noteOption["menuText"].asString()), this, SLOT(noteSelected(bool)));
			a1->setCheckable(true);
			a1->setData(tq(noteOption["key"].asString()));
			a1->setChecked(noteOption["visible"].asBool());
		}
	}

	if (menuOptions["hasRemove"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Remove " + objName, this, SLOT(removeSelected()));
	}

	QPoint point = ui->webViewResults->mapToGlobal(QPoint(menuOptions["rX"].asInt(), menuOptions["rY"].asInt()));

	_analysisMenu->move(point);
	_analysisMenu->show();
}



void MainWindow::removeAnalysisRequestHandler(int id)
{
	Analysis *analysis = _analyses->get(id);
	removeAnalysis(analysis);
}

void MainWindow::getAnalysesNotes()
{
	QVariant notesData = ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.getAnalysesNotes();");

	Json::Value notes;
	Json::Reader parser;
	parser.parse(fq(notesData.toString()), notes);

	for (Json::Value::iterator iter = notes.begin(); iter != notes.end(); iter++)
	{
		Json::Value &notesObj = *iter;

		Analysis *analysis = _analyses->get(notesObj["id"].asInt());

		Json::Value &analysisNotes = notesObj["notes"];

		analysis->setNotes(analysisNotes, true);
	}
}

void MainWindow::updateNoteHandler(int id, QString key)
{
	_package->setModified(true);
}

void MainWindow::removeSelected()
{

	ui->webViewResults->page()->mainFrame()->evaluateJavaScript("window.removeMenuClicked();");

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

void MainWindow::noteSelected(bool checked)
{
	QAction *action = (QAction *)this->sender();
	QString noteKey = action->data().toString();
	QString call = QString("window.notesMenuClicked('%1', %2);").arg(noteKey).arg(checked ? "true" : "false");

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
