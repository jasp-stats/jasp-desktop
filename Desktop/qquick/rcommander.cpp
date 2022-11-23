#include "rcommander.h"
#include "engine/enginesync.h"
#include "mainwindow.h"

RCommander * RCommander::_lastCommander = nullptr;

RCommander::RCommander()
{
	_lastCommander = this;

	_engine = EngineSync::singleton()->createRCmdEngine();

	connect(_engine, &EngineRepresentation::rCodeReturned,		this, &RCommander::rCodeReturned);
	connect(_engine, &EngineRepresentation::rCodeReturnedLog,	this, &RCommander::rCodeReturnedLog);

	_scrollTimer = new QTimer(this);
	_scrollTimer->setInterval(50);
	_scrollTimer->setSingleShot(true);

	connect(_scrollTimer,	&QTimer::timeout, this, &RCommander::scrollDown);
	_scrollTimer->start();

	connect(MainWindow::singleton(), &MainWindow::closeWindows, this, &RCommander::closeWindow);
}

RCommander::~RCommander()
{
	if(_engine && EngineSync::singleton())
		try {	EngineSync::singleton()->destroyEngine(_engine); }
		catch(...) {}

	_engine = nullptr;

	if(_lastCommander == this)
		_lastCommander = nullptr;
}

void RCommander::makeActive()
{
	emit _lastCommander->activated();
}

bool RCommander::runCode(const QString & code)
{
	if(running() || !_engine->idle() || code == "")
		return false;

	setRunning(true);

	_engine->runScriptOnProcess(code);

	setLastCmd(code);

	appendToOutput("> " + code);

	return true;
}

bool RCommander::addAnalysis(const QString &code)
{
	if(running() || !_engine->idle() || code == "")
		return false;

	setRunning(true);

	// Temporary solution: check whether the code starts with '<moduleName>::<analysisNameWrapper>(...'
	QString codeTrimmed = code.trimmed();
	QStringList analysisParts = codeTrimmed.mid(0, codeTrimmed.indexOf("(")).split("::");

	if (analysisParts.length() == 2)
	{
		QString moduleName = analysisParts[0], analysisName = analysisParts[1];
		if (analysisName.endsWith("Wrapper"))
			analysisName = analysisName.mid(0, analysisName.lastIndexOf("Wrapper"));

		if (!moduleName.isEmpty() && !analysisName.isEmpty())
		{
			Analysis* analysis = Analyses::analyses()->createAnalysis(moduleName, analysisName);
			if (analysis)
				analysis->sendRScript(code, AnalysisForm::rSyntaxControlName, false);
		}
		setRunning(false);
	}
	else
		_engine->runScriptOnProcess(code);

	setLastCmd(code);

	appendToOutput("> " + code);

	return true;
}

void RCommander::rCodeReturned(const QString & result, int, bool)
{
	appendToOutput(result);

	setRunning(false);
}

void RCommander::rCodeReturnedLog(const QString & log, bool)
{
	appendToOutput(log);

	setRunning(false);
}

void RCommander::setRunning(bool running)
{
	if (_running == running)
		return;

	_running = running;
	emit runningChanged(_running);
}

void RCommander::setLastCmd(QString lastCmd)
{
	if (_lastCmd == lastCmd)
		return;

	_lastCmd = lastCmd;
	emit lastCmdChanged(_lastCmd);
}

void RCommander::countDownToScroll()
{
	_scrollTimer->start();
}

void RCommander::setOutput(const QString & output)
{
	if (_output == output)
		return;

	_output = output;
	emit outputChanged(_output);

	_scrollTimer->start(); //I had some trouble getting the scrolldown thing to work easily, this workaround seems good.
}
