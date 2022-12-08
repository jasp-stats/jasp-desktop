#include "rcommander.h"
#include "engine/enginesync.h"
#include "mainwindow.h"
#include "modules/dynamicmodules.h"

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

bool RCommander::parseAnalysisCode(const QString& code, QString& moduleName, QString& analysisName) const
{
	// Check whether the code starts with '<moduleName>::<analysisName>(...)'
	QString codeTrimmed = code.trimmed();
	int startBracket = codeTrimmed.indexOf('(');

	if (startBracket < 0) return false;
	if (codeTrimmed.indexOf(')', startBracket) < 0 ) return false;

	QStringList analysisParts = codeTrimmed.mid(0, startBracket).split("::");
	if (analysisParts.length() != 2) return false;

	moduleName = analysisParts[0];
	analysisName = analysisParts[1];

	Modules::DynamicModule* module = Modules::DynamicModules::dynMods()->dynamicModule(moduleName);
	if (!module) return false;

	for (const Modules::AnalysisEntry* entry : module->menu())
	{
		if (entry->isAnalysis() && entry->isEnabled() && entry->hasWrapper() && entry->function() == fq(analysisName))
			return true;
	}

	return false;
}

bool RCommander::addAnalysis(const QString &code)
{
	if(running() || !_engine->idle() || code == "")
		return false;

	setRunning(true);

	QString moduleName, analysisName;
	if (parseAnalysisCode(code, moduleName, analysisName))
	{
		Analysis* analysis = Analyses::analyses()->createAnalysis(moduleName, analysisName);
		if (analysis)
			analysis->sendRScript(code, AnalysisForm::rSyntaxControlName, false);
		else
			appendToOutput("> " + tr("Analysis not found"));
	}
	else
		appendToOutput("> " + tr("Not an analysis function: <module name>::<analysis name>(...)"));

	setRunning(false);
	setLastCmd(code);

	appendToOutput("> " + code);

	return true;
}

void RCommander::setIsAnalysisCode(bool isAnalysisCode)
{
	if (_isAnalysisCode == isAnalysisCode)
		return;

	_isAnalysisCode = isAnalysisCode;

	emit isAnalysisCodeChanged(_isAnalysisCode);
}

void RCommander::checkRCode(const QString &code)
{
	QString moduleName, analysisName;
	setIsAnalysisCode(parseAnalysisCode(code, moduleName, analysisName));
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
