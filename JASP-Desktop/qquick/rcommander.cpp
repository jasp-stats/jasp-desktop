#include "rcommander.h"
#include "engine/enginesync.h"
#include "mainwindow.h"

RCommander::RCommander()
{
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
		try {	EngineSync::singleton()->destroyRCmdEngine(_engine); }
		catch(...) {}

	_engine = nullptr;
}

bool RCommander::runCode(const QString & code)
{
	if(running())			return false;
	if(!_engine->idle())	return false;

	setRunning(true);

	_engine->runScriptOnProcess(code);

	setLastCmd(code);

	appendToOutput("> " + code);

	return true;
}

void RCommander::rCodeReturned(const QString & result, int)
{
	appendToOutput(result);

	setRunning(false);
}

void RCommander::rCodeReturnedLog(const QString & log)
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
