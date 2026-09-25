#include "pythonscriptrunner.h"
#include "rpc/jasprpcserver.h"
#include "rpc/jasprpcdispatcher.h"
#include "dirs.h"

#include <QCollator>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QStandardPaths>
#include <algorithm>

PythonScriptRunner::PythonScriptRunner(QObject * parent)
	: QObject(parent), _moduleDir(QString::fromStdString(Dirs::resourcesDir()) + "python")
{
}

PythonScriptRunner::~PythonScriptRunner()
{
	if (_process)
	{
		_process->disconnect(this);
		_process->kill();
		_process->waitForFinished(2000);
	}
}

QString PythonScriptRunner::findInterpreter()
{
	QStringList candidates;

#if defined(_WIN32)
	// The python.exe in WindowsApps is only a placeholder that opens the Microsoft Store
	for (const QString & onPath : { QStandardPaths::findExecutable("python"), QStandardPaths::findExecutable("python3") })
		if (!QDir::fromNativeSeparators(onPath).contains("/WindowsApps/", Qt::CaseInsensitive))
			candidates << onPath;

	// Where the installers of python.org put it, newest first
	QCollator byVersion;
	byVersion.setNumericMode(true);

	for (const QString & root : { QDir::fromNativeSeparators(qEnvironmentVariable("LOCALAPPDATA")) + "/Programs/Python",
								  QDir::fromNativeSeparators(qEnvironmentVariable("ProgramFiles")) })
	{
		QStringList versions = QDir(root).entryList({ "Python3*" }, QDir::Dirs);
		std::sort(versions.begin(), versions.end(), [&](const QString & a, const QString & b) { return byVersion.compare(a, b) > 0; });

		for (const QString & version : versions)
			candidates << root + "/" + version + "/python.exe";
	}
#elif defined(__APPLE__)
	// Homebrew (Apple silicon, then Intel) and python.org first: an app started from the Finder has only /usr/bin:/bin:/usr/sbin:/sbin on its path
	candidates << "/opt/homebrew/bin/python3" << "/usr/local/bin/python3" << "/Library/Frameworks/Python.framework/Versions/Current/bin/python3";

	// /usr/bin/python3 asks to install the Command Line Tools when they are not there, so rather the Python it stands for when they are
	const QString onPath = QStandardPaths::findExecutable("python3");
	if (onPath != "/usr/bin/python3")
		candidates << onPath;

	candidates << "/Library/Developer/CommandLineTools/usr/bin/python3" << "/Applications/Xcode.app/Contents/Developer/usr/bin/python3";
#else
	candidates << QStandardPaths::findExecutable("python3") << "/usr/bin/python3";
#endif

	for (const QString & candidate : candidates)
		if (!candidate.isEmpty() && QFileInfo(candidate).isExecutable())
			return candidate;

	return "";
}

bool PythonScriptRunner::run(const QString & code)
{
	if (_process)
		return false;

	const QString python = _interpreter.isEmpty() ? findInterpreter() : _interpreter;

	if (python.isEmpty())
	{
		appendMessage(tr("No Python 3 was found on this computer. Install Python 3.8 or later, or give the path to one."));
		return false;
	}

	if (!QFileInfo(python).isExecutable())
	{
		appendMessage(tr("There is no Python to run at %1.").arg(python));
		return false;
	}

	if (JaspRpcDispatcher * dispatcher = JaspRpcDispatcher::singleton())
		_server = JaspRpcServer::startForScript(*dispatcher);

	if (!_server)
	{
		appendMessage(tr("JASP could not open a connection for the script."));
		return false;
	}

	_scriptDir = std::make_unique<QTemporaryDir>();
	QFile script(_scriptDir->filePath("script.py"));

	if (!_scriptDir->isValid() || !script.open(QIODevice::WriteOnly))
	{
		appendMessage(tr("JASP could not write the script to a temporary file."));
		_server.reset();
		_scriptDir.reset();
		return false;
	}

	script.write(code.toUtf8());
	script.close();

	QProcessEnvironment	environment	= QProcessEnvironment::systemEnvironment();
	const QString		pythonPath	= environment.value("PYTHONPATH");

	environment.insert("PYTHONPATH",				pythonPath.isEmpty() ? _moduleDir : _moduleDir + QDir::listSeparator() + pythonPath); // JASP's own jasp module first
	environment.insert("PYTHONDONTWRITEBYTECODE",	"1");		// No __pycache__ in JASP's folders: on macOS they are inside a signed app bundle
	environment.insert("PYTHONUNBUFFERED",			"1");		// What the script prints shows right away, not once it ends
	environment.insert("PYTHONIOENCODING",			"utf-8");	// Also on Windows, where Python would otherwise write in the console's code page
	environment.insert("JASP_RPC_URL",				_server->url());
	environment.insert("JASP_RPC_TOKEN",			_server->token());

	_stopped = false;
	_decoder.resetState();

	_process = new QProcess(this);
	_process->setProcessEnvironment(environment);
	_process->setProcessChannelMode(QProcess::MergedChannels); // A traceback then stays in its place after what was printed before it
	_process->setWorkingDirectory(QDir::homePath());

	connect(_process, &QProcess::readyReadStandardOutput, this, &PythonScriptRunner::readOutput);

	connect(_process, &QProcess::finished, this, [this](int exitCode, QProcess::ExitStatus status)
	{
		ended(status == QProcess::NormalExit && !_stopped ? exitCode : -1);
	});

	connect(_process, &QProcess::errorOccurred, this, [this, python](QProcess::ProcessError error)
	{
		if (error == QProcess::FailedToStart && _process) // Then there is no finished()
		{
			appendMessage(tr("Python could not be started from %1: %2").arg(python, _process->errorString()));
			ended(-1);
		}
	});

	emit runningChanged();

	_process->start(python, { script.fileName() });

	return _process != nullptr; // Already gone when it failed to start right away
}

void PythonScriptRunner::stop()
{
	if (!_process)
		return;

	_stopped = true;
	_process->kill(); // At once: a script can take as long as it likes to end by itself
}

void PythonScriptRunner::clearOutput()
{
	_output.clear();
	emit outputChanged();
}

void PythonScriptRunner::setInterpreter(const QString & interpreter)
{
	if (_interpreter == interpreter)
		return;

	_interpreter = interpreter;
	emit interpreterChanged();
}

void PythonScriptRunner::readOutput()
{
	if (_process)
		appendOutput(_decoder.decode(_process->readAllStandardOutput()));
}

void PythonScriptRunner::appendOutput(const QString & text)
{
	if (text.isEmpty())
		return;

	_output += text;

	// A script that keeps printing should not take all memory: keep the last part
	constexpr qsizetype maxOutput = 1000000;
	if (_output.size() > maxOutput)
		_output = _output.right(maxOutput);

	emit outputChanged();
}

void PythonScriptRunner::appendMessage(const QString & message)
{
	appendOutput((_output.isEmpty() || _output.endsWith('\n') ? "" : "\n") + message + "\n");
}

void PythonScriptRunner::ended(int exitCode)
{
	if (!_process)
		return;

	readOutput();

	if (_stopped)
		appendMessage(tr("The script was stopped."));

	_process->deleteLater();
	_process = nullptr;
	_server.reset();	// The script's connection to JASP lives only as long as its run
	_scriptDir.reset();

	emit runningChanged();
	emit finished(exitCode);
}
