#ifndef PYTHONSCRIPTRUNNER_H
#define PYTHONSCRIPTRUNNER_H

#include <QObject>
#include <QProcess>
#include <QStringDecoder>
#include <QTemporaryDir>
#include <QVariant>
#include <memory>

class JaspRpcServer;

/// Runs a Python script that calls JASP through the jasp module (Resources/python/jasp), for the Python window.
///
/// Every run gets its own JaspRpcServer (JaspRpcServer::startForScript): its calls count as the script's, not the
/// AI's, and it is gone again when the run ends. The script runs as its own process, with the url and token of that
/// server in JASP_RPC_URL and JASP_RPC_TOKEN, so the jasp module connects as it is imported. What it prints, and any
/// error Python reports, is collected in output as it comes.
class PythonScriptRunner : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString	output		READ output									NOTIFY outputChanged		)
	Q_PROPERTY(bool		running		READ running								NOTIFY runningChanged		)
	Q_PROPERTY(QString	interpreter	READ interpreter	WRITE setInterpreter	NOTIFY interpreterChanged	)

public:
	explicit				PythonScriptRunner(QObject * parent = nullptr);
							~PythonScriptRunner() override;

	const QString		&	output()		const	{ return _output;				}
	bool					running()		const	{ return _process != nullptr;	}

	/// The Python to run scripts with. When empty, findInterpreter() picks one.
	const QString		&	interpreter()	const	{ return _interpreter;			}

	/// Where the jasp module is, Resources/python unless set otherwise.
	const QString		&	moduleDir()		const	{ return _moduleDir;			}
	void					setModuleDir(const QString & moduleDir)	{ _moduleDir = moduleDir; }

	/// A Python 3 installed on this computer, or "" when there is none.
	/// Never one of the placeholders that start an installer instead of Python:
	/// /usr/bin/python3 on macOS without the Command Line Tools, or the python.exe of
	/// WindowsApps that opens the Microsoft Store.
	static QString			findInterpreter();

public slots:
	/// Runs code as a Python script. False when it cannot start, with the reason in output,
	/// and while another script still runs: there is one at a time.
	bool					run(const QString & code);

	/// Ends the running script at once.
	void					stop();

	/// The contents of a script file, or nothing (undefined in QML) with the reason in output when it cannot be read.
	QVariant				readScript(const QString & path);

	/// Writes a script to a file, UTF-8 as Python reads it. False, with the reason in output, when that fails.
	bool					writeScript(const QString & path, const QString & code);

	void					clearOutput();
	void					setInterpreter(const QString & interpreter);

signals:
	void					outputChanged();
	void					runningChanged();
	void					interpreterChanged();

	/// A run has ended: with Python's exit code, or -1 when it was stopped or could not start.
	void					finished(int exitCode);

private:
	void					readOutput();
	void					appendOutput(const QString & text);
	void					appendMessage(const QString & message); ///< A line of JASP's own, not the script's
	void					ended(int exitCode);

	QString							_output,
									_interpreter,
									_moduleDir;
	QProcess					*	_process	= nullptr;
	std::unique_ptr<JaspRpcServer>	_server;
	std::unique_ptr<QTemporaryDir>	_scriptDir;
	QStringDecoder					_decoder	= QStringDecoder(QStringDecoder::Utf8); ///< Keeps a character split over two reads whole
	bool							_stopped	= false;
};

#endif // PYTHONSCRIPTRUNNER_H
