#ifndef RCOMMANDER_H
#define RCOMMANDER_H

#include <QQuickItem>
#include <QFileInfo>

class EngineRepresentation;

///
/// Interface for the R-commander. Keeps a log, and handles communication with the engine that belongs to it.
class RCommander : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QString	output			READ output				WRITE setOutput		NOTIFY outputChanged			)
	Q_PROPERTY(bool		running			READ running			WRITE setRunning	NOTIFY runningChanged			)
	Q_PROPERTY(bool		isAnalysisCode	READ isAnalysisCode							NOTIFY isAnalysisCodeChanged	)
	Q_PROPERTY(QString	lastCmd			READ lastCmd			WRITE setLastCmd	NOTIFY lastCmdChanged			)

public:
	RCommander();
	~RCommander();

	QString output()			const { return _output;			}
	bool	running()			const { return _running;		}
	QString lastCmd()			const { return _lastCmd;		}
	bool	isAnalysisCode()	const { return _isAnalysisCode; }

	static	bool opened()		{ return _lastCommander; }
	static	void makeActive();

public slots:
	bool runCode(			const QString & code);
	bool addAnalysis(		const QString & code);
	void checkRCode(		const QString & code);
	void setOutput(			const QString & output);
	void rCodeReturned(		const QString & result, int requestId, bool hasError);
	void rCodeReturnedLog(	const QString & log, bool hasError);
	void clearOutput()														{ setOutput(""); }
	void appendToOutput(const QString & toAppend, QString separator = "\n") { setOutput(output() + separator + toAppend); }
	void setRunning(bool running);
	void setLastCmd(QString lastCmd);
	void countDownToScroll();

signals:
	void outputChanged(QString output);
	void runningChanged(bool running);
	void isAnalysisCodeChanged(bool changed);
	void lastCmdChanged(QString lastCmd);
	void scrollDown();
	void closeWindow();
	void activated();

private:
	bool parseAnalysisCode(const QString& code, QString& moduleName, QString& analysisName) const;
	void setIsAnalysisCode(bool isAnalysisCode);

	static RCommander		*	_lastCommander;
	QString						_output			= "", //Set in qml to have it be translatable
								_lastCmd		= "";
	EngineRepresentation	*	_engine			= nullptr;
	bool						_running		= false,
								_isAnalysisCode = false;
	QTimer					*	_scrollTimer	= nullptr;
};

#endif // RCOMMANDER_H
