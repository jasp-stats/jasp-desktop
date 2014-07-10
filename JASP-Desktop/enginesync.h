#ifndef ENGINESYNC_H
#define ENGINESYNC_H

#ifdef __APPLE__
#include <semaphore.h>
#else
#include <boost/interprocess/sync/named_semaphore.hpp>
#endif

#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include <QProcess>
#include <QTimer>
#include <vector>

#include "options/options.h"
#include "analysis.h"
#include "analyses.h"
#include "ipcchannel.h"

class EngineSync : public QObject
{
	Q_OBJECT

public:
	EngineSync(Analyses *analyses, QObject *parent);
	~EngineSync();

	void start();

	bool engineStarted();

signals:

	void updateReceived(QString data);
	void engineTerminated();

private:

	Analyses *_analyses;
	bool _engineStarted;

	std::vector<QProcess *> _slaveProcesses;
	std::vector<IPCChannel *> _channels;
	std::vector<Analysis *> _analysesInProgress;

	IPCChannel *nextFreeProcess(Analysis *analysis);
	void sendToProcess(int processNo, Analysis *analysis);

	QTimer *_timer;

	void sendMessages();
	void startSlaveProcess(int no);

	std::string _memoryName;

private slots:

	void process();

	void subProcessStandardOutput();
	void subProcessStandardError();
	void subProcessStarted();
	void subProcessError(QProcess::ProcessError error);
	void subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // ENGINESYNC_H
