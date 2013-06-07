#ifndef ENGINESYNC_H
#define ENGINESYNC_H

#ifdef __APPLE__
#include <semaphore.h>
#else
#include <boost/interprocess/sync/named_semaphore.hpp>
#endif

#include <boost/interprocess/ipc/message_queue.hpp>

#include <QProcess>
#include <QTimer>
#include <vector>

#include "options.h"
#include "analysis.h"
#include "analyses.h"


class EngineSync : public QObject
{
	Q_OBJECT

public:
	EngineSync(Analyses *analyses, QObject *parent);
	~EngineSync();

signals:

	void updateReceived(QString data);

private:

	void analysisAddedHandler(Analysis *analysis);
	void analysisOptionsChangedHandler(Analysis *analysis);
	void send(Json::Value json);
	//void analysisChanged();

	QProcess *_process;
	QString _engineExe;
	int _maxProcesses;
	Analyses *_analyses;

	QTimer *_timer;

    boost::interprocess::message_queue* _messageQueueIn;
    boost::interprocess::message_queue* _messageQueueOut;

#ifdef __APPLE__
    sem_t* _semaphoreIn;
    sem_t* _semaphoreOut;
#else
    boost::interprocess::named_semaphore* _semaphoreIn;
    boost::interprocess::named_semaphore* _semaphoreOut;
#endif

	void processAnalyses();

#define BUFFER_SIZE 1048576 // 1 meg

	char _buffer[BUFFER_SIZE];


private slots:

	void checkForMessages();

	void subProcessStandardOutput();
	void subProcessStandardError();
	void subProcessStarted();
	void subProcessError(QProcess::ProcessError error);
	void subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // ENGINESYNC_H
