//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

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
#include "activitylog.h"

/* EngineSync is responsible for launching the background
 * processes, scheduling analyses, and for sending and
 * receiving communications with the running analyses.
 * It keeps track of which analyses are executing on
 * which background process.
 */

class EngineSync : public QObject
{
	Q_OBJECT

public:
	EngineSync(Analyses *analyses, QObject *parent);
	~EngineSync();

	void start();

	bool engineStarted();
	void setLog(ActivityLog *log);

	void setPPI(int ppi);

signals:

	void engineTerminated();

private:

	Analyses *_analyses;
	bool _engineStarted;
	ActivityLog *_log;

	int _ppi;

	std::vector<QProcess *> _slaveProcesses;
	std::vector<IPCChannel *> _channels;
	std::vector<Analysis *> _analysesInProgress;

	IPCChannel *nextFreeProcess(Analysis *analysis);
	void sendToProcess(int processNo, Analysis *analysis);

	void startSlaveProcess(int no);

	std::string _memoryName;
	std::string _engineInfo;

private slots:

	void sendMessages();
	void deleteOrphanedTempFiles();
	void heartbeatTempFiles();

	void process();

	void subProcessStandardOutput();
	void subProcessStandardError();
	void subProcessStarted();
	void subProcessError(QProcess::ProcessError error);
	void subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // ENGINESYNC_H
