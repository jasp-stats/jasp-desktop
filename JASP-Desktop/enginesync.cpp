#include "enginesync.h"

#include <QApplication>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QDebug>

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/foreach.hpp>

#include "lib_json/json.h"

#include "process.h"
#include "common.h"

using namespace boost::interprocess;
using namespace std;

EngineSync::EngineSync(Analyses *analyses, QObject *parent = 0)
	: QObject(parent)
{
	_analyses = analyses;
	_engineStarted = false;

	_analyses->analysisAdded.connect(boost::bind(&EngineSync::sendMessages, this));
	_analyses->analysisOptionsChanged.connect(boost::bind(&EngineSync::sendMessages, this));

}

EngineSync::~EngineSync()
{
	for (int i = 0; i < _slaveProcesses.size(); i++)
	{
		_slaveProcesses[i]->terminate();
		_slaveProcesses[i]->kill();
	}

	shared_memory_object::remove(_memoryName.c_str());
}

void EngineSync::start()
{
	if (_engineStarted)
		return;

	_engineStarted = true;

	_timer = new QTimer(this);
	connect(_timer, SIGNAL(timeout()), this, SLOT(process()));
	_timer->start(50);

	try {

		unsigned long pid = Process::currentPID();

		stringstream ss;
		ss << "JASP-IPC-" << pid;

		_memoryName = ss.str();

		_channels.push_back(new IPCChannel(_memoryName, 0));

#ifdef QT_NO_DEBUG
		_channels.push_back(new IPCChannel(_memoryName, 1));
		_channels.push_back(new IPCChannel(_memoryName, 2));
		_channels.push_back(new IPCChannel(_memoryName, 3));
#endif

	}
	catch (interprocess_exception e)
	{
		qDebug() << "interprocess exception! " << e.what() << "\n";
		throw e;
	}

	for (uint i = 0; i < _channels.size(); i++)
	{
		_analysesInProgress.push_back(NULL);
		startSlaveProcess(i);
	}
}

bool EngineSync::engineStarted()
{
	return _engineStarted;
}

void EngineSync::sendToProcess(int processNo, Analysis *analysis)
{
#ifdef QT_DEBUG
	std::cout << "send " << analysis->id() << " to process " << processNo << "\n";
	std::cout.flush();
#endif

	bool init;

	if (analysis->status() == Analysis::Empty)
	{
		init = true;
		analysis->setStatus(Analysis::Initing);
	}
	else
	{
		init = false;
		analysis->setStatus(Analysis::Running);
	}

	_analysesInProgress[processNo] = analysis;

	Json::Value json = Json::Value(Json::objectValue);

	json["id"] = analysis->id();
	json["name"] = analysis->name();
	json["options"] = analysis->options()->asJSON();
	json["perform"] = (init ? "init" : "run");

	string str = json.toStyledString();

	_channels[processNo]->send(str);

#ifndef QT_NO_DEBUG
	cout << str << "\n";
	cout.flush();
#endif

}

void EngineSync::process()
{
	for (int i = 0; i < _channels.size(); i++)
	{
		Analysis *analysis = _analysesInProgress[i];

		if (analysis == NULL)
			continue;

		IPCChannel *channel = _channels[i];
		string data;

		if (channel->receive(data))
		{
#ifdef QT_DEBUG
			std::cout << "message received\n";
            std::cout << data << "\n";
			std::cout.flush();
#endif

			Json::Reader reader;
			Json::Value json;
			reader.parse(data, json);

			//int id = json.get("id", -1).asInt();
			//bool init = json.get("perform", "init").asString() == "init";
			Json::Value results = json.get("results", Json::nullValue);
			string status = json.get("status", "error").asString();

			if (status == "complete")
			{
				analysis->setStatus(Analysis::Complete);
				analysis->setResults(results);
				_analysesInProgress[i] = NULL;
				sendMessages();
			}
			else if (analysis->status() == Analysis::Running)
			{
				analysis->setResults(results);
			}
			else if (analysis->status() == Analysis::Initing)
			{
				analysis->setStatus(Analysis::Inited);
				analysis->setResults(results);
				_analysesInProgress[i] = NULL;
				sendMessages();
			}
			else
			{
				sendMessages();
			}
		}

	}
}

void EngineSync::sendMessages()
{
#ifdef QT_DEBUG
	std::cout << "send messages\n";
	std::cout.flush();
#endif

	for (int i = 0; i < _analysesInProgress.size(); i++)
	{
		Analysis *analysis = _analysesInProgress[i];
		if (analysis != NULL && analysis->status() == Analysis::Empty)
			sendToProcess(i, analysis);
	}

	for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
	{
		Analysis *analysis = *itr;

		if (analysis->status() == Analysis::Empty)
		{
			bool sent = false;

			for (int i = 0; i < _analysesInProgress.size(); i++)
			{
				if (_analysesInProgress[i] == NULL)
				{
					sendToProcess(i, analysis);
					sent = true;
					break;
				}
			}

			if (sent == false)  // no free processes left
				return;
		}
		else if (analysis->status() == Analysis::Inited)
		{
#ifndef QT_DEBUG
			for (int i = 1; i < _analysesInProgress.size(); i++) // don't perform 'runs' on process 0, only inits.
#else
			for (int i = 0; i < _analysesInProgress.size(); i++)
#endif
			{
				if (_analysesInProgress[i] == NULL)
				{
					sendToProcess(i, analysis);
					break;
				}
			}
		}
	}

}

void EngineSync::startSlaveProcess(int no)
{
	QDir programDir = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
	QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
	QString engineExe = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");

	QStringList args;
	args << QString::number(no);

#ifdef __WIN32__

#if defined(ARCH_32)
#define ARCH_SUBPATH "i386"
#else
#define ARCH_SUBPATH "x64"
#endif

	//env.insert("PATH", programDir.absoluteFilePath("R\\library\\RInside\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\library\\Rcpp\\libs\\" ARCH_SUBPATH) + ";" + programDir.absoluteFilePath("R\\bin\\" ARCH_SUBPATH));
	//env.insert("R_HOME", programDir.absoluteFilePath("R"));

    unsigned long processId = Process::currentPID();
    args << QString::number(processId);

#undef ARCH_SUBPATH

#elif __APPLE__
	env.insert("R_HOME", programDir.absoluteFilePath("../Frameworks/R.framework/Versions/3.1/Resources"));
#else
    //env.insert("LD_LIBRARY_PATH", programDir.absoluteFilePath("R/lib") + ";" + programDir.absoluteFilePath("R/library/RInside/lib") + ";" + programDir.absoluteFilePath("R/library/Rcpp/lib"));
    //env.insert("R_HOME", programDir.absoluteFilePath("R"));
#endif


	QProcess *slave = new QProcess(this);
	slave->setProcessEnvironment(env);
	slave->start(engineExe, args);

	_slaveProcesses.push_back(slave);

	connect(slave, SIGNAL(readyReadStandardOutput()), this, SLOT(subProcessStandardOutput()));
	connect(slave, SIGNAL(readyReadStandardError()), this, SLOT(subProcessStandardError()));
	connect(slave, SIGNAL(error(QProcess::ProcessError)), this, SLOT(subProcessError(QProcess::ProcessError)));
	connect(slave, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(subprocessFinished(int,QProcess::ExitStatus)));
	connect(slave, SIGNAL(started()), this, SLOT(subProcessStarted()));

}

void EngineSync::subProcessStandardOutput()
{
	QProcess *process = qobject_cast<QProcess *>(this->sender());
	QByteArray data = process->readAllStandardOutput();
	qDebug() << QString(data);
}

void EngineSync::subProcessStandardError()
{
	QProcess *process = qobject_cast<QProcess *>(this->sender());
	qDebug() << process->readAllStandardError();
}

void EngineSync::subProcessStarted()
{
	qDebug() << "subprocess started";
}

void EngineSync::subProcessError(QProcess::ProcessError error)
{
	emit engineTerminated();

	qDebug() << "subprocess error" << error;
}

void EngineSync::subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	qDebug() << "subprocess finished" << exitCode;
}

