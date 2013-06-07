#include "enginesync.h"

#include <QApplication>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QDebug>

#include <boost/foreach.hpp>

#include "../JASP-Common/lib_json/json.h"

using namespace boost::interprocess;

EngineSync::EngineSync(Analyses *analyses, QObject *parent = 0)
	: QObject(parent)
{
	_analyses = analyses;

	_maxProcesses = 4;
	_engineExe = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir().absoluteFilePath("JASPEngine");
	_process = new QProcess(parent);

	_analyses->analysisAdded.connect(boost::bind(&EngineSync::analysisAddedHandler, this, _1));
	_analyses->analysisOptionsChanged.connect(boost::bind(&EngineSync::analysisOptionsChangedHandler, this, _1));

	connect(_process, SIGNAL(readyReadStandardOutput()), this, SLOT(subProcessStandardOutput()));
	connect(_process, SIGNAL(readyReadStandardError()), this, SLOT(subProcessStandardError()));
	connect(_process, SIGNAL(error(QProcess::ProcessError)), this, SLOT(subProcessError(QProcess::ProcessError)));
	connect(_process, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(subprocessFinished(int,QProcess::ExitStatus)));
	connect(_process, SIGNAL(started()), this, SLOT(subProcessStarted()));

	_timer = new QTimer(this);
	connect(_timer, SIGNAL(timeout()), this, SLOT(checkForMessages()));
	_timer->start(50);

    try {

        message_queue::remove("JASP_MQ");
        message_queue::remove("JASPEngine_MQ");

        _messageQueueOut = new message_queue(create_only, "JASP_MQ", 4, BUFFER_SIZE);
        _messageQueueIn = new message_queue(create_only, "JASPEngine_MQ", 4, BUFFER_SIZE);

#ifdef __APPLE__
        _semaphoreIn = sem_open("JASPEngine_SM", O_CREAT, S_IWUSR | S_IRGRP | S_IROTH, 0);
        _semaphoreOut = sem_open("JASP_SM", O_CREAT, S_IWUSR | S_IRGRP | S_IROTH, 0);

		if (_semaphoreOut == SEM_FAILED || _semaphoreIn == SEM_FAILED)
			qDebug() << "sempahore not created!";
#else

        named_semaphore::remove("JASPEngine_SM");
        named_semaphore::remove("JASP_SM");

        _semaphoreIn = new named_semaphore(create_only, "JASPEngine_SM", 0);
        _semaphoreOut = new named_semaphore(create_only, "JASP_SM", 0);

#endif

		QDir programDir = QFileInfo( QCoreApplication::applicationFilePath() ).absoluteDir();
		QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
#ifdef __APPLE__
        env.insert("DYLD_LIBRARY_PATH", programDir.absoluteFilePath("R-3.0.0/lib"));
#else
        env.insert("PATH", programDir.absoluteFilePath("R-3.0.0\\library\\RInside\\libs\\i386") + ";" + programDir.absoluteFilePath("R-3.0.0\\library\\Rcpp\\libs\\i386") + ";" + programDir.absoluteFilePath("R-3.0.0\\bin\\i386"));
#endif

        env.insert("R_HOME", programDir.absoluteFilePath("R-3.0.0"));

		_process->setEnvironment(env.toStringList());
		_process->start(_engineExe, QStringList("0"));

    }
    catch (interprocess_exception e)
    {
        qDebug() << "interprocess exception! " << e.what() << "\n";

        throw e;
    }

}

EngineSync::~EngineSync()
{
	_process->terminate();
	_process->kill();
}

/*void EngineSync::addAnalysis(Analysis *analysis)
{
	int id = _nextId++;

	bimap<int, Analysis *>::value_type item(id, analysis);
	_analyses.insert(item);

	initAnalysis(analysis);

	BOOST_FOREACH(AnalysisPart *part, *analysis)
	{
		typedef bimap<int, AnalysisPart *>::value_type pair;

		id = _nextPartId++;
		_analysesParts.insert(pair(id, part));
	}

	//processAnalyses();
}*/


/*void EngineSync::processAnalyses()
{
	try
	{
		if (_currentPart != NULL && _currentPart->isCompleted() == false)
			return;

		if (_currentAnalysis == NULL || _currentAnalysis->isCompleted())
		{
			typedef bimap<int, Analysis*>::left_reference pair;

			BOOST_FOREACH(pair pair, _analyses.left)
			{
				Analysis *analysis = pair.second;
				if (analysis->isCompleted() == false)
					_currentAnalysis = analysis;
			}
		}

		if (_currentAnalysis == NULL)
			return;

		BOOST_FOREACH(AnalysisPart *part, *_currentAnalysis)
		{
			if ( ! part->isCompleted())
				_currentPart = part;
		}

		Json::Value analysisPartAsJson = _currentPart->asJSON();
		analysisPartAsJson["perform"] = "init";
		analysisPartAsJson["id"] = _analyses.right.at(_currentAnalysis);
		analysisPartAsJson["pid"] = _analysesParts.right.at(_currentPart);
		analysisPartAsJson["revision"] = _currentPart->revision();

		string asString = analysisPartAsJson.toStyledString();

		qDebug() << asString.c_str();

		_messageQueueOut->send(asString.c_str(), asString.length(), 0);

#ifdef __APPLE__
		sem_post(_semaphoreOut);
#else
		_semaphoreOut->post();
#endif

		//analysis->onChange.connect(boost::bind(&EngineSync::analysisChanged, this));

	}
	catch (interprocess_exception e)
	{
		qDebug() << "interprocess_exception sending message " << e.what();

		throw e;
	}
}*/

void EngineSync::analysisAddedHandler(Analysis *analysis)
{
	Json::Value json = Json::Value(Json::objectValue);

	json["perform"] = "init";
	json["id"] = analysis->id();
	json["name"] = analysis->name();
	json["options"] = analysis->options()->asJSON();

	send(json);
}

void EngineSync::analysisOptionsChangedHandler(Analysis *analysis)
{
	Json::Value json = Json::Value(Json::objectValue);

	json["perform"] = "run";
	json["id"] = analysis->id();
	json["name"] = analysis->name();
	json["options"] = analysis->options()->asJSON();

	send(json);
}

void EngineSync::send(Json::Value json)
{
    string asString = json.toStyledString();
    _messageQueueOut->send(asString.c_str(), asString.length(), 0);

#ifdef __APPLE__
	sem_post(_semaphoreOut);
#else
	_semaphoreOut->post();
#endif

	cout << asString;
	cout.flush();

}

/*void EngineSync::analysisChanged()
{
	Json::Value v = _analyses->get(0)->asJSON();

	string data = v.toStyledString();

    try {

		_messageQueueOut->send(data.c_str(), data.length(), 0);

    }
    catch (interprocess_exception e)
    {
        qDebug() << "interprocess_exception sending message " << e.what();

        throw e;
    }

	//_messageQueueOut->send(data.utf16(), sizeof(unsigned short) * data.size(), 0);

#ifdef __APPLE__
	sem_post(_semaphoreOut);
#else
    _semaphoreOut->post();
#endif
}*/

void EngineSync::subProcessStandardOutput()
{
	QByteArray data = _process->readAllStandardOutput();
	qDebug() << QString(data);
}

void EngineSync::subProcessStandardError()
{
	qDebug() << _process->readAllStandardError();
}

void EngineSync::subProcessStarted()
{
	qDebug() << "subprocess started";
}

void EngineSync::subProcessError(QProcess::ProcessError error)
{
	qDebug() << "subprocess error" << error;
}

void EngineSync::subprocessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	qDebug() << "subprocess finished" << exitCode;
}


void EngineSync::checkForMessages()
{

#ifdef __APPLE__
    while (sem_trywait(_semaphoreIn) == 0) // 0 = success?!
#else
    while (_semaphoreIn->try_wait())
#endif
	{
		boost::interprocess::message_queue::size_type messageSize;
		uint priority;

		if ( ! _messageQueueIn->try_receive(_buffer, sizeof(_buffer), messageSize, priority))
			continue;

		string message = string(_buffer, messageSize);

		cout << message;
		cout.flush();

		Json::Reader reader;

		Json::Value json;
		reader.parse(message, json);

		int id = json.get("id", 0).asInt();
        //string name = json.get("name", Json::nullValue).asString();
		string perform = json.get("perform", Json::nullValue).asString();
		Json::Value payload = json.get("analyses", Json::nullValue);

		Analysis *analysis = _analyses->get(id);

		if (perform == "init")
		{
			analysis->initialise(payload);
		}
		else if (perform == "run")
		{
			analysis->initialise(payload);
		}
	}
}
