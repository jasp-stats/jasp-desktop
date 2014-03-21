#include "engine.h"

#include <strstream>
#include <cstdio>

#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/rinterface.h"
#include "../JASP-Common/analysisloader.h"
#include "../JASP-Common/process.h"

#include <sstream>

#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

using namespace std;
using namespace boost::interprocess;
using namespace Json;
using namespace boost::posix_time;

Engine::Engine()
{
	_dataSet = NULL;
	_channel = NULL;
	_slaveNo = 0;
	_currentAnalysis = NULL;
	_nextAnalysis = NULL;
}

void Engine::runAnalysis()
{
	if (_currentAnalysis == NULL || _currentAnalysis->status() == Analysis::Complete || _currentAnalysis->status() == Analysis::Aborted)
	{
		if (_nextAnalysis != NULL)
		{
			if (_currentAnalysis != NULL)
				delete _currentAnalysis;
			_currentAnalysis = _nextAnalysis;
			_nextAnalysis = NULL;
		}
		else
		{
			return;
		}
	}

	while (_currentAnalysis->status() == Analysis::Empty)
		_currentAnalysis->init();

	while (_currentAnalysis->status() == Analysis::Running)
		_currentAnalysis->run();

	while (_currentAnalysis->status() == Analysis::Empty)
		_currentAnalysis->init();
}

void Engine::analysisResultsChanged(Analysis *analysis)
{
	receiveMessages();

	if (analysis->status() != Analysis::Empty) // i.e. didn't become uninitialised by the messages received
		send(analysis);
}

void Engine::run()
{
	stringstream ss;
	ss << "JASP-IPC-" << Process::parentPID();
	string memoryName = ss.str();

	_channel = new IPCChannel(memoryName, _slaveNo, true);

	do
	{
		receiveMessages(20);
		if ( ! Process::isParentRunning())
			break;
		runAnalysis();

	}
    while(1);

	shared_memory_object::remove(memoryName.c_str());
}

void Engine::setSlaveNo(int no)
{
	_slaveNo = no;
}

void Engine::receiveMessages(int timeout)
{
	string data;

	if (_channel->receive(data, timeout))
	{
		Value jsonRequest;
		Reader r;
		r.parse(data, jsonRequest, false);

		string analysisName = jsonRequest.get("name", nullValue).asString();
		int id = jsonRequest.get("id", -1).asInt();
		bool run = jsonRequest.get("perform", "run").asString() == "run";
		Value jsonOptions = jsonRequest.get("options", nullValue);

		if (_currentAnalysis != NULL && _currentAnalysis->id() == id)
		{
			_currentAnalysis->options()->set(jsonOptions);
			if (run)
				_currentAnalysis->setStatus(Analysis::Running);
		}
		else if (_nextAnalysis != NULL && _nextAnalysis->id() == id)
		{
			_nextAnalysis->options()->set(jsonOptions);
			if (run)
				_nextAnalysis->setStatus(Analysis::Running);
		}
		else
		{
			Analysis *analysis = AnalysisLoader::load(id, analysisName);
			analysis->options()->set(jsonOptions);
			if (run)
				analysis->setStatus(Analysis::Running);

			if (_dataSet == NULL)
			{
				managed_shared_memory *mem = SharedMemory::get();
				_dataSet = mem->find<DataSet>(boost::interprocess::unique_instance).first;
				_R.setDataSet(_dataSet);
			}

			analysis->setDataSet(_dataSet);
			analysis->setRInterface(&_R);
			analysis->resultsChanged.connect(boost::bind(&Engine::analysisResultsChanged, this, _1));

			if (_nextAnalysis != NULL)
				delete _nextAnalysis;

			_nextAnalysis = analysis;

			if (_currentAnalysis != NULL)
				_currentAnalysis->setStatus(Analysis::Aborted);
		}
	}
}

void Engine::send(Analysis *analysis)
{
	Value results = Value(objectValue);

	results["id"] = analysis->id();
	results["name"] = analysis->name();
	results["results"] = analysis->results();

	string status;

	switch (analysis->status())
	{
	case Analysis::Inited:
		status = "inited";
		break;
	case Analysis::Running:
		status = "running";
		break;
	case Analysis::Complete:
		status = "complete";
		break;
	default:
		status = "error";
		break;
	}

	results["status"] = status;

	string message = results.toStyledString();

	//cout << message;
	//cout.flush();

	_channel->send(message);
}

