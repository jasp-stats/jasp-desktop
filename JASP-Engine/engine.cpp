#include "engine.h"

#include <strstream>
#include <sstream>
#include <cstdio>

#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/analysisloader.h"
#include "../JASP-Common/process.h"
#include "../JASP-Common/datasetloader.h"
#include "../JASP-Common/tempfiles.h"

#include "rbridge.h"



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

	_status = empty;

	rbridge_init();
	tempfiles_attach(Process::parentPID());

	DataSet *dataSet = DataSetLoader::getDataSet();
	rbridge_setDataSet(dataSet);

	rbridge_setFileNameSource(boost::bind(&Engine::provideTempFileName, this, _1));
	rbridge_setStateFileSource(boost::bind(&Engine::provideStateFileName, this));
}

void Engine::setSlaveNo(int no)
{
	_slaveNo = no;
}

void Engine::runAnalysis()
{
	if (_status == empty)
		return;

	string perform;

	if (_status == toInit)
	{
		perform = "init";
		_status = initing;
	}
	else
	{
		perform = "run";
		_status = running;
	}

	vector<string> tempFilesFromLastTime = tempfiles_retrieveList(_analysisId);

	RCallback callback = boost::bind(&Engine::callback, this, _1);
	_analysisResults = rbridge_run(_analysisName, _analysisOptions, perform, _ppi, callback);

	receiveMessages();

	if (_status == toInit)
	{
		// if a new message was received, the analysis was changed and we shouldn't send results
	}
	else if (_status == initing)
	{
		_status = inited;
		sendResults();
		_status = empty;

		tempfiles_deleteList(tempFilesFromLastTime);
	}
	else if (_status == running)
	{
		_status = complete;
		sendResults();
		_status = empty;

		tempfiles_deleteList(tempFilesFromLastTime);
	}

}

void Engine::run()
{
	stringstream ss;
	ss << "JASP-IPC-" << Process::parentPID();
	string memoryName = ss.str();

	_channel = new IPCChannel(memoryName, _slaveNo, true);

	do
	{
		receiveMessages(100);
		if ( ! Process::isParentRunning())
			break;
		runAnalysis();

	}
	while(1);

	shared_memory_object::remove(memoryName.c_str());
}

bool Engine::receiveMessages(int timeout)
{
	string data;

	if (_channel->receive(data, timeout))
	{
		Value jsonRequest;
		Reader r;
		r.parse(data, jsonRequest, false);

		_analysisId = jsonRequest.get("id", -1).asInt();
		_analysisName = jsonRequest.get("name", nullValue).asString();
		_analysisOptions = jsonRequest.get("options", nullValue).toStyledString();

		string perform = jsonRequest.get("perform", "run").asString();

		if (perform == "init")
			_status = toInit;
		else
			_status = toRun;

		Json::Value settings = jsonRequest.get("settings", Json::nullValue);
		if (settings.isObject())
		{
			Json::Value ppi = settings.get("ppi", Json::nullValue);
			_ppi = ppi.isInt() ? ppi.asInt() : 96;
		}
		else
		{
			_ppi = 96;
		}

		return true;
	}

	return false;
}

void Engine::sendResults()
{
	Value response = Value(objectValue);
	Value results;

	Json::Reader parser;
	parser.parse(_analysisResults, results, false);

	response["id"] = _analysisId;
	response["name"] = _analysisName;

	Json::Value resultsStatus = results.get("status", Json::nullValue);
	if (resultsStatus != Json::nullValue)
	{
		response["results"] = results.get("results", Json::nullValue);
		response["status"]  = resultsStatus.asString();
	}
	else
	{
		string status;

		switch (_status)
		{
		case inited:
			status = "inited";
			break;
		case running:
			status = "running";
			break;
		case complete:
			status = "complete";
			break;
		default:
			status = "error";
			break;
		}

		response["results"] = results;
		response["status"] = status;
	}

	string message = response.toStyledString();

	_channel->send(message);
}

int Engine::callback(const string &results)
{
	if (receiveMessages())
	{
		return 1; // abort analysis
	}

	if (results != "null")
	{
		_analysisResults = results;
		sendResults();
	}

	return 0;
}

string Engine::provideStateFileName()
{
	return tempfiles_createSpecific("state", _analysisId);
}

string Engine::provideTempFileName(const string &extension)
{
	return tempfiles_create(extension, _analysisId);
}

