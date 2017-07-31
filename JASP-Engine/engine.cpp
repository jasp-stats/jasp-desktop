//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "engine.h"

#include <sstream>
#include <cstdio>

#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/analysisloader.h"
#include "../JASP-Common/processinfo.h"
#include "../JASP-Common/tempfiles.h"
#include "../JASP-Common/utils.h"
#include "../JASP-Common/sharedmemory.h"

#include "rbridge.h"



#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

using namespace std;
using namespace boost::interprocess;
using namespace boost::posix_time;

Engine::Engine()
{
	_dataSet = NULL;
	_channel = NULL;
	_slaveNo = 0;
	_ppi = 96;

	_status = empty;

	rbridge_init();
	tempfiles_attach(ProcessInfo::parentPID());

	rbridge_setDataSetSource(boost::bind(&Engine::provideDataSet, this));
	rbridge_setFileNameSource(boost::bind(&Engine::provideTempFileName, this, _1, _2, _3));
	rbridge_setStateFileSource(boost::bind(&Engine::provideStateFileName, this, _1, _2));
}

void Engine::setSlaveNo(int no)
{
	_slaveNo = no;
}

void Engine::saveImage()
{
	if (_status != saveImg)
		return;

	vector<string> tempFilesFromLastTime = tempfiles_retrieveList(_analysisId);

	RCallback callback = boost::bind(&Engine::callback, this, _1);

	std::string name = _imageOptions.get("name", Json::nullValue).asString();
	std::string type = _imageOptions.get("type", Json::nullValue).asString();

	int height = _imageOptions.get("height", Json::nullValue).asInt();
	int width = _imageOptions.get("width", Json::nullValue).asInt();
	std::string result = rbridge_saveImage(name, type, height, width, _ppi);


	_status = complete;
	Json::Reader parser;
	parser.parse(result, _analysisResults, false);
	_analysisResults["results"]["inputOptions"] = _imageOptions;
	sendResults();
	_status = empty;

	//tempfiles_deleteList(tempFilesFromLastTime);

}

void Engine::runAnalysis()
{
	if (_status == empty || _status == aborted)
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

	_currentAnalysisKnowsAboutChange = false;
	_analysisResultsString = rbridge_run(_analysisName, _analysisOptions, perform, _ppi, callback);

	if (_status == initing || _status == running)  // if status hasn't changed
		receiveMessages();

	if (_status == toInit || _status == aborted || _status == error || _status == exception)
	{
		// analysis was aborted, and we shouldn't send the results
	}
	else if (_status == changed && (_currentAnalysisKnowsAboutChange == false || _analysisResultsString == "null"))
	{
		// analysis was changed, and the analysis either did not know about
		// the change (because it did not call a callback),
		// or it could not incorporate the changes (returned null).
		// in both cases it needs to be re-run, and results should
		// not be sent

		_status = toInit;
	}
	else
	{
		Json::Reader parser;
		parser.parse(_analysisResultsString, _analysisResults, false);

		_status = _status == initing ? inited : complete;
		sendResults();
		_status = empty;

		vector<string> filesToKeep;

		if (_analysisResults.isObject())
		{
			Json::Value filesToKeepValue = _analysisResults.get("keep", Json::nullValue);

			if (filesToKeepValue.isArray())
			{
				for (size_t i = 0; i < filesToKeepValue.size(); i++)
				{
					Json::Value fileToKeepValue = filesToKeepValue.get(i, Json::nullValue);
					if ( ! fileToKeepValue.isString())
						continue;

					filesToKeep.push_back(fileToKeepValue.asString());
				}
			}
			else if (filesToKeepValue.isString())
			{
				filesToKeep.push_back(filesToKeepValue.asString());
			}
		}

		Utils::remove(tempFilesFromLastTime, filesToKeep);

		tempfiles_deleteList(tempFilesFromLastTime);
	}
}

void Engine::run()
{
	if (_slaveNo == 0)
	{
		_engineInfo = rbridge_check();

		Json::Value v;
		Json::Reader r;
		r.parse(_engineInfo, v);

		std::cout << v.toStyledString() << "\n";
		std::cout.flush();
	}

	stringstream ss;
	ss << "JASP-IPC-" << ProcessInfo::parentPID();
	string memoryName = ss.str();

	_channel = new IPCChannel(memoryName, _slaveNo, true);

	do
	{
		receiveMessages(100);
		if ( ! ProcessInfo::isParentRunning())
			break;
		if (_status == saveImg)
			saveImage();
		else
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
		std::cout << "received message" << std::endl;
		std::cout.flush();

		Json::Value jsonRequest;
		Json::Reader r;
		r.parse(data, jsonRequest, false);

		int analysisId = jsonRequest.get("id", -1).asInt();
		string perform = jsonRequest.get("perform", "run").asString();

		if (analysisId == _analysisId && _status == running)
		{
			// if the current running analysis has changed

			if (perform == "init")
				_status = changed;
			else if (perform == "stop")
				_status = stopped;
			else
				_status = aborted;
		}
		else
		{
			// the new analysis should be init or run (existing analyses will be aborted)

			_analysisId = analysisId;

			if (perform == "init")
				_status = toInit;
			else if (perform == "run")
				_status = toRun;
			else if (perform == "saveImg")
				_status = saveImg;
			else
				_status = error;
		}

		if (_status == toInit || _status == toRun || _status == changed || _status == saveImg)
		{
			_analysisName = jsonRequest.get("name", Json::nullValue).asString();
			_analysisOptions = jsonRequest.get("options", Json::nullValue).toStyledString();
			_analysisRevision = jsonRequest.get("revision", -1).asInt();
			_imageOptions = jsonRequest.get("image", Json::nullValue);

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
		}

		return true;
	}

	return false;
}

void Engine::sendResults()
{
	Json::Value response = Json::Value(Json::objectValue);

	response["id"] = _analysisId;
	response["name"] = _analysisName;
	response["revision"] = _analysisRevision;

	Json::Value resultsStatus = Json::nullValue;

	if (_analysisResults.isObject())
		resultsStatus = _analysisResults.get("status", Json::nullValue);

	if (resultsStatus != Json::nullValue)
	{
		response["results"] = _analysisResults.get("results", Json::nullValue);
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
		case changed:
			status = "running";
			break;
		case complete:
			status = "complete";
			break;
		case stopped:
			status = "stopped";
			break;
		default:
			status = "error";
			break;
		}

		response["results"] = _analysisResults;
		response["status"] = status;
	}

	string message = response.toStyledString();

	_channel->send(message);
}

string Engine::callback(const string &results)
{
	receiveMessages();

	if (_status == aborted || _status == toInit || _status == toRun)
		return "{ \"status\" : \"aborted\" }"; // abort

	if (_status == changed && _currentAnalysisKnowsAboutChange)
	{
		_status = running;
		_currentAnalysisKnowsAboutChange = false;
	}

	if (results != "null")
	{
		_analysisResultsString = results;

		Json::Reader parser;
		parser.parse(_analysisResultsString, _analysisResults, false);

		sendResults();
	}

	if (_status == changed)
	{
		_currentAnalysisKnowsAboutChange = true; // because we're telling it now
		return "{ \"status\" : \"changed\", \"options\" : " + _analysisOptions + " }";
	}
	else if (_status == stopped)
	{
		return "{ \"status\" : \"stopped\" }";
	}
	else if (_status == aborted)
	{
		return "{ \"status\" : \"aborted\" }";
	}

	return "{ \"status\" : \"ok\" }";
}

DataSet *Engine::provideDataSet()
{
	return SharedMemory::retrieveDataSet();
}

void Engine::provideStateFileName(string &root, string &relativePath)
{
	return tempfiles_createSpecific("state", _analysisId, root, relativePath);
}

void Engine::provideTempFileName(const string &extension, string &root, string &relativePath)
{	
	tempfiles_create(extension, _analysisId, root, relativePath);
}
