#include "engine.h"

#include <strstream>
#include <cstdio>

#include "../JASP-Common/lib_json/json.h"

#include "rinterface.h"

#include "../JASP-Common/analysisloader.h"
#include "../JASP-Common/analyses/frequencies.h"
#include "../JASP-Common/analyses/ttestonesample.h"

#include <sstream>

using namespace std;
using namespace boost::interprocess;
using namespace Json;
using namespace boost::posix_time;

Engine::Engine()
{
	_dataSet = NULL;
}

void Engine::receiveMessage(char *buffer, size_t message_size)
{
    Value v;
	Reader r;
	r.parse(buffer, &buffer[message_size], v, false);

	_currentAnalysis = v;

	string analysisName = _currentAnalysis.get("name", nullValue).asString();
	int id = _currentAnalysis.get("id", nullValue).asInt();
	Value options = _currentAnalysis.get("options", nullValue);
	string perform = _currentAnalysis.get("perform", "init").asString();

	Analysis *analysis = AnalysisLoader::load(id, analysisName);
	analysis->options()->set(options);

	if (_dataSet == NULL)
	{
		managed_shared_memory *mem = SharedMemory::get();
		_dataSet = mem->find<DataSet>(boost::interprocess::unique_instance).first;
	}

	if (analysis != NULL)
	{
		analysis->setDataSet(_dataSet);
		analysis->setRInterface(&_R);

		analysis->init();

		Value results = Value(objectValue);

		results["id"] = id;
		results["name"] = analysisName;
		results["results"] = analysis->results();
		results["perform"] = "init";

		if (perform == "init")
			send(results);

		analysis->run();

		results["results"] = analysis->results();
		results["perform"] = "run";

		send(results);

		delete analysis;
	}
}

void Engine::run()
{

    _mqIn = new message_queue(open_only, "JASP_MQ");
    _mqOut = new message_queue(open_only, "JASPEngine_MQ");

#ifdef __APPLE__
    _semaphoreIn = sem_open("JASP_SM", O_EXCL);
    _semaphoreOut = sem_open("JASPEngine_SM", O_EXCL);
#else
    _semaphoreIn = new named_semaphore(open_only, "JASP_SM");
    _semaphoreOut = new named_semaphore(open_only, "JASPEngine_SM");
#endif

	_lastReceive = microsec_clock::universal_time();

	do
	{
		// waiting semaphores in OSX consume 100% CPU!

#ifdef __APPLE__
		if (sem_trywait(_semaphoreIn) == 0)  // 0 = success?!
#else

		ptime now(microsec_clock::universal_time());
		ptime then = now + microseconds(20000);

        //if (_semaphoreIn->timed_wait(then))
        _semaphoreIn->wait();
#endif
		{
			if (_mqIn->try_receive(buffer, sizeof(buffer), messageSize, priority))
				receiveMessage(buffer, messageSize);
			_lastReceive = microsec_clock::universal_time();
		}
#ifdef __APPLE__
		else
		{
			usleep(20000);
		}

		time_duration elapsed = microsec_clock::universal_time() - _lastReceive;
		if (elapsed.seconds() >= 2)
			break;

#endif
	}
	while(1);
}

void Engine::send(Json::Value json)
{
	string message = json.toStyledString();

	cout << message;
	cout.flush();

	int length = message.length();

	_mqOut->send(message.c_str(), message.length(), 0);

#ifdef __APPLE__
	sem_post(_semaphoreOut);
#else
    _semaphoreOut->post();
#endif
}
