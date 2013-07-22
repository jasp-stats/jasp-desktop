#ifndef ENGINE_H
#define ENGINE_H

#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/interprocess/sync/named_semaphore.hpp>
#include <map>

#include "../JASP-Common/lib_json/json.h"

#include "rcppbridge.h"

#define BUFFER_SIZE 1048576 // 1 meg

class Engine
{
public:
	explicit Engine();
	
public:

	void receiveMessage(char *buffer, size_t message_size);

	void run();
	
private:

	void send(Json::Value json);

	char buffer[BUFFER_SIZE];
	boost::interprocess::message_queue::size_type messageSize;
	unsigned int priority;

	//std::map<std::string, Json::Value> _analyses;
	Json::Value _currentRequest;
	Json::Value _currentAnalysis;

	boost::interprocess::message_queue *_mqIn;
	boost::interprocess::message_queue *_mqOut;

	DataSet *_dataSet;

	RcppBridge _R;

#ifdef __APPLE__
	sem_t *_semaphoreIn;
	sem_t *_semaphoreOut;
#else
    boost::interprocess::named_semaphore *_semaphoreIn;
    boost::interprocess::named_semaphore *_semaphoreOut;
#endif

};

#endif // ENGINE_H
