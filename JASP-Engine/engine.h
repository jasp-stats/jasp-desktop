#ifndef ENGINE_H
#define ENGINE_H

#include <map>

#include "../JASP-Common/analyses.h"
#include "../JASP-Common/lib_json/json.h"

#include "rcppbridge.h"
#include "../JASP-Common/ipcchannel.h"

class Engine
{
public:
	explicit Engine();
	
public:

	void run();
    void setParentPID(unsigned long pid);
	void setSlaveNo(int no);
	
private:

	void receiveMessages(int timeout = 0);
	void runAnalysis();
	void analysisResultsChanged(Analysis *analysis);

	bool shouldISuicide();

	Analysis *_currentAnalysis;
	Analysis *_nextAnalysis;

	void send(Analysis *analysis);

	Json::Value _currentRequest;

	IPCChannel *_channel;

	DataSet *_dataSet;

	RcppBridge _R;

	unsigned long _parentPID;
	int _slaveNo;

#ifdef __WIN32__
    void* _parentHandle;
#endif

};

#endif // ENGINE_H
