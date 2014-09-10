#ifndef ENGINE_H
#define ENGINE_H

#include "rcppbridge.h"

#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/analysis.h"
#include "../JASP-Common/ipcchannel.h"
#include "../JASP-Common/process.h"

class Engine
{
public:
	explicit Engine();
	
public:

	void run();
	void setSlaveNo(int no);
	
private:

	void receiveMessages(int timeout = 0);
	void runAnalysis();
	void analysisResultsChanged(Analysis *analysis);
	void analysisYield();

	Analysis *_currentAnalysis;
	Analysis *_nextAnalysis;

	void send(Analysis *analysis);

	Json::Value _currentRequest;

	IPCChannel *_channel;

	DataSet *_dataSet;

	RcppBridge _R;

	int _slaveNo;

};

#endif // ENGINE_H
