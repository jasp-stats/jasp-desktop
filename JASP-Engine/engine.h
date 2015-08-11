#ifndef ENGINE_H
#define ENGINE_H

#include "../JASP-Common/dataset.h"
#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/ipcchannel.h"
#include "../JASP-Common/processinfo.h"

class Engine
{
public:
	explicit Engine();
	
public:

	void run();
	void setSlaveNo(int no);
	
private:

	bool receiveMessages(int timeout = 0);
	void runAnalysis();
	void sendResults();
	std::string callback(const std::string &results);

	void provideTempFileName(const std::string &extension, std::string &root, std::string &relativePath);
	void provideStateFileName(std::string &root, std::string &relativePath);

	typedef enum { empty, toInit, initing, inited, toRun, running, changed, complete, error, aborted, stopped } Status;

	Status _status;

	int _analysisId;
	int _analysisRevision;
	std::string _analysisName;
	std::string _analysisOptions;
	std::string _analysisResultsString;
	int _ppi;

	bool _currentAnalysisKnowsAboutChange;

	Json::Value _analysisResults;

	IPCChannel *_channel;
	DataSet *_dataSet;
	std::string _engineInfo;

	int _slaveNo;
};

#endif // ENGINE_H
