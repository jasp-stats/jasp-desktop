#ifndef ENGINE_H
#define ENGINE_H

#include "../JASP-Common/dataset.h"
#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/ipcchannel.h"
#include "../JASP-Common/processinfo.h"

/* The Engine represents the background processes.
 * It's job is pretty straight forward; it reads analysis
 * requests from shared memory (a semaphore is set when there
 * is a new message), and runs the analysis.
 * If an analysis is running when a new request is received,
 * and it is the same analysis (analysisId's match), then the
 * analysis is notified of the change (probably to one of its
 * options).
 * If the analysisId's don't match, then the old analysis is
 * aborted, and the new one is set running.
 */

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
