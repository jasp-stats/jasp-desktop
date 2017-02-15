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
	void saveImage();
	void sendResults();
	std::string callback(const std::string &results);

	DataSet *provideDataSet();
	void provideTempFileName(const std::string &extension, std::string &root, std::string &relativePath);
	void provideStateFileName(std::string &root, std::string &relativePath);

	typedef enum { empty, toInit, initing, inited, toRun, running, changed, complete, error, exception, aborted, stopped, saveImg } Status;

	Status _status;

	int _analysisId;
	int _analysisRevision;
	std::string _analysisName;
	std::string _analysisOptions;
	std::string _analysisResultsString;
	Json::Value _imageOptions;
	int _ppi;

	bool _currentAnalysisKnowsAboutChange;

	Json::Value _analysisResults;

	IPCChannel *_channel;
	DataSet *_dataSet;
	std::string _engineInfo;

	int _slaveNo;
};

#endif // ENGINE_H
