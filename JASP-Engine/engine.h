//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "dataset.h"
#include "ipcchannel.h"
#include "processinfo.h"
#include "jsonredirect.h"

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
 *
 * Additionally: an engine can run a filter and return the result of that to the dataset.
 */

class Engine
{
public:
	explicit Engine();
	
public:

	void run();
	void setSlaveNo(int no);
	void applyFilter();
	
private:

	bool receiveMessages(int timeout = 0);
	void runAnalysis();
	void saveImage();
    void editImage();
	void sendResults();
	void sendFilterResult(std::vector<bool> filterResult, std::string warning = "");
	void sendFilterError(std::string errorMessage);
	void evalRCode(const std::string &rCode);
	void sendRCodeResult(std::string rCodeResult);
	void sendRCodeError();
	std::string callback(const std::string &results, int progress);

	DataSet *provideDataSet();
	void provideTempFileName(const std::string &extension, std::string &root, std::string &relativePath);
	void provideStateFileName(std::string &root, std::string &relativePath);

	typedef enum { empty, toInit, initing, inited, toRun, running, changed, complete, error, exception, aborted, stopped, saveImg, editImg} Status;

	Status _status = empty;

	int _analysisId;
	int _analysisRevision;
	int _progress;
	std::string _analysisName;
	std::string _analysisTitle;
	bool _analysisRequiresInit;
	std::string _analysisDataKey;
	std::string _analysisOptions;
	std::string _analysisResultsMeta;
	std::string _analysisStateKey;
	std::string _analysisResultsString;
	Json::Value _imageOptions;
	int _ppi = 96;

	bool _currentAnalysisKnowsAboutChange;

	Json::Value _analysisResults;

	IPCChannel *_channel = NULL;
	DataSet *_dataSet = NULL;

	int _slaveNo = 0;

	bool filterChanged = false;
	std::string filter = "", generatedFilter = "";
};

#endif // ENGINE_H
