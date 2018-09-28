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

#include "enginedefinitions.h"
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
 *
 * Since 2018-06 (JCG): This is getting less and less accurate of a description but i am not about to change it right now.
 */

class Engine
{
public:
	explicit Engine(int slaveNo, unsigned long parentPID);
	static Engine * theEngine() { return _EngineInstance; } //There is only ever one engine in a process so we might as well have a static pointer to it.


	void run();
	bool receiveMessages(int timeout = 0);
	void setSlaveNo(int no);
	void sendString(std::string message) { _channel->send(message); }

	typedef enum { empty, toInit, initing, inited, toRun, running, changed, complete, error, exception, aborted, stopped, saveImg, editImg} Status;
	Status getStatus() { return _status; }
	analysisResultStatus getStatusToAnalysisStatus();

	//return true if changed:
	bool setColumnDataAsScale(std::string columnName, std::vector<double> scalarData)				{	return provideDataSet()->columns()[columnName].overwriteDataWithScale(scalarData);		}
	bool setColumnDataAsOrdinal(std::string columnName, std::vector<int> ordinalData)				{	return provideDataSet()->columns()[columnName].overwriteDataWithOrdinal(ordinalData);	}
	bool setColumnDataAsNominal(std::string columnName, std::vector<int> nominalData)				{	return provideDataSet()->columns()[columnName].overwriteDataWithNominal(nominalData);	}
	bool setColumnDataAsNominalText(std::string columnName, std::vector<std::string> nominalData)	{	return provideDataSet()->columns()[columnName].overwriteDataWithNominal(nominalData);	}

	int dataSetRowCount() { return static_cast<int>(provideDataSet()->rowCount()); }

private:
// Methods:
	void receiveFilterMessage(			Json::Value jsonRequest);
	void receiveRCodeMessage(			Json::Value jsonRequest);
	void receiveComputeColumnMessage(	Json::Value jsonRequest);
	void receiveAnalysisMessage(		Json::Value jsonRequest);

	void runAnalysis();
	void runRCode();
	void runFilter();
	void runComputeColumn();

	void removeNonKeepFiles(Json::Value filesToKeepValue);
	void saveImage();
    void editImage();

	void sendAnalysisResults();

	void sendFilterResult(std::vector<bool> filterResult, std::string warning = "");
	void sendFilterError(std::string errorMessage);


	void sendRCodeResult(std::string rCodeResult);
	void sendRCodeError();

	std::string callback(const std::string &results, int progress);

	DataSet *provideDataSet();

	void provideTempFileName(const std::string &extension, std::string &root, std::string &relativePath);
	void provideStateFileName(std::string &root,		std::string &relativePath);
	void provideJaspResultsFileName(std::string &root,	std::string &relativePath);

// Data:
	static Engine * _EngineInstance;

	Status _status = empty;


	int			_analysisId,
				_analysisRevision,
				_progress,
				_ppi = 96,
				_slaveNo = 0,
				_rCodeRequestId = -1,
				_filterRequestId = -1;

	bool		_analysisRequiresInit,
				_analysisJaspResults,
				_currentAnalysisKnowsAboutChange;

	std::string _analysisName,
				_analysisTitle,
				_analysisDataKey,
				_analysisOptions,
				_analysisResultsMeta,
				_analysisStateKey,
				_analysisResultsString,
				_filter = "",
				_generatedFilter = "",
				_rCode = "",
				_computeColumnCode = "",
				_computeColumnName = "";

	Column::ColumnType		_computeColumnType = Column::ColumnTypeUnknown;

	Json::Value _imageOptions,
				_analysisResults;

	IPCChannel *_channel = NULL;

	unsigned long _parentPID = 0;

	engineState currentEngineState = engineState::idle;
};

#endif // ENGINE_H
