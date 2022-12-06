#include "enginerepresentation.h"
#include "gui/preferencesmodel.h"
#include "utilities/messageforwarder.h"
#include "utilities/qutils.h"
#include "utils.h"
#include "log.h"

EngineRepresentation::EngineRepresentation(size_t channelNumber, QProcess * slaveProcess, QObject * parent)
	: QObject(parent), _channelNumber(channelNumber)
{
	setSlaveProcess(slaveProcess);
}


void EngineRepresentation::setSlaveProcess(QProcess * slaveProcess)
{
	Log::log() << "Setting new engine process to engineRepresentation Engine #" << _channelNumber << std::endl;

	_slaveCrashed = false;
	_slaveProcess = slaveProcess;
	_slaveProcess->setParent(this);

	_slaveFinishedConnection = connect(_slaveProcess, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),	this, &EngineRepresentation::processFinished);
}

EngineRepresentation::~EngineRepresentation()
{
	Log::log() << "~EngineRepresentation() Engine #" << _channelNumber << std::endl;


	if(_slaveProcess != nullptr)
	{
		_slaveProcess->terminate();
		_slaveProcess->kill();
	}
}

void EngineRepresentation::cleanUpAfterClose()
{
	disconnect(_analysisInProgressStatusConnection); //Just in case

	Analysis * runMeLater = nullptr;
	if(_analysisAborted && (_analysisAborted->isAborted() || _analysisAborted->isAborting()))
		runMeLater = _analysisAborted;

	_analysisInProgress = nullptr;
	_analysisAborted	= nullptr;
	_idRemovedAnalysis	= -1;
	_lastRequestId		= -1;
	_abortTime			= -1;
	_pauseRequested		= false;
	_stopRequested		= false;
	_slaveCrashed		= false;
	_settingsChanged	= true;
	_abortAndRestart	= false;
	_lastCompColName	= "???";


	if(_dynModName != "")
		emit unregisterForModule(this, _dynModName);

	_dynModName			= "";
	_moduleLoaded		= "";

	if(runMeLater)
		runMeLater->run();
}

void EngineRepresentation::sendString(std::string str)
{
#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "sending to jaspEngine: " << str << "\n" << std::endl;
#endif
	channel()->send(str);
}



void EngineRepresentation::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	Log::log() << "Engine # " << channelNumber() << " finished while in state '" << _engineState << "' " << (exitStatus == QProcess::ExitStatus::NormalExit || stopped() ? "normally" : "crashing") << " and with exitCode " << exitCode << "!" << std::endl;

	if(_slaveProcess)
		_slaveProcess->deleteLater();
	_slaveProcess = nullptr;
	_slaveCrashed = exitStatus == QProcess::ExitStatus::CrashExit && !(stopped() || killed());

	handleEngineCrash();
}

void EngineRepresentation::handleEngineCrash()
{
	Log::log() << "EngineRepresentation::handleEngineCrash():\n" << currentStateForDebug() << std::endl;

	switch(_engineState)
	{

	case engineState::analysis:
		if(_analysisInProgress)
		{
			_analysisInProgress->setErrorInResults(fq(tr("The engine crashed while trying to run this analysis...")));
			clearAnalysisInProgress();
		}
		break;

	case engineState::filter:
		emit processFilterErrorMsg(tr("The engine crashed while trying to run the filter..."), _lastRequestId);
		break;

	case engineState::computeColumn:
		emit computeColumnFailed(tq(_lastCompColName), tr("The engine crashed while trying to compute this column..."));
		break;

	case engineState::rCode:
		emit rCodeReturned(tr("The engine crashed while trying to run rscript..."), _lastRequestId, true);
		break;

	case engineState::logCfg:
		//So if the engine crashes on log config change request then we can still continue because it will also get the proper settings on startup.
		//And if it is still broken then we will simply see a crash screen then...
		break;

	case engineState::idle:
	{
		static int idleCrashes = 0;

		if(idleCrashes++ < 3)
			break; //Try it a couple of times because the engine might have crashed right after completing an analysis (because of .crashPlease() for instance) or something. But if it keeps happening something is probably wrong
	}

	case engineState::stopped:
		//It will be resumed manually
		return;

	case engineState::killed:
		return; //It will be resumed by EngineSync::restartKilledEngines()

	default: //If not one of the above then let the engine crash and burn (https://www.youtube.com/watch?v=UtUpXPiSJEg)
		emit  engineTerminated();
		return;
	}

	setState(engineState::initializing);

	emit requestEngineRestart(this); //Only for actual crashes
}

void EngineRepresentation::clearAnalysisInProgress()
{
	Log::log() << "Engine " << channelNumber() << " clears current analysis in progress (" << (_analysisInProgress ? _analysisInProgress->name() : "???" ) << ")" << std::endl;

	disconnect(_analysisInProgressStatusConnection);
	_analysisInProgress = nullptr;
	setState(engineState::idle);
}

void EngineRepresentation::setAnalysisInProgress(Analysis* analysis)
{
	if(_engineState == engineState::analysis)
	{
		if(_analysisInProgress == analysis)
			return; //we are already busy with this analysis so everything is fine
		else
			throw std::runtime_error("Engine " + std::to_string(channel()->channelNumber()) + " is running another analysis. Yet you are trying to set an analysis in progress on it..");
	}

	if(_engineState != engineState::idle)
		throw std::runtime_error("Engine " + std::to_string(channel()->channelNumber()) + " is not idle! Yet you are trying to set an analysis in progress on it..");

	if(_dynModName	!= analysis->dynamicModule()->name())
		throw std::runtime_error("Engine " + std::to_string(channel()->channelNumber()) + " is assigned to module '" + _dynModName + "'! Yet you are trying to set an analysis from '" + analysis->dynamicModule()->name() + "' in progress on it..");

	_analysisInProgress = analysis;

	setState(engineState::analysis);

	_analysisInProgressStatusConnection = connect(_analysisInProgress, &Analysis::statusChanged, this, &EngineRepresentation::analysisStatusChanged);

}

void EngineRepresentation::setDynamicModule(const std::string & name)
{
	if(!_runsAnalysis)
		throw std::runtime_error("Cannot register engine " + std::to_string(channelNumber()) + " for dynamic module '" + name + "' to engine not meant for analyses!");

	_dynModName		= name;
	_moduleLoaded	= false;
}

void EngineRepresentation::moduleLoad()
{
	if(_dynModName == "" || moduleLoaded() || moduleLoading())
		return;


	if(!idle())
		return;

	runModuleLoadRequestOnProcess(Modules::DynamicModules::dynMods()->dynamicModule(_dynModName)->requestJsonForPackageLoadingRequest());
}

void EngineRepresentation::processReplies()
{
	if(!channel())
	{
		Log::log() << "EngineRepresentation::processReplies() for engine #" << channelNumber() << " called but no channel available." << std::endl;
		return;
	}

	if(!_slaveProcess)
		return; //No point in receiving replies from an engine that isn't running is there?

	if(_engineState == engineState::idle)
	{

		if		(_stopRequested)	sendStopEngine();
		else if	(_pauseRequested)	sendPauseEngine();

		if(_idleStartSecs == -1)
			_idleStartSecs = Utils::currentSeconds();

		return;
	}

	_idleStartSecs = -1;

	std::string data;

	if (channel()->receive(data))
	{
#ifdef PRINT_ENGINE_MESSAGES
		{
			const int _maxDataChars = 300;//I do not want to keep scrolling forever all the time...
			if(data != "")	Log::log() << "message received from engine #" << channelNumber() << ": " << (data.size() < _maxDataChars ? data : data.substr(0, _maxDataChars)) + "..." << std::endl;
			else			Log::log() << "Engine #" << channelNumber() << " cleared its send-buffer." << std::endl;
		}
#endif

		if(data == "")
			return;

		Json::Value json;
		bool		jsonIsOK		= false,
					jsonMakesSense	= false;
		std::string jsonParseError;

		try
		{
			Json::Reader reader;
			jsonIsOK = reader.parse(data, json);

			jsonParseError = reader.getFormattedErrorMessages();

			jsonMakesSense = jsonIsOK && (json.get("typeRequest", Json::nullValue).isString() || _engineState == engineState::analysis);
		}
		catch(std::exception & e)
		{
			Log::log() << "Parsing/checking json had exception: " << e.what() << std::endl;
		}

		if(!jsonIsOK)
		{
			Log::log() << "Malformed reply from engine in state " << _engineState << ": '" << data << "', problem was: '" << jsonParseError << "'" << std::endl;
			throw std::runtime_error("Malformed reply from engine!");
		}

		if(!jsonMakesSense)
		{
			Log::log() << "Json doesnt make sense?" << std::endl;
		}

		engineState typeRequest = engineStateFromString(json.get("typeRequest", "analysis").asString());

		if(_engineState == engineState::initializing)
		{
			Log::log() << "Engine #" << channelNumber() << " still initializing and got " + engineStateToString(typeRequest) << std::endl;

			if(typeRequest == engineState::resuming)
				_engineState = engineState::idle;
		}
		else
			switch(typeRequest)
			{
			case engineState::filter:				processFilterReply(json);			break;
			case engineState::rCode:				processRCodeReply(json);			break;
			case engineState::analysis:				processAnalysisReply(json);			break;
			case engineState::computeColumn:		processComputeColumnReply(json);	break;
			case engineState::paused:				processEnginePausedReply();			break;
			case engineState::resuming:				processEngineResumedReply();		break;
			case engineState::stopped:				processEngineStoppedReply();		break;
			case engineState::moduleInstallRequest:
			case engineState::moduleLoadRequest:	processModuleRequestReply(json);	break;
			case engineState::logCfg:				processLogCfgReply();				break;
			case engineState::settings:				processSettingsReply();				break;
			case engineState::reloadData:			processReloadDataReply();			break;
			default:								throw std::logic_error("If you define new engineStates you should add them to the switch in EngineRepresentation::process()!");
			}
	}
	else if(_engineState == engineState::initializing && !_stopRequested)
		resumeEngine();


	if(!_stopRequested && _analysisAborted && _analysisInProgress && _abortTime + ENGINE_KILLTIME < Utils::currentMillis()) //We wait a second or two before we kill the engine if it does not want to abort.
	{
		if(jaspEngineStillRunning())
			killEngine();

		restartAbortedAnalysis();
	}
}

void EngineRepresentation::runScriptOnProcess(RFilterStore * filterStore)
{
	Json::Value json = Json::Value(Json::objectValue);

	setState(engineState::filter);

	json["typeRequest"]		= engineStateToString(_engineState);
	json["generatedFilter"] = filterStore->generatedfilter.toStdString();
	json["requestId"]		= filterStore->requestId;

	_lastRequestId			= filterStore->requestId;

	QString dataFilter = filterStore->script == "" ? "*" : filterStore->script;
	json["filter"] = dataFilter.toStdString();

	Log::log() << "sending filter with requestID " << filterStore->requestId << " to engine" << std::endl;

	sendString(json.toStyledString());
}

void EngineRepresentation::processFilterReply(Json::Value & json)
{
	checkIfExpectedReplyType(engineState::filter);

	setState(engineState::idle);

#ifdef PRINT_ENGINE_MESSAGES
			Log::log() << "msg is filter reply" << std::endl;
#endif

	int requestId = json.get("requestId", -1).asInt();

	emit filterDone(requestId);

	if(json.get("filterResult", Json::Value(Json::intValue)).isArray()) //If the result is an array then it came from the engine.
	{
		std::vector<bool> filterResult;
		for(Json::Value & jsonResult : json.get("filterResult", Json::Value(Json::arrayValue)))
			filterResult.push_back(jsonResult.asBool());

		emit processNewFilterResult(filterResult, requestId);

		if(json.get("filterError", "").asString() != "")
			emit processFilterErrorMsg(QString::fromStdString(json.get("filterError", "there was a warning").asString()), requestId);
	}
	else
		emit processFilterErrorMsg(QString::fromStdString(json.get("filterError", "something went wrong").asString()), requestId);
}

void EngineRepresentation::runScriptOnProcess(const QString & rCmdCode)
{
	RScriptStore * script = new RScriptStore(-1, rCmdCode, "", engineState::rCode, false, true);

	runScriptOnProcess(script);

	delete script;
}

void EngineRepresentation::runScriptOnProcess(RScriptStore * scriptStore)
{
	Json::Value json = Json::Value(Json::objectValue);

	setState(engineState::rCode);

	json["typeRequest"]		= engineStateToString(_engineState);
	json["rCode"]			= scriptStore->script.toStdString();
	json["requestId"]		= scriptStore->requestId;
	json["whiteListed"]		= scriptStore->whiteListedVersion;
	json["returnLog"]		= scriptStore->returnLog;

	_lastRequestId			= scriptStore->requestId;

	sendString(json.toStyledString());
}


void EngineRepresentation::processRCodeReply(Json::Value & json)
{
	checkIfExpectedReplyType(engineState::rCode);

	setState(engineState::idle);

	std::string rCodeResult = json.get("rCodeResult", "").asString();
	std::string rCodeError	= json.get("rCodeError", "").asString();
	int requestId			= json.get("requestId", -1).asInt();
	bool hasError			= !rCodeError.empty();

	if (hasError) rCodeResult = rCodeError;

	if(!runsRCmd())			emit rCodeReturned(		tq(rCodeResult), requestId, hasError);
	else					emit rCodeReturnedLog(	tq(rCodeResult), hasError);

	Log::log() << "rCode reply for request (" << requestId << ") returned: " << rCodeResult << " with error: '" << json.get("rCodeError", "no error") << "'\n" <<  std::flush;
}


void EngineRepresentation::runScriptOnProcess(RComputeColumnStore * computeColumnStore)
{
	Json::Value json = Json::Value(Json::objectValue);

	setState(engineState::computeColumn);

	json["typeRequest"]		= engineStateToString(_engineState);
	json["columnName"]		= computeColumnStore->_columnName.toStdString();
	json["computeCode"]		= computeColumnStore->script.toStdString();
	json["columnType"]		= columnTypeToString(computeColumnStore->_columnType);

	_lastCompColName		= json["columnName"].asString();

	sendString(json.toStyledString());
}


void EngineRepresentation::processComputeColumnReply(Json::Value & json)
{
	checkIfExpectedReplyType(engineState::computeColumn);

	setState(engineState::idle);


	std::string result		= json.get("result", "some string that is not 'TRUE' or 'FALSE'").asString();
	std::string error		= json.get("error", "").asString();
	std::string columnName	= json.get("columnName", "").asString();

	if(result == "TRUE")		emit computeColumnSucceeded(QString::fromStdString(columnName), QString::fromStdString(error), true);
	else if(result == "FALSE")	emit computeColumnSucceeded(QString::fromStdString(columnName), QString::fromStdString(error), false);
	else						emit computeColumnFailed(	QString::fromStdString(columnName), QString::fromStdString(error == "" ? "Unknown Error" : error));
}

void EngineRepresentation::runAnalysisOnProcess(Analysis *analysis)
{
#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "send request for analysis-id #" << analysis->id() << " to jaspEngine on channel #" << channelNumber() << std::endl;
#endif

	setAnalysisInProgress(analysis);

	Json::Value json(analysis->createAnalysisRequestJson());

#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "sending: " << json.toStyledString() << std::endl;
#endif

	channel()->send(json.toStyledString());

}

void EngineRepresentation::analysisRemoved(Analysis * analysis)
{
	if(_engineState != engineState::analysis || _analysisInProgress != analysis)
		return;

	_idRemovedAnalysis = analysis->id();
	abortAnalysisInProgress(false);
	_analysisInProgress = nullptr; //Because it will be deleted!
	disconnect(_analysisInProgressStatusConnection);
	//But we keep the engineState at analysis to make sure another analysis won't try to run until the aborted one gets the message!
}

void EngineRepresentation::processAnalysisReply(Json::Value & json)
{
#ifdef PRINT_ENGINE_MESSAGES
	Log::log() << "Analysis reply: " << json.toStyledString() << std::endl;
#endif

	if(_engineState == engineState::paused || _engineState == engineState::resuming || _engineState == engineState::idle)
	{
		Log::log() << "Do not process analysis reply because engineState is paused, resuming or idle" << std::endl;
		return;
	}

	if(_engineState != engineState::analysis)
	{
		Log::log() << "The engineState is not analysis!!!" << std::endl;
		return;
	}

	int id						= json.get("id",		-1).asInt();
	int revision				= json.get("revision",	-1).asInt();
	
	Json::Value progress		= json.get("progress",	Json::nullValue);
	Json::Value results			= json.get("results",	Json::nullValue);

	analysisResultStatus status	= analysisResultStatusFromString(json.get("status", "???").asString());

	if(_analysisInProgress == nullptr && id == _idRemovedAnalysis)
	{
		Log::log() << "Reply was for an analysis that was removed, we now check if it was done or not, the resultstatus was: " << analysisResultStatusToString(status) << std::endl;
		switch(status)
		{
		case analysisResultStatus::changed:
		case analysisResultStatus::complete:
		case analysisResultStatus::fatalError:
		case analysisResultStatus::validationError:
			setState(engineState::idle);
			_idRemovedAnalysis	= -1;

			Log::log() << "Analysis got the message and we now reset the engineStatus to idle!" << std::endl;
			break;

		default:
			Log::log() << "Analysis ignores the abort it got and keeps going..." << std::endl;
			break;
		}
		return;
	}

	Analysis *analysis			= _analysisInProgress;

	if (analysis->id() != id || analysis->revision() < revision)
		throw std::runtime_error("Received results for wrong analysis!");

	if(analysis->revision() > revision && status != analysisResultStatus::imagesRewritten) //imagesRewritten always has revision 0
	{
		Log::log() << "Analysis reply was for an older revision (" << revision << ") than the one currently requested (" << analysis->revision() << "), so it can be ignored.\n";
		Log::log() << "Current status of analysis is: " << analysis->statusQ() << std::endl;

		if(_pauseRequested || _analysisAborted == analysis)
		{
			Log::log() << "A pause was requested or some setting changed, so the analysis was aborted." << std::endl;
			clearAnalysisInProgress();
		}

		return;
	}

	Log::log() << "Resultstatus of analysis was " << analysisResultStatusToString(status) << " and it will now be processed." << std::endl;

	switch(status)
	{
	case analysisResultStatus::imageSaved:
		if (results.get("error", false).asBool())
		{
			MessageForwarder::showWarning(tr("Error saving plot"), tr("Unfortunately the plot could not be saved.\n\nError message:\n%1\n\nIf the problem persists, please report the message above at: https://jasp-stats.org/bug-reports").arg(tq(results.get("errorMessage", "").asString())));
			analysis->setStatus(Analysis::Complete);
		}
		else
			analysis->imageSaved(results);
		clearAnalysisInProgress();
		break;


	case analysisResultStatus::imageEdited:

		if (results.get("error", false).asBool())
		{
			if (results.get("resized", false).asBool())
				MessageForwarder::showWarning(tr("Error resizing plot"), tr("Unfortunately the plot could not be resized.\n\nError message:\n%1\n\nIf the problem persists, please report the message above at: https://jasp-stats.org/bug-reports").arg(tq(results.get("errorMessage", "").asString())));
			else // plot editing
				MessageForwarder::showWarning(tr("Error editing plot"), tr("Unfortunately the plot could not be edited.\n\nError message:\n%1\n\nIf the problem persists, please report the message above at: https://jasp-stats.org/bug-reports").arg(tq(results.get("errorMessage", "").asString())));
		}
		analysis->imageEdited(results); // if an error occurs js needs to resize the plot back to the old size
		emit plotEditorRefresh();

		clearAnalysisInProgress();
		break;

	case analysisResultStatus::imagesRewritten:
		analysis->imagesRewritten(results);
		clearAnalysisInProgress();
		break;

	case analysisResultStatus::validationError:
	case analysisResultStatus::fatalError:
	case analysisResultStatus::complete:
		analysis->setResults(results, status);
		clearAnalysisInProgress();

		if(analysis->computedColumns().size() > 0)
			checkForComputedColumns(results);
		break;

	case analysisResultStatus::running:
		if(!(analysis->isRunningImg()))
			analysis->setResults(results, status, progress);
		break;

	default:
		analysis->setResults(results, status, progress);
		break;
	}
}

void EngineRepresentation::checkForComputedColumns(const Json::Value & results)
{
	if(results.isArray())
	{
		for(const Json::Value & row : results)
			checkForComputedColumns(row);
	}
	else if(results.isObject())
	{
		auto members = results.getMemberNames();
		std::set<std::string> memberset(members.begin(), members.end());

		if(memberset.count("columnName") > 0 && memberset.count("columnType") > 0 && memberset.count("dataChanged") > 0)
		{
			Log::log() << "The analysis reply contained information on changed computed columns: " << results.toStyledString() << std::endl;

			//jaspColumnType	columnType	= jaspColumnTypeFromString(results["columnType"].asString()); This would work if jaspColumn wasn't defined in jaspColumn.h and Windows would not need to have that separately in a DLL... But it isn't really needed here anyway.
			std::string		columnName	= results["columnName"].asString();
			bool			dataChanged	= results["dataChanged"].asBool();
			bool			typeChanged	= results["typeChanged"].asBool();

			emit computeColumnSucceeded(tq(columnName), "", dataChanged);

			if(typeChanged)
				emit columnDataTypeChanged(tq(columnName));
		}
		else
			for(const std::string & member : members)
				checkForComputedColumns(results[member]);
	}
}

void EngineRepresentation::handleRunningAnalysisStatusChanges()
{
	if (_engineState != engineState::analysis || _idRemovedAnalysis >= 0)
		return;

	if(		(_analysisInProgress->isEmpty() || _analysisInProgress->isAborted() )
			&& _analysisAborted != _analysisInProgress)
		abortAnalysisInProgress(_analysisInProgress->isEmpty());
}

void EngineRepresentation::killEngine()
{
	Log::log() << "Killing Engine #" << channelNumber() << std::endl; //" and " << (disconnectFinished ? "disconnecting" : "leaving attached") << " it's finished signal." << std::endl;

	if(_analysisInProgress)
		abortAnalysisInProgress(true);

	setState(engineState::killed);

	if(_slaveProcess)
	{
		//Always disconnect finished because if the engine is to be killed by JASP jasp should not show a "crashed" msgbox
		//if(disconnectFinished)
			//We must make sure we do not get a popup, and while processFinished checks for "killed" or not it doesnt help that that needs the eventloop to be processed
			//I want pause and resume all engines to be done in a single function call without returning to the eventloop, so we just disconnect "finished" if we want to kill the engine.
		disconnect(_slaveProcess, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished),	this, &EngineRepresentation::processFinished);

		_slaveProcess->kill();
		_slaveProcess->deleteLater();
		_slaveProcess = nullptr;
	}

	EngineRepresentation::processFinished();
}

void EngineRepresentation::stopEngine()
{
	Log::log() << "Engine #" << _channelNumber << " stopEngine(), _stopRequested will be set" << std::endl;
	_stopRequested = true;
}

void EngineRepresentation::shutEngineDown()
{

	Log::log() << "Engine #" << _channelNumber << " shutEngineDown()" << std::endl;

	if(_analysisInProgress)
	{
		Log::log() << "An analysis is running, so we abort it and wait for the response." << std::endl;
		abortAnalysisInProgress(true);	

		stopEngine();
		
		//Wait for the engine to get the message.
		while(_analysisAborted && _analysisInProgress && _abortTime + ENGINE_KILLTIME > Utils::currentMillis())
			processReplies();
		
		if(!killed() && !stopped())
			killEngine();
	}
	else
	{
		size_t stopTime = Utils::currentMillis();
		stopEngine();
		
		while(!stopped() && stopTime + ENGINE_KILLTIME > Utils::currentMillis())
			processReplies();
		
		if(!stopped())
			killEngine();
	}
}

void EngineRepresentation::sendStopEngine()
{
	Json::Value json		= Json::Value(Json::objectValue);
	setState(engineState::stopRequested);
	json["typeRequest"]		= engineStateToString(_engineState);

	Log::log() << "informing engine #" << channelNumber() << " that it ought to stop" << std::endl;

	sendString(json.toStyledString());
}

void EngineRepresentation::restartEngine(QProcess * jaspEngineProcess)
{
	Log::log() << "informing engine #" << channelNumber() << " that it ought to restart" << std::endl;

	if(_slaveProcess != nullptr && _slaveProcess != jaspEngineProcess)
	{
		Log::log() << "Killing and deleting old engine process and replacing it with fresh one" << std::endl;

		if(jaspEngineStillRunning())
			shutEngineDown();

		_slaveProcess->kill();
		_slaveProcess->deleteLater();

		if(_engineState != engineState::killed && _engineState != engineState::stopped)
			Log::log() << "EngineRepresentation::restartEngine says: Engine already had jaspEngine process that is now replaced!" << std::endl;
	}

	sendString("");
	cleanUpAfterClose();
	setSlaveProcess(jaspEngineProcess);

	setState(engineState::initializing);
}

int EngineRepresentation::idleFor() const
{
	return _idleStartSecs >= 0 ? Utils::currentSeconds() - _idleStartSecs : 0;
}

bool EngineRepresentation::isBored() const 
{ 
	
	return _idleStartSecs != -1 && (_idleStartSecs + ENGINE_BORED_SHUTDOWN < Utils::currentSeconds());
}

bool EngineRepresentation::busyWithData() const
{
	switch(_engineState)
	{
	case engineState::analysis:
	case engineState::computeColumn:
	case engineState::filter:
	case engineState::rCode:
		return true;

	default:
		return false;
	}
}

void EngineRepresentation::pauseEngine(bool unloadData)
{
	if(initializing())
		return;
	
	_pauseRequested  = true;
	_pauseUnloadData = unloadData;
	abortAnalysisInProgress(true);
}

void EngineRepresentation::sendPauseEngine()
{
	Json::Value json		= Json::Value(Json::objectValue);
	setState(engineState::pauseRequested);
	json["typeRequest"]		= engineStateToString(_engineState);
	json["unloadData"]		= _pauseUnloadData;

	Log::log() << "informing engine #" << channelNumber() << " that it ought to pause for a bit" << std::endl;

	sendString(json.toStyledString());
}

void EngineRepresentation::resumeEngine(bool setResuming)
{
	if(_engineState != engineState::paused && _engineState != engineState::stopped && _engineState != engineState::initializing)
		throw unexpectedEngineReply("Attempt to resume engine #" + std::to_string(channelNumber()) + " made but it isn't paused, initializing or stopped");

	if(setResuming)
		setState(engineState::resuming);

	_pauseRequested			= false;
	Json::Value json		= Json::Value(Json::objectValue);
	json["typeRequest"]		= engineStateToString(engineState::resuming);

	addSettingsToJson(json);

	Log::log() << "informing engine #" << channelNumber() << " that it may resume." << std::endl;

	sendString(json.toStyledString());
}

void EngineRepresentation::processEnginePausedReply()
{
	Log::log() << "EngineRepresentation::processEnginePausedReply() for engine #" << channelNumber() << std::endl;
	checkIfExpectedReplyType(engineState::pauseRequested);

	setState(engineState::paused);
}

void EngineRepresentation::processEngineResumedReply()
{
	Log::log() << "EngineRepresentation::processEngineResumedReply() for engine #" << channelNumber() << std::endl;

	if(_engineState != engineState::resuming && _engineState != engineState::initializing)
		throw unexpectedEngineReply("Received an unexpected engine #" + std::to_string(channelNumber()) + " resumed reply!");

	//_settingsChanged = true; //Make sure we send the settings at least once. We now do this be also sending the settings in the resume request

	setState(engineState::idle);
}

void EngineRepresentation::processEngineStoppedReply()
{
	Log::log() << "EngineRepresentation::processEngineStoppedReply() for engine #" << channelNumber() << std::endl;
	checkIfExpectedReplyType(engineState::stopRequested);

	setState(engineState::stopped);

	disconnect(_slaveFinishedConnection);
}

void EngineRepresentation::runModuleInstallRequestOnProcess(Json::Value request)
{
	setState(engineState::moduleInstallRequest);
	request["typeRequest"]	= engineStateToString(_engineState);

	_requestModName	= request["moduleName"].asString();

	sendString(request.toStyledString());
}

void EngineRepresentation::runModuleLoadRequestOnProcess(Json::Value request)
{
	_moduleLoaded = false;
	setState(engineState::moduleLoadRequest);
	request["typeRequest"]	= engineStateToString(_engineState);

	_requestModName	= request["moduleName"].asString();

	sendString(request.toStyledString());
}

void EngineRepresentation::processModuleRequestReply(Json::Value & json)
{
	checkIfExpectedReplyType(_engineState);

	setState(engineState::idle);

	moduleStatus moduleRequest	= moduleStatusFromString(json["moduleRequest"].asString());
	bool succes					= json["succes"].asBool();
	QString moduleName			= QString::fromStdString(json["moduleName"].asString());
	auto getError				= [&](){ return QString::fromStdString(json.get("error", "Unknown error").asString()); };

	if(_requestModName != fq(moduleName))
		throw std::runtime_error("Received answer for a module request, as expected, but from the wrong module... '" + _requestModName + "' was expected, but got '" + fq(moduleName) + "' instead.");
	_requestModName = "";

	switch(moduleRequest)
	{
	case moduleStatus::installNeeded:
		if(succes)	emit moduleInstallationSucceeded(moduleName);
		else		emit moduleInstallationFailed(moduleName, getError());
		break;

	case moduleStatus::loading:
		if(succes)	{ emit moduleLoadingSucceeded(moduleName, channelNumber());				_dynModName = fq(moduleName);		_moduleLoaded = true;	}
		else		{ emit moduleLoadingFailed(moduleName, getError(), channelNumber());	_dynModName = "";											}
		break;

	default:
		throw std::runtime_error("Unsupported module request reply to EngineRepresentation::processModuleRequestReply!");
	}

	// If the engine isn't loading then probably an error occured like "cannot open connection" and this is often solved by restarting the engine,
	// so unless someone figures out why the connections sometimes isnt there this is a reasonable workaround
	if(!succes && moduleRequest == moduleStatus::loading)
		emit stopAndDestroyEngine(this);

	if(moduleRequest == moduleStatus::installNeeded)
		emit stopModuleEngine(moduleName);
}

void EngineRepresentation::sendLogCfg()
{
	Log::log() << "EngineRepresentation::sendLogCfg()" << std::endl;

	if(_engineState != engineState::idle)
		throw std::runtime_error("EngineRepresentation::sendLogCfg() expects to be run from an idle engine.");

	setState(engineState::logCfg);
	Json::Value msg		= Log::createLogCfgMsg();
	msg["typeRequest"]	= engineStateToString(_engineState);

	sendString(msg.toStyledString());
}

void EngineRepresentation::processLogCfgReply()
{
	setState(engineState::idle);

	emit logCfgReplyReceived(this);
}

std::string EngineRepresentation::currentStateForDebug() const
{
	try {
		std::stringstream out;

		out << "**Engine # " << channelNumber() << "** process ";

		if(_slaveCrashed)		out << "*crashed*";
		else if(!_slaveProcess)	out << "*does not exist (anymore)*";
		else
			switch(_slaveProcess->state())
			{
			case QProcess::ProcessState::NotRunning:	out << " is *not running*";		break;
			case QProcess::ProcessState::Running:		out << " is *running*";			break;
			case QProcess::ProcessState::Starting:		out << " is *starting*";		break;
			}

		out << " and it's state is *" << engineStateToString(_engineState) << "*.";

		if(_engineState == engineState::analysis)
			try			{	out << " Analysis is " << (_analysisInProgress ? ("**" + _analysisInProgress->name() + "** with status *" + Analysis::statusToString(_analysisInProgress->status()) + "*") : "*???*") << ""; }
			catch(...)	{	out << " Something is wrong with the analysis..."; }

		return out.str();
	}
	catch (...)
	{
		return "EngineRepresentation::currentStateForDebug() didn't work...";
	}
}

void EngineRepresentation::abortAnalysisInProgress(bool restartAfterwards)
{
	if(_engineState == engineState::analysis && _analysisInProgress != nullptr)
	{
		if(_analysisInProgress->status() != Analysis::Status::Aborting && _analysisInProgress->status() != Analysis::Status::Aborted)
			_analysisInProgress->setStatus(Analysis::Status::Aborting);

		runAnalysisOnProcess(_analysisInProgress);

		_analysisAborted	= _analysisInProgress;
		_abortTime			= Utils::currentMillis(); //We'll give it some time to abort, so we need to remember when we gave the order.
		_abortAndRestart	= restartAfterwards;
	}
}

void EngineRepresentation::settingsChanged()
{
	Log::log() << "void EngineRepresentation::settingsChanged()" << std::endl;
	_settingsChanged = true;
	abortAnalysisInProgress(true);
}

void EngineRepresentation::setRunsAnalysis(bool runsAnalysis)
{
	if (_runsAnalysis == runsAnalysis)
		return;

	_runsAnalysis = runsAnalysis;
	emit runsAnalysisChanged(_runsAnalysis);
}

void EngineRepresentation::setRunsUtility(bool runsUtility)
{
	if (_runsUtility == runsUtility)
		return;

	_runsUtility = runsUtility;
	emit runsUtilityChanged(_runsUtility);
}

void EngineRepresentation::setRunsRCmd(bool runsRCmd)
{
	if (_runsRCmd == runsRCmd)
		return;

	_runsRCmd = runsRCmd;
	emit runsRCmdChanged(_runsRCmd);
}

void EngineRepresentation::sendSettings()
{
	Log::log() << "EngineRepresentation::sendSettings()" << std::endl;

	if(_engineState != engineState::idle)
		throw std::runtime_error("EngineRepresentation::sendSettings() expects to be run from an idle engine.");

	setState(engineState::settings);
	Json::Value msg			= Json::objectValue;
	msg["typeRequest"]		= engineStateToString(_engineState);
	addSettingsToJson(msg);
	sendString(msg.toStyledString());

	_settingsChanged = false;
}

void EngineRepresentation::sendReloadData()
{
	Log::log() << "EngineRepresentation::sendReloadData()" << std::endl;

	if(_engineState != engineState::idle)
		throw std::runtime_error("EngineRepresentation::sendReloadData() expects to be run from an idle engine.");

	setState(engineState::reloadData);
	Json::Value msg			= Json::objectValue;
	msg["typeRequest"]		= engineStateToString(_engineState);

	sendString(msg.toStyledString());
}

void EngineRepresentation::addSettingsToJson(Json::Value & msg)
{
	msg["ppi"]					=	 PreferencesModel::prefs()->plotPPI();
	msg["developerMode"]		=	 PreferencesModel::prefs()->developerMode();
	msg["imageBackground"]		= fq(PreferencesModel::prefs()->plotBackground());
	msg["languageCode"]			= fq(PreferencesModel::prefs()->languageCode());
	msg["GITHUB_PAT"]			= fq(PreferencesModel::prefs()->githubPatResolved());
	msg["numDecimals"]			=	 PreferencesModel::prefs()->numDecimals();
	msg["fixedDecimals"]		=	 PreferencesModel::prefs()->fixedDecimals();
	msg["exactPValues"]			=	 PreferencesModel::prefs()->exactPValues();
	msg["normalizedNotation"]	=	 PreferencesModel::prefs()->normalizedNotation();
}

void EngineRepresentation::processSettingsReply()
{
	setState(engineState::idle);
	restartAbortedAnalysis();
}

void EngineRepresentation::restartAbortedAnalysis()
{
	if(_analysisAborted && _abortAndRestart)
		_analysisAborted->run();

	_analysisAborted = nullptr;
}

bool EngineRepresentation::willProcessAnalysis(Analysis * analysis)
{
	if(_stopRequested || !channel() || !analysis || !idle() || !analysis->shouldRun())
		return false;

	const std::string modName = analysis->dynamicModule()->name();

	return	(	runsAnalysis()					&& _dynModName == modName && _moduleLoaded)
			|| (analysis->utilityRunAllowed()	&& runsUtility()		 );
}

void canIRegisterModule(const std::string & name);

bool EngineRepresentation::idleSoon() const
{
	switch(_engineState)
	{
	case engineState::resuming:
	case engineState::paused:
	case engineState::initializing:
	case engineState::settings:
	case engineState::logCfg:
	case engineState::moduleLoadRequest:
	case engineState::reloadData:
	case engineState::idle:
		return true;
	
	default:
		return false;
	}
}

void EngineRepresentation::setState(engineState newState)
{
	if (_engineState == newState)
		return;

	_engineState = newState;

	emit stateChanged();
}

const QString EngineRepresentation::analysisStatus() const
{
	return _analysisInProgress ? _analysisInProgress->statusQ() : "NA";
}

