#ifndef ENGINEDEFINITIONS_H
#define ENGINEDEFINITIONS_H

#include "enumutilities.h"

DECLARE_ENUM(engineState,			initializing, idle, analysis, filter, rCode, computeColumn, moduleRequest, pauseRequested, paused, resuming, stopRequested, stopped, logCfg, settings, killed);
DECLARE_ENUM(performType,			init, run, abort, saveImg, editImg, rewriteImgs);
DECLARE_ENUM(analysisResultStatus,	validationError, fatalError, imageSaved, imageEdited, imagesRewritten, complete, inited, running, changed, waiting);
DECLARE_ENUM(moduleStatus,			initializing, installNeeded, installModPkgNeeded, loadingNeeded, unloadingNeeded, readyForUse, error);
DECLARE_ENUM(engineAnalysisStatus,	empty, toInit, initing, inited, toRun, running, changed, complete, error, exception, aborted, stopped, saveImg, editImg, rewriteImgs, synchingData);

struct unexpectedEngineReply  : public std::runtime_error
{
	unexpectedEngineReply(std::string msg)															: std::runtime_error(msg) {}
	unexpectedEngineReply(engineState unexpectedState, std::string msg = "Engine got unexpected")	: std::runtime_error(msg + ": " + engineStateToString(unexpectedState)) {}
	unexpectedEngineReply(engineState unexpectedState, int engineNo, std::string extra = "")		: std::runtime_error("Engine " + std::to_string(engineNo) + " got unexpected reply: " + engineStateToString(unexpectedState) + extra) {}
	const char* what() const noexcept override; //Put that

	static void checkIfExpected(engineState expectedReplyState, engineState currentState, int channelNo);
};



#endif // ENGINEDEFINITIONS_H
