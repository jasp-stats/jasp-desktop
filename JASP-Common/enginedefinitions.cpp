#include "enginedefinitions.h"


std::string engineStateToString(engineState e)
{
	switch(e)
	{
	case engineState::idle:						return "idle";
	case engineState::rCode:					return "rCode";
	case engineState::filter:					return "filter";
	case engineState::analysis:					return "analysis";
	case engineState::computeColumn:			return "computeColumn";
	case engineState::moduleRequest:			return "moduleRequest";
	default:									throw std::logic_error("When you define new engineStates you must add them to engineStateToString!");
	}
}

engineState engineStateFromString(std::string e)
{
	if(e == "idle")					return engineState::idle;
	if(e == "rCode")					return engineState::rCode;
	if(e == "filter")					return engineState::filter;
	if(e == "analysis")				return engineState::analysis;
	if(e == "computeColumn")			return engineState::computeColumn;
	if(e == "moduleRequest")			return engineState::moduleRequest;

	throw std::logic_error("Unknown engineState " + e +"! When you define new engineStates you must add it to engineStateFromString!");
}

std::string performTypeToString(performType p)
{
	switch(p)
	{
	case performType::init:		return "init";
	case performType::run:		return "run";
	case performType::abort:	return "abort";
	case performType::saveImg:	return "saveImg";
	case performType::editImg:	return "editImg";
	default:					throw std::logic_error("When you define new performTypes you must add them to performTypeToString!");
	}
}

performType performTypeFromString(std::string p)
{
	if(p == "init")		return performType::init;
	if(p == "run")		return performType::run;
	if(p == "abort")	return performType::abort;
	if(p == "saveImg")	return performType::saveImg;
	if(p == "editImg")	return performType::editImg;

	throw std::logic_error("Unknown perform " + p +"! When you define new performTypes you must add it to performTypeFromString!");
}

std::string analysisResultStatusToString(analysisResultStatus a)
{	
	switch(a)
	{
	case analysisResultStatus::error:		return "error";
	case analysisResultStatus::inited:		return "inited";
	case analysisResultStatus::running:		return "running";
	case analysisResultStatus::changed:		return "changed";
	case analysisResultStatus::waiting:		return "waiting";
	case analysisResultStatus::complete:	return "complete";
	case analysisResultStatus::exception:	return "exception";
	case analysisResultStatus::imageSaved:	return "imageSaved";
	case analysisResultStatus::imageEdited:	return "imageEdited";
	default:								throw std::logic_error("When you define new analysisResultStatuses you must add them to analysisResultStatusToString!");
	}
}

analysisResultStatus analysisResultStatusFromString(std::string p)
{
	if(p == "error")		return analysisResultStatus::error;
	if(p == "inited")		return analysisResultStatus::inited;
	if(p == "running")		return analysisResultStatus::running;
	if(p == "changed")		return analysisResultStatus::changed;
	if(p == "waiting")		return analysisResultStatus::waiting;
	if(p == "complete")		return analysisResultStatus::complete;
	if(p == "exception")	return analysisResultStatus::exception;
	if(p == "imageSaved")	return analysisResultStatus::imageSaved;
	if(p == "imageEdited")	return analysisResultStatus::imageEdited;

	throw std::logic_error("When you define new analysisResultStatuses you must add it (" + p + ") to analysisResultStatusFromString!");
}


std::string moduleStatusToString(moduleStatus status)
{
	switch(status)
	{
	case moduleStatus::installNeeded:	return "installNeeded";
	case moduleStatus::loadingNeeded:	return "loadingNeeded";
	case moduleStatus::readyForUse:		return "readyForUse";
	case moduleStatus::error:			return "error";
	default:							throw std::logic_error("Add new moduleStatusses to DynamicModule::moduleStatusToString!");
	}
}

moduleStatus moduleStatusFromString(std::string status)
{
	if(status == "installNeeded")	return moduleStatus::installNeeded;
	if(status == "loadingNeeded")	return moduleStatus::loadingNeeded;
	if(status == "readyForUse")		return moduleStatus::readyForUse;
	if(status == "error")			return moduleStatus::error;

	throw std::logic_error("Add new moduleStatusses " + status + " to DynamicModule::moduleStatusFromString!");
}
