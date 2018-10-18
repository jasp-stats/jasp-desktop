#include "enginedefinitions.h"


std::string engineStateToString(engineState e)
{
	switch(e)
	{
	case engineState::idle:				return "idle";
	case engineState::analysis:			return "analysis";
	case engineState::filter:			return "filter";
	case engineState::rCode:			return "rCode";
	case engineState::computeColumn:	return "computeColumn";
	default:			throw std::logic_error("When you define new engineStates you should add them to engineStateToString and engineStateFromString!");
	}
}

engineState engineStateFromString(std::string e)
{
	if(e == "idle")					return engineState::idle;
	else if(e == "analysis")		return engineState::analysis;
	else if(e == "filter")			return engineState::filter;
	else if(e == "rCode")			return engineState::rCode;
	else if(e == "computeColumn")	return engineState::computeColumn;
	else							throw std::logic_error("Unknown engineState " + e +"! When you define new engineStates you should add them to engineStateToString and engineStateFromString!");
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
	default:				throw std::logic_error("When you define new performTypes you should add them to performTypeToString and performTypeFromString!");
	}
}

performType performTypeFromString(std::string p)
{
	if(p == "init")			return performType::init;
	else if(p == "run")		return performType::run;
	else if(p == "abort")	return performType::abort;
	else if(p == "saveImg")	return performType::saveImg;
	else if(p == "editImg")	return performType::editImg;
	else					throw std::logic_error("Unknown perform " + p +"! When you define new performTypes you should add them to performTypeToString and performTypeFromString!");
}

std::string analysisResultStatusToString(analysisResultStatus a)
{	
	switch(a)
	{
	case analysisResultStatus::error:		return "error";
	case analysisResultStatus::exception:	return "exception";
	case analysisResultStatus::imageSaved:	return "imageSaved";
	case analysisResultStatus::imageEdited:	return "imageEdited";
	case analysisResultStatus::complete:	return "complete";
	case analysisResultStatus::inited:		return "inited";
	case analysisResultStatus::running:		return "running";
	case analysisResultStatus::changed:		return "changed";
	case analysisResultStatus::waiting:		return "waiting";
	default:								throw std::logic_error("When you define new analysisResultStatuss you should add them to analysisResultStatusToString!");
	}
}

analysisResultStatus analysisResultStatusFromString(std::string p)
{
	if(p == "error")			return analysisResultStatus::error;
	else if(p == "exception")	return analysisResultStatus::exception;
	else if(p == "imageSaved")	return analysisResultStatus::imageSaved;
	else if(p == "imageEdited")	return analysisResultStatus::imageEdited;
	else if(p == "complete")	return analysisResultStatus::complete;
	else if(p == "inited")		return analysisResultStatus::inited;
	else if(p == "running")		return analysisResultStatus::running;
	else if(p == "changed")		return analysisResultStatus::changed;
	else if(p == "waiting")		return analysisResultStatus::waiting;
	else						throw std::logic_error("When you define new analysisResultStatuses you should add them " + p + " to analysisResultStatusToString and analysisResultStatusFromString!");
}
