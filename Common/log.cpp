#define ENUM_DECLARATION_CPP
#include "log.h"
#include <cstdio>
#include <iostream>
#include <chrono>
#ifdef WIN32
#include <io.h>
#endif

#include <fstream>
#include "utils.h"
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/null.hpp>
#include <codecvt>
#include <fstream>

static std::ofstream _logFile;// = bofstream();

std::string Log::logFileNameBase	= "";

logType		Log::_where				= logType::cout;
std::string	Log::_logFilePath		= "";
logError	Log::_logError			= logError::noProblem;
int			Log::_stdoutfd			= -1;
int			Log::_engineNo			= -1;


logType		Log::_default			=
#ifdef JASP_DEBUG
	logType::cout;
#else
	logType::null;
#endif

void Log::setDefaultDestination(logType newDestination)
{
	if(newDestination == logType::file) //It doesnt make any sense to have the default non-file logType be file...
		newDestination = logType::cout;

	if(_default == newDestination)
		return;

	bool setNewDefaultToWhere = _where == _default;

	_default = newDestination;

	if(setNewDefaultToWhere)
		redirectStdOut();
}

void Log::setWhere(logType where)
{
	log() << std::flush;

	if(where == _where)
		return;

	_where = where;

	redirectStdOut();
}

void Log::setLoggingToFile(bool logToFile)
{
	setWhere(logToFile ? logType::file : _default);
}

void Log::setLogFileName(const std::string & filePath)
{
	if(_logFilePath == filePath)
		return;

	_logFilePath = filePath;

	if(_where == logType::file)
		redirectStdOut();
}

void Log::initRedirects()
{
	_where			= _default;
}

void Log::redirectStdOut()
{
	switch(_where)
	{
	default:
		break;

	case logType::file:
	{
		if(_logFilePath == "")
		{
			_logError = logError::filePathNotSet;
			_where    = _default;
			redirectStdOut();
			return;
		}

		//_currentFile = freopen(_logFilePath.c_str(), "a", stdout);
		//if(!_currentFile)

		_logFile.open(_logFilePath.c_str(), std::ios_base::app | std::ios_base::out);

		if(_logFile.fail())
		{
			_logError	= logError::fileNotOpen;
			_where		= _default;
			redirectStdOut();
			return;
		}
		break;
	};
	}

	_logError = logError::noProblem;
}

Json::Value	Log::createLogCfgMsg()
{
	Json::Value json	= Json::objectValue;

	json["where"]		= logTypeToString(_where);

	return json;
}

void Log::parseLogCfgMsg(const Json::Value & json)
{
	setWhere(logTypeFromString(json["where"].asString()));
}

const char * Log::getTimestamp()
{
	static char buf[13];
	static auto startTime = std::chrono::time_point_cast<std::chrono::milliseconds>(std::chrono::system_clock::now());

	std::chrono::milliseconds duration = std::chrono::time_point_cast<std::chrono::milliseconds>(std::chrono::system_clock::now()) - startTime;

	long	durationMilli	= (std::chrono::duration_cast<std::chrono::milliseconds>(duration)).count(),
			durationSec		= durationMilli / 1000,
			durationMin		= durationSec	/ 60,
			durationHour	= durationMin	/ 60;

	int	milli	= durationMilli % 1000,
		sec		= durationSec	% 60,
		min		= durationMin	% 60,
		hour	= durationHour	% 60;

	std::sprintf(buf, "%02u:%02u:%02u.%03u", hour, min, sec, milli);

	return buf;
}

std::ostream & Log::log(bool addTimestamp)
{
	switch(_where)
	{
#ifndef __clang__
#ifdef __GNUG__
	default:				//Gcc is stupid and is not aware that the next three cases cover all
#endif
#endif
	case logType::cout:
	{
		if (addTimestamp) std::cout << ( _engineNo < 0 ? std::string("Desktop:\t") : "Engine#" + std::to_string(_engineNo) + ":\t");
		return std::cout;
	}
	case logType::null:
	{
		static boost::iostreams::stream<boost::iostreams::null_sink> nullstream((boost::iostreams::null_sink())); //https://stackoverflow.com/questions/8243743/is-there-a-null-stdostream-implementation-in-c-or-libraries
		return nullstream;
	}
	case logType::file:
		if (addTimestamp) _logFile << Log::getTimestamp() << ": ";
		return _logFile;
	}
}

std::ostream & operator<<(std::ostream & os, const std::wstring & wStr)
{
	static std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> strCvt;
	
	return os << strCvt.to_bytes(wStr);
};
