#ifndef LOG_H
#define LOG_H

#include <string>
#include "enumutilities.h"
#include <json/json.h>
#include <ostream>

DECLARE_ENUM(logType,  cout, file, null);
DECLARE_ENUM(logError, noProblem, fileNotOpen, filePathNotSet);

///
/// As might be obvious from the name this is the main class for logging.
/// It can be set in three different states, in development default is to log everything to cout, which shows all output in the application output of qt creator
/// For the released version the default is to a "/dev/null" equivalent, aka everything is dropped to waste minimal time on formatting etc.
/// In both cases a setting can be turned on to write it all to files, then a file for Desktop is created and one for each running engine. 
/// They will all have the exact same timestamp in the filename to easily group them.
/// For almost all messages a timestamp and identifier is added. But because the output from R (and some other places) comes in in pieces we omit that there.
/// 
class Log
{
public:
	static std::ostream & log(bool addTimestamp = true);

	static std::string	logFileNameBase;

	static void			initRedirects();
	static void			setLogFileName(const std::string & filePath);

	static void			setDefaultDestination(logType newDestination);

	static void			setLoggingToFile(bool logToFile);
	static void			setWhere(logType where);
	static void			setEngineNo(int num)	{ _engineNo = num; }

	static Json::Value	createLogCfgMsg();
	static void			parseLogCfgMsg(const Json::Value & json);

	static std::string	whereStr() { return logTypeToString(_where); }

	static bool			toCout() { return _where == logType::cout; }

private:
						Log() { }
	static void			redirectStdOut();
	static const char * getTimestamp();

	static logType		_default;
	static logType		_where;
	static std::string	_logFilePath;
	static logError		_logError;
	static int			_stdoutfd,
						_engineNo;

	static const char*	_nullStream;
};

std::ostream & operator<<(std::ostream & os, const std::wstring & wStr);

#endif // LOG_H
