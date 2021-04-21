#ifndef LOG_H
#define LOG_H

#include <string>
#include "enumutilities.h"
#include "jsonredirect.h"
#include <ostream>

DECLARE_ENUM(logType,  cout, file, null);
DECLARE_ENUM(logError, noProblem, fileNotOpen, filePathNotSet);



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
