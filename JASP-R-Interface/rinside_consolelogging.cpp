#include "rinside_consolelogging.h"

void RInside_ConsoleLogging::WriteConsole(const std::string & line, int type)
{
	outputBuffer << line << std::endl;
#ifdef JASP_DEBUG
	std::cout << "RConsole Output: " << line << std::endl;
#endif
}

std::string RInside_ConsoleLogging::getConsoleOutput()
{
	outputBuffer << std::flush;
	return outputBuffer.str();
}

