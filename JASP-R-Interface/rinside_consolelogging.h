#ifndef RINSIDE_CONSOLELOGGING_H
#define RINSIDE_CONSOLELOGGING_H
#include "RInside/RInside.h"

//This doesnt work on Windows because there is no RInterface.h there..
class RInside_ConsoleLogging : public Callbacks
{
public:
	void		WriteConsole(const std::string & line, int type) override;
	bool		has_WriteConsole() { return true; }
	std::string	getConsoleOutput();
	void		clearConsoleBuffer() { outputBuffer.str(std::string("")); }


private:
	std::ostringstream outputBuffer;
};

#endif // RINSIDE_CONSOLELOGGING_H
