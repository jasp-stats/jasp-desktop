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

#include "engine.h"
#include "timers.h"
#include "log.h"
#include <iostream>
#include <fstream>
#include <codecvt>
#include "otoolstuff.h"
#include "rbridge.h"
#include "utils.h"
#include "dirs.h"
#include "boost/iostreams/stream.hpp"
#include <boost/iostreams/device/null.hpp>

#ifdef _WIN32
void openConsoleOutput(unsigned long slaveNo, unsigned parentPID)
{

		FreeConsole();

		// create a separate new console window
		AllocConsole();

		// attach the new console to this application's process
		AttachConsole(GetCurrentProcessId());

		static std::string title = "jaspEngine " + std::to_string(slaveNo) + " for parent JASP at PID " + std::to_string(parentPID);
		SetConsoleTitleA(title.c_str());

		// reopen the std I/O streams to redirect I/O to the new console
		freopen("CONOUT$", "w", stdout);
		freopen("CONOUT$", "w", stderr);
}
#endif


#ifdef _WIN32
int wmain( int argc, wchar_t *argv[ ], wchar_t *envp[ ] )
{
	if(argc == 3)
	{
		std::string arg1(Utils::wstringToString(argv[1])), arg2(Utils::wstringToString(argv[2]));
		if(arg1 == "--collectJunctions")
		{
			Log::log() << "Engine started for junctions, got folder '" << arg2 << "'!" << std::endl;
			rbridge_junctionHelper(true, arg2, "", "");
			exit(0);
		}
	}
	else if(argc == 5)
	{
		std::string arg1(Utils::wstringToString(argv[1])), arg2(Utils::wstringToString(argv[2])), arg3(Utils::wstringToString(argv[3])), arg4(Utils::wstringToString(argv[4]));
		if(arg1 == "--recreateJunctions")
		{
			Log::log() << "Engine started for junctions, got folder '" << arg2 << "'!" << std::endl;
			rbridge_junctionHelper(false, arg2, arg3, arg4);
			exit(0);
		}
	}

	if(argc > 4)
	{
		unsigned long	slaveNo			= wcstoul(argv[1], NULL, 10),
						parentPID		= wcstoul(argv[2], NULL, 10);
		std::string		logFileBase		= Utils::wstringToString(argv[3]),
						logFileWhere	= Utils::wstringToString(argv[4]);


        if(argc > 5)
            Dirs::setReportingDir(Utils::wstringToString(argv[5]));
			
#else
int main(int argc, char *argv[])
{
	if(argc > 4)
	{
		unsigned long	slaveNo			= strtoul(argv[1], NULL, 10),
						parentPID		= strtoul(argv[2], NULL, 10);
		std::string		logFileBase		= argv[3],
						logFileWhere	= argv[4];


        if(argc > 5)
            Dirs::setReportingDir(argv[5]);

#endif
		static boost::iostreams::stream<boost::iostreams::null_sink> nullstream((boost::iostreams::null_sink())); //https://stackoverflow.com/questions/8243743/is-there-a-null-stdostream-implementation-in-c-or-libraries

		Log::logFileNameBase = logFileBase;
		Log::init(&nullstream);
		Log::setLogFileName(logFileBase + " Engine " + std::to_string(slaveNo) + ".log");
		Log::setWhere(logTypeFromString(logFileWhere));
		Log::setEngineNo(slaveNo);

		Log::log() << "Log and possible redirects initialized!" << std::endl;
		Log::log() << "jaspEngine started and has slaveNo " << slaveNo << " and it's parent PID is " << parentPID << std::endl;
		Log::log() << "Current directory is: '" << std::filesystem::current_path().string() << "'" << std::endl;

		try
		{
			JASPTIMER_START(Engine Starting);
			Engine e(slaveNo, parentPID);
			JASPTIMER_STOP(Engine Starting);

			e.run();

		}
		catch (std::exception & e)
		{
			Log::log() << "Engine had an uncaught exception of: " << e.what() << std::endl;;
			throw e;
		}

		JASPTIMER_PRINTALL();

		Log::log() << "jaspEngine " << slaveNo << " child of " << parentPID << " stops." << std::endl;
		exit(0);
	}
	else if(argc == 2)
	{
		std::string singleArg =
#ifdef _WIN32
			Utils::wstringToString(argv[1]);
#else
			argv[1];
#endif
		std::cout << "Engine started in R (Module) Library Fixer mode because it received a single argument: '" << singleArg << "'." << std::endl;

		Engine e(0, 0); //It needs to start to make sure rbridge functions work

		_moduleLibraryFixer(singleArg, true, true);

		exit(0);
	}

	Log::log() << "Engine started in testing mode because it didn't receive any count of arguments otherwise, (1, 3 or 4), it got " << argc << " instead." << std::endl;

	const char * testFileName = "testFile.txt";
	std::ofstream writeTestFile(testFileName);

	std::cout << "Opening testfile \"" << testFileName << "\" " << (writeTestFile.is_open() ? "Succeeded!" : "Failed!") << std::endl;

	if(writeTestFile.is_open())
	{
		writeTestFile << "Hello beautiful world!" << std::endl;

		std::cout << "After writing something into the testfile the state of the outputstream is: " << (writeTestFile.good() ? "good" : "bad") << "." << std::endl;

		writeTestFile.close();
	}

	std::cout << "Thank you for helping us make JASP better!" << std::endl;

	exit(1);
}
