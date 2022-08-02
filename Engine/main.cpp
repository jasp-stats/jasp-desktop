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
	if(argc > 4)
	{
		unsigned long	slaveNo			= wcstoul(argv[1], NULL, 10),
						parentPID		= wcstoul(argv[2], NULL, 10);
		std::string		logFileBase		= Utils::wstringToString(argv[3]),
						logFileWhere	= Utils::wstringToString(argv[4]);
			
#else
int main(int argc, char *argv[])
{
	if(argc > 4)
	{
		unsigned long	slaveNo			= strtoul(argv[1], NULL, 10),
						parentPID		= strtoul(argv[2], NULL, 10);
		std::string		logFileBase		= argv[3],
						logFileWhere	= argv[4];

#endif

		Log::logFileNameBase = logFileBase;
		Log::initRedirects();
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
		std::cout << "Engine started in R (Module) Library Fixer mode because it received a single argument: '" << argv[1] << "'." << std::endl;

		Engine e(0, 0); //It needs to start to make sure rbridge functions work

#ifdef _WIN32
		_moduleLibraryFixer(Utils::wstringToString(argv[1]), true, true);
#else
		_moduleLibraryFixer(argv[1], true, true);
#endif

		exit(0);
	}
#ifdef _WIN32
	else if(argc == 3)
	{
		std::string arg1(Utils::wstringToString(argv[1])), arg2(Utils::wstringToString(argv[2]));
		const std::string junctionCollectArg("--collectJunctions"), junctionRecreateArg("--recreateJunctions"), junctionRemoveArg("--removeJunctions");
		
		if(arg1 == junctionRemoveArg || arg1 == junctionRecreateArg) //Also remove the old modules if it already exists and we are asked to recreate them, because it might be the old ones (previous install)
		{
			std::string junctionsCreationLog("junctions-recreated-successfully.log");
			std::filesystem::path	junctionsCreationLogPath = Utils::osPath(junctionsCreationLog);
			
			if(exists(junctionsCreationLogPath))
				remove(junctionsCreationLogPath);

			std::string modulesFolder("Modules");
			std::cout << "Engine started to remove the Modules folder" << std::endl;
			std::filesystem::path	modulesPath	= Utils::osPath(modulesFolder);
			
			if(exists(modulesPath)) {
				for(const std::filesystem::directory_entry & entry : std::filesystem::directory_iterator(modulesPath))
					if(entry.path().string().find("\\jasp") != std::string::npos)
						remove_all(entry);
				
			}
			else if(arg1 == junctionRemoveArg)
				std::cout << "Error: Could not find the Modules folder" << std::endl;
			
		}
		
		if(arg1 == junctionCollectArg || arg1 == junctionRecreateArg)
		{
			std::cout << "Engine started for junctions, got folder '" << arg2 << "'!" << std::endl;
			rbridge_junctionHelper(arg1 == junctionCollectArg, arg2);
		}
		
		exit(0);
	}
#endif

	std::cout << "Engine started in testing mode because it didn't receive any count of arguments otherwise, (1, 3 or 4), it got " << argc << " instead." << std::endl;

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
