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

#ifdef _WIN32
void openConsoleOutput()
{

		FreeConsole();

		// create a separate new console window
		AllocConsole();

		// attach the new console to this application's process
		AttachConsole(GetCurrentProcessId());

		// reopen the std I/O streams to redirect I/O to the new console
		freopen("CONOUT$", "w", stdout);
		freopen("CONOUT$", "w", stderr);
}
#endif

int main(int argc, char *argv[])
{
	if(argc > 2)
	{
		unsigned long slaveNo	= strtoul(argv[1], NULL, 10);
		unsigned long parentPID = strtoul(argv[2], NULL, 10);

#ifdef _WIN32
		// openConsoleOutput(); //uncomment to have a console window open per Engine that shows you std out and cerr. (On windows only, on unixes you can just run JASP from a terminal)
#endif

		std::cout << "jaspEngine started and has slaveNo " << slaveNo << " and it's parent PID is " << parentPID << std::endl;

		Engine e(slaveNo, parentPID);
		e.run();

		std::cout << "jaspEngine " << slaveNo << " child of " << parentPID << " stops." << std::endl;
		return 0;
	}

	std::cout << "jaspEngine does not have all required information to run, it needs slaveNo as first argument and parent PID as second!" << std::endl;
	return 1;
}
