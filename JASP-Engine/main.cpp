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

int main(int argc, char *argv[])
{
	if(argc > 2)
	{
		unsigned long slaveNo	= strtoul(argv[1], NULL, 10);
		unsigned long parentPID = strtoul(argv[2], NULL, 10);

		//sleep(10000000);

		Engine *e = new Engine(slaveNo, parentPID);
		e->run();

#ifdef JASP_DEBUG
		std::cout << "jaspEngine " << slaveNo << " child of " << parentPID << " stops gracefully." << std::endl;
#endif
		return 0;
	}

	return 1;
}
