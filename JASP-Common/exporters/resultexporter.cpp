//
// Copyright (C) 2016 University of Amsterdam
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

#include "resultexporter.h"
#include "utils.h"
#include <boost/nowide/fstream.hpp>

void ResultExporter::saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback)
{

	int maxSleepTime = 5000;
	int sleepTime = 100;
	int delay = 0;

	while (package->isReady() == false)
	{
		if (delay > maxSleepTime)
			break;

		Utils::sleep(sleepTime);
		delay += sleepTime;
	}


	boost::nowide::ofstream outfile(path.c_str(), std::ios::out);

	outfile << package->analysesHTML;
	outfile.flush();
	outfile.close();

	progressCallback("Export Html Set", 100);
}


