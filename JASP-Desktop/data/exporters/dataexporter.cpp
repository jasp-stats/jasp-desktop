//
// Copyright (C) 2018 University of Amsterdam
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

#include "dataexporter.h"
#include <boost/filesystem.hpp>
#include <sys/stat.h>
#include "dataset.h"
#include <boost/nowide/fstream.hpp>
#include "stringutils.h"

using namespace std;


DataExporter::DataExporter(bool includeComputeColumns) : _includeComputeColumns(includeComputeColumns) {
	_defaultFileType = Utils::csv;
    _allowedFileTypes.push_back(Utils::csv);
    _allowedFileTypes.push_back(Utils::txt);
}

void DataExporter::saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback)
{

	boost::nowide::ofstream outfile(path.c_str(), ios::out);

	package->writeDataSetToOStream(outfile, _includeComputeColumns);

	outfile.flush();
	outfile.close();

	progressCallback("Export Data Set", 100);
}


