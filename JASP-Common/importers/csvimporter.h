//
// Copyright (C) 2013-2016 University of Amsterdam
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

#ifndef CSVIMPORTER_H
#define CSVIMPORTER_H

#include "datasetpackage.h"

#include <boost/function.hpp>

#include <string>
#include <vector>

class CSVImporter
{
public:

	static void loadDataSet(DataSetPackage *packageData, const std::string &locator, boost::function<void (const std::string &, int)> progressCallback);

private:
	static void initColumn(Column &column, const std::string &name, const std::vector<std::string> &cells);
	static std::string deEuropeanise(const std::string &value);
};

#endif // CSVIMPORTER_H
