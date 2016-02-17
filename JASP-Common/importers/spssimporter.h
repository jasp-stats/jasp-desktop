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

#ifndef SPSSIMPORTER_H
#define SPSSIMPORTER_H

#include "datasetpackage.h"
#include <boost/function.hpp>
#include <string>

#include <boost/nowide/fstream.hpp>
#include <iostream>

class SPSSImporter
{
private:
	typedef struct
	{
		std::string name;
		bool isStrings;
		int columnSpan;
		std::vector<double> numeric;
		std::vector<std::string> strings;

	} SPSSColumn;

public:

	static void loadDataSet(DataSetPackage *packageData, const std::string &locator, boost::function<void (const std::string &, int)> progress);
	static void readHeaders(std::istream &stream, std::vector<SPSSColumn> &columns);
	static void readColumnInfoRecord(std::istream &stream, std::vector<SPSSColumn> &columns);
	static void readDataRecord(std::istream &stream);
	static void readLabelRecord(std::istream &stream);
	static void readData(std::istream &stream, std::vector<SPSSColumn> &columns);
	static DataSet *setDataSetSize(DataSet *dataSet, int rowCount, int columnCount);

};

#endif // SPSSIMPORTER_H
