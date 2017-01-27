//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "csvimporter.h"
#include "csvimportcolumn.h"
#include "csv.h"
#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>

using namespace std;
using boost::lexical_cast;

CSVImporter::CSVImporter(DataSetPackage *packageData) : Importer(packageData)
{
	_packageData->isArchive = false;
}

ImportDataSet* CSVImporter::loadFile(const string &locator, boost::function<void(const string &, int)> progressCallback)
{
	ImportDataSet* result = new ImportDataSet();
	vector<string> colNames;
	CSV csv(locator);
	csv.open();

	csv.readLine(colNames);
	vector<CSVImportColumn *> importColumns;
	importColumns.reserve(colNames.size());

	int colNo = 0;
	for (vector<string>::iterator it = colNames.begin(); it != colNames.end(); ++it, ++colNo)
	{
		string colName = *it;
		if (colName == "")
		{
			stringstream ss;
			ss << "V";
			ss << (colNo + 1);
			colName = ss.str();
		}

		if (it != colNames.begin())
		{
			// col name must be unique
			if (std::find(colNames.begin(), it, colName) != it)
			{
				stringstream ss;
				ss << colName;
				ss << "_";
				ss << (colNo + 1);
				colName = ss.str();
				colNames[colNo] = colName;
			}
		}

		importColumns.push_back(new CSVImportColumn(colName));
	}

	unsigned long long progress;
	unsigned long long lastProgress = -1;

	size_t columnCount = colNames.size();

//	for (size_t i = 0; i < columnCount; i++)  // columns
//		cells.push_back(vector<string>());

	vector<string> line;
	bool success = csv.readLine(line);

	while (success)
	{
		progress = 50 * csv.pos() / csv.size();
		if (progress != lastProgress)
		{
			progressCallback("Loading Data Set", progress);
			lastProgress = progress;
		}

		if (line.size() != 0) {
			size_t i = 0;
			for (; i < line.size() && i < columnCount; i++)
				importColumns.at(i)->addValue(line[i]);
			for (; i < columnCount; i++)
				importColumns.at(i)->addValue(string());
		}

		line.clear();
		success = csv.readLine(line);
	}

	for (vector<CSVImportColumn *>::iterator it = importColumns.begin(); it != importColumns.end(); ++it)
		result->addColumn(*it);

	// Build dictionary for sync.
	result->buildDictionary();

	return result;
}


void CSVImporter::fillSharedMemoryColumn(ImportColumn *importColumn, Column &column)
{
	// try to make the column nominal

	bool success = true;
	set<int> uniqueValues;
	std::vector<int> intValues;
	intValues.reserve(importColumn->size());
	CSVImportColumn *csvColumn = dynamic_cast<CSVImportColumn *>(importColumn);

	if (csvColumn->convertToInt(intValues, uniqueValues))
	{
		if (uniqueValues.size() <= 24)
		{
			column.setColumnAsNominalOrOrdinal(intValues, uniqueValues);
			return;
		}
	}

	// try to make the column scale
	success = true;
	vector<double> doubleValues;
	doubleValues.reserve(importColumn->size());

	if (csvColumn->convertToDouble(doubleValues))
	{
		column.setColumnAsScale(doubleValues);
		return;
	}

	// if it can't be made nominal numeric or scale, make it nominal-text
	column.setColumnAsNominalString(csvColumn->getValues());
}

