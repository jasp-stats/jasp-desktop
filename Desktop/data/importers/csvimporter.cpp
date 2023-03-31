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

#include "csvimporter.h"
#include "csv/csvimportcolumn.h"
#include "csv/csv.h"
#include "timers.h"

using namespace std;


CSVImporter::CSVImporter() : Importer()
{
	DataSetPackage::pkg()->setIsArchive(false);
}

ImportDataSet* CSVImporter::loadFile(const string &locator, boost::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(CSVImporter::loadFile);

	ImportDataSet* result = new ImportDataSet(this);
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
		else
		{
			// Colname should not be integer
			try
			{
				int number = std::stoi(colName);
				
				if(std::to_string(number) == colName) //Check if it is a number or has more afterwards (stoi won't fail if it starts with numbers. So it would convert "1hahaha" into "V1hahaha")
					colName = "V" + colName;
			} catch (...) {
			}
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
			}
		}
		*it = colName;

		importColumns.push_back(new CSVImportColumn(result, colName));
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
			progressCallback(progress);
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

	JASPTIMER_STOP(CSVImporter::loadFile);

	return result;
}
