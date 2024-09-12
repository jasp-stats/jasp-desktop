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
}

ImportDataSet* CSVImporter::loadFile(const string &locator, std::function<void(int)> progressCallback)
{
	JASPTIMER_RESUME(CSVImporter::loadFile);
	
	ImportDataSet* result = new ImportDataSet(this);
	stringvec colNames;
	CSV csv(locator);
	csv.open();

	csv.readLine(colNames);
	vector<CSVImportColumn *> importColumns;
	importColumns.reserve(colNames.size());

	int colNo = 0;
	for (stringvec::iterator it = colNames.begin(); it != colNames.end(); ++it, ++colNo)
	{
		string colName = *it;
        
		if (colName == "")
            colName = "V" + std::to_string(colNo+1);
		else
		{
			// Colname should not be just an integer
			try
			{
				if(std::to_string(std::stoi(colName)) == colName) //Check if it is a number or has more afterwards (stoi won't fail if it starts with numbers. To avoid it converting "1hahaha" into "V1hahaha")
					colName = "V" + colName;
			}
            catch (...) {}
		}

		if (it != colNames.begin() && std::find(colNames.begin(), it, colName) != it)
				colName = colName + "_" + std::to_string(colNo + 1);

		*it = colName;

		importColumns.push_back(new CSVImportColumn(result, colName, csv.numRows()));
	}

	unsigned long long progress;
	unsigned long long lastProgress = -1;

	size_t columnCount = colNames.size();

	stringvec line;
	bool success = csv.readLine(line);

	while (success)
	{
		progress = 50 * csv.pos() / csv.size();
		if (progress != lastProgress)
		{
			progressCallback(progress);
			lastProgress = progress;
		}

		if (line.size() != 0) //ignore empty lines
            for(size_t i = 0; i<columnCount; i++)
                importColumns.at(i)->addValue(i < line.size() ? line[i] : ""); //add components and add empty vals for missing columns

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
