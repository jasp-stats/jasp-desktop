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

#include "csvimporter.h"
#include "csvimportcolumn.h"
#include "csv.h"
#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>

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
				importColumns.at(i)->data.push_back(line[i]);
			for (; i < columnCount; i++)
				importColumns.at(i)->data.push_back(string());
		}

		line.clear();
		success = csv.readLine(line);
	}

	for (vector<CSVImportColumn *>::iterator it = importColumns.begin(); it != importColumns.end(); ++it)
		result->addColumn(*it);

	return result;
}

void CSVImporter::initSharedMemoryColumn(ImportColumn *importColumn, Column &column)
{
	// we treat single spaces as missing values, because SPSS saves missing values as a single space in CSV files

	column.setName(importColumn->getName());
	// try to make the column nominal

	bool success = true;
	set<int> uniqueValues;
	std::vector<int> intValues;
	intValues.reserve(importColumn->size());
	vector<string> &cells = (dynamic_cast<CSVImportColumn *>(importColumn))->data;

	BOOST_FOREACH(const string &value, cells)
	{
		if (value != "NaN" && value != "nan" && value != "" && value != " ")
		{
			try
			{
				int v = lexical_cast<int>(value);
				uniqueValues.insert(v);
				intValues.push_back(v);
			}
			catch (...)
			{
				// column can't be made nominal numeric

				success = false;
				break;
			}
		}
		else
		{
			intValues.push_back(INT_MIN);
		}
	}

	if (success && uniqueValues.size() <= 24)
	{
		column.setColumnAsNominalOrOrdinal(intValues, uniqueValues);
		return;
	}

	// try to make the column scale
	success = true;
	vector<double> doubleValues;
	doubleValues.reserve(importColumn->size());

	BOOST_FOREACH(const string &value, cells)
	{
		string v = deEuropeanise(value);
		double doubleValue;

		if (v != "" && v != " ")
		{
			try
			{
				doubleValue = lexical_cast<double>(v);
			}
			catch (...)
			{
				// column can't be made scale
				success = false;
				break;
			}
		}
		else
		{
			doubleValue = NAN;
		}

		doubleValues.push_back(doubleValue);
	}

	if (success)
	{
		column.setColumnAsScale(doubleValues);
		return;
	}

	// if it can't be made nominal numeric or scale, make it nominal-text
	column.setColumnAsNominalString(cells);
}

string CSVImporter::deEuropeanise(const string &value)
{
	int dots = 0;
	int commas = 0;

	for (size_t i = 0; i < value.length(); i++)
	{
		if (value[i] == '.')
			dots++;
		else if (value[i] == ',')
			commas++;
	}

	if (commas > 0)
	{
		string uneurope = value;

		if (dots > 0)
		{
			size_t i = 0;
			size_t j = 0;

			for (;i < value.size(); i++)
			{
				if (value[i] == '.')
					continue;
				uneurope[j] = value[i];

				j++;
			}

			uneurope.resize(j);
		}

		for (size_t i = 0; i < uneurope.length(); i++)
		{
			if (uneurope[i] == ',')
			{
				uneurope[i] = '.';
				break;
			}
		}

		return uneurope;
	}

	return value;
}
