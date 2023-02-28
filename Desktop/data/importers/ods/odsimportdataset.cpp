/*
	Copyright (C) Copyright (C) 2013-2018 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 11-01-2017
	Original file name was
*/

#include "odsimportdataset.h"
#include "log.h"
#include "../importdataset.h"
#include "odsimportcolumn.h"
#include "../odsimporter.h"

using namespace std;
using namespace ods;

const string ODSImportDataSet::manifestPath("META-INF/manifest.xml"); //< manfiest file in archive.
// String for content file name: Case insensitve match for *content*.xml
const QString ODSImportDataSet::contentRegExpression(".*content.*\\.xml");


ODSImportDataSet::ODSImportDataSet(ODSImporter* importer) : ImportDataSet(importer)
{

}

ODSImportDataSet::~ODSImportDataSet()
{

}


ODSImportColumn & ODSImportDataSet::createColumn(string name)
{
	//Log::log() << "ODSImportDataSet::createColumn: " << name << std::endl;
	ODSImportColumn* column = new ODSImportColumn(this, columnCount(), name);
	addColumn(column);
	return *column;
}


/**
 * @brief operator [] Exposes the underlying vector of the ImportDataSet.
 * @param index The bracketed value.
 * @return A reference to the indexed value.
 */
ODSImportColumn & ODSImportDataSet::operator [] (const int index)
{
	return static_cast<ODSImportColumn &>(*(_columns[index]));
}

/**
 * @brief get or Create the underlying vector of the ImportDataSet.
 * @param index The bracketed value.
 * @return A reference to the indexed value.
 */
ODSImportColumn & ODSImportDataSet::getOrCreate (const int index)
{
	if (index < columnCount())
		return static_cast<ODSImportColumn &>(*(_columns[index]));
	else
	{
		stringstream ss;
		ss << "_col" << columnCount() + 1;
		return createColumn(ss.str());
	}
}


/**
 * @brief postLoadProcess Performs post load processing.
 */
void ODSImportDataSet::postLoadProcess()
{
	size_t numRows = 0;
	// Pass the message on to the columns, and find the maximum rows/cases.
	for (ImportColumns::iterator colI = begin(); colI != end(); ++colI)
	{
		ODSImportColumn * col = static_cast<ODSImportColumn *>(*colI);
		col->postLoadProcess();
		numRows = max(numRows, col->size());
	}


	// ensure that we have enough rows.
	for (ImportColumns::iterator colI = begin(); colI != end(); ++colI)
	{
		ODSImportColumn * col = static_cast<ODSImportColumn *>(*colI);
		col->createSpace(numRows - 1);
	}
}
