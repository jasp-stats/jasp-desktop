/*
	Copyright (C) Copyright (C) 2013-2017 University of Amsterdam

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

#include "../importerutils.h"

using namespace std;
using namespace ods;

const string ODSImportDataSet::manifestPath("META-INF/manifest.xml"); //< manfiest file in archive.
// String for content file name: Case insensitve match for *content*.xml
const QString ODSImportDataSet::contentRegExpression(".*content.*\\.xml");


ODSImportDataSet::ODSImportDataSet()
{

}

ODSImportDataSet::~ODSImportDataSet()
{

}


/**
 * @brief createSpace Ensure that we have enough columns for the passed value.
 * @param column The column number to check for.
 * @return The number of columns available. - Maybe greater than column.
 */
size_t ODSImportDataSet::createSpace(int column)
{
#ifndef QT_NO_DEBUG
	size_t numAdded = 0;
#endif
	while(column >= columnCount())
#ifndef QT_NO_DEBUG
	{
		numAdded++;
#endif
		addColumn(new ODSImportColumn(columnCount()));
#ifndef QT_NO_DEBUG
	}

	if (numAdded != 0)
		DEBUG_COUT7("ODSImportDataSet::createSpace(", column, ") - added ", numAdded, " column",
				((numAdded != 1) ? "s" : ""), ".");
#endif
	return columnCount();
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
