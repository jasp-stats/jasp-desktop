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
	Original file name was odsimportcolumn.cpp
*/

#include "odsimportcolumn.h"
#include "odssheetcell.h"

#include "../importerutils.h"

#include "odstypes.h"
#include "odsimportdataset.h"

#include <set>

using namespace std;
using namespace ods;


ODSImportColumn::ODSImportColumn(ODSImportDataSet* importDataSet, int columnNumber, string name)
	: ImportColumn(importDataSet, name)
	, _columnNumber(columnNumber)
	, _columnType(columnType::unknown)
{
}

ODSImportColumn::~ODSImportColumn()
{
}


// ImportColumn interface
size_t ODSImportColumn::size()
const
{
	return _rows.size();
}

std::vector<std::string> ODSImportColumn::allValuesAsStrings() const
{
	return getData();
}

/**
 * @brief insert Inserts string value for cell, irrespective of type.
 * @param row
 * @param data
 */
void insert(int row, const std::string& data);


/**
 * @brief _createSpace Ensures that we have enough elements in _rows.
 * @param row Row number to check for.
 */
void ODSImportColumn::createSpace(size_t row)
{
#ifdef JASP_DEBUG
	size_t numAdded = 0;
#endif

	while (_rows.size() <= row)
#ifdef JASP_DEBUG
	{
		numAdded++;
#endif
		_rows.push_back(ODSSheetCell());
#ifdef JASP_DEBUG
	}

	if (numAdded != 0)
		DEBUG_COUT7("ODSImportColumn::_createSpace(", row, ") - added ", numAdded, " row",
					((numAdded != 1) ? "s" : ""), ".");
#endif
}

void ODSImportColumn::setValue(int row, const string &data)
{
	DEBUG_COUT7("Inserting ", data, ", row ", row, ", column ", _columnNumber, ".");

	// Big enough?
	createSpace(row);

	ODSSheetCell & cell = _rows.at(row);
	cell.setValue(data);	
}

/**
 * @brief postLoadProcess Performs posy load processing.
 * @param dataSet Dataset we are a member of.
 *
 * This includes finding the long column name,
 * and the type of the column, and converting all
 * the cells to the same type.
 */
void ODSImportColumn::postLoadProcess()
{
}

vector<string> ODSImportColumn::getData() const
{
	vector<string> values;
	for (Cases::const_iterator i = _rows.begin(); i != _rows.end(); ++i)
	{
		const string &value = i->valueAsString();
		values.push_back(value);
	}
	return values;
}

/**
 * @brief colNumberAsExcel Returns the column number as a string (base 26 digits A-Z).
 * @param column Column number
 * @return
 */
string ODSImportColumn::_colNumberAsExcel(int column)
{
	string result;
	int divisor = 26 * 26 * 26 * 26 * 26 * 26;	// give up after col ZZZZZZ

	// In essence, the the classic number base conversion.
	bool found = false;
	while(divisor > 0)
	{
		int c = column / divisor;
		// supress leading zeros (i.e. 'A')
		if ((c > 0) || (divisor == 1))
			found = true;
		if (found)
			result.push_back((char) (c + 'A'));
		column = column % divisor;
		divisor = divisor / 26;
	}

	return result;
}
