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


ODSImportColumn::ODSImportColumn(int columnNumber)
	: ImportColumn(_colNumberAsExcel(columnNumber))
	, _columnNumber(columnNumber)
	, _columnType(Column::ColumnTypeUnknown)
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

/**
 * @brief insert Inserts string value for cell, irrespective of type.
 * @param row
 * @param data
 */
void insert(int row, const std::string& data);


bool ODSImportColumn::isValueEqual(Column &col, size_t row)
const
{
	if (row >= size())
		return false;

	bool result = false;
	switch (col.columnType())
	{
		case Column::ColumnTypeScale:
			result = col.isValueEqual(row, _rows[row]._numData.dbl);
		break;

		case Column::ColumnTypeNominal:
		case Column::ColumnTypeOrdinal:
			result = col.isValueEqual(row, _rows[row]._numData.i);
		break;

		case Column::ColumnTypeNominalText:
			result = col.isValueEqual(row, _rows[row]._string);
		break;

		case Column::ColumnTypeUnknown:
			result = false;
		break;
	}
	return result;
}

/**
 * @brief _createSpace Ensures that we have enough elements in _rows.
 * @param row Row number to check for.
 */
void ODSImportColumn::createSpace(size_t row)
{
#ifndef QT_NO_DEBUG
	size_t numAdded = 0;
#endif

	while (_rows.size() <= row)
#ifndef QT_NO_DEBUG
	{
		numAdded++;
#endif
		_rows.push_back(ODSSheetCell());
#ifndef QT_NO_DEBUG
	}

	if (numAdded != 0)
		DEBUG_COUT7("ODSImportColumn::_createSpace(", row, ") - added ", numAdded, " row",
					((numAdded != 1) ? "s" : ""), ".");
#endif
}

/**
 * @brief setValue Inserts one cell value.
 * @param row Row to insert
 * @param type ODS data type.
 * @param data ODS cell value.
 */
void ODSImportColumn::setValue(int row, XmlDatatype type, const QString &data)
{
	DEBUG_COUT9("Inserting ", data.toStdString(), " as ", ODSTYPE_STR[type], ", row ", row, ", column ", _columnNumber, ".");

	// Big enough?
	createSpace(row);

	// insert the data
	_rows.at(row).setTypeAndValue(type, data);

}

/**
 * @brief insert Inserts string value for cell, irrespective of type.
 * @param row
 * @param data
 */
void ODSImportColumn::setValue(int row, const QString& data)
{
	DEBUG_COUT7("Inserting ", data.toStdString(), ", row ", row, ", column ", _columnNumber, ".");

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
	// Sanity check: Do we have a heading?
	if ((_rows.size() == 0) || (_rows[0].xmlType() != odsType_string))
	{
		string msg("No column header found in sheet row 1, column ");
		msg.append(_colNumberAsExcel(_columnNumber));
		msg.append(".");
		throw runtime_error(msg);
	}

	// Get the long name, rename this column, and erase the first cell.
//	setLongName(_rows[0]._string);
	setName(_rows[0]._string);
	_rows.erase(_rows.begin());

	// Collect data over all the rows.
	set<Column::ColumnType> jaspTypes;
	for (Cases::const_iterator i = _rows.begin(); i != _rows.end(); ++i)
		jaspTypes.insert(i->jaspType());

	switch(jaspTypes.size())
	{
	case 1:
		// This is easy - we are done!
		if ((*jaspTypes.begin()) != Column::ColumnTypeUnknown)
		{
			_columnType = (*jaspTypes.begin());
			break;
		}
		// Else fall through.
	case 0:
	{
		string msg("No valid data found in column ");
		msg.append(_name);
		msg.append(".");
		throw runtime_error(msg);
	}
		break;

	default:
		/*
		 * These rules determine the column type, where the cells are of mixed types.
		 * We allow cells of type unkown to fall through the net, since they are
		 * by definition empty, and thus polymorphic.
		 */
		// Any cell a string?
		if (jaspTypes.find(Column::ColumnTypeNominalText) != jaspTypes.end())
			// Then it's all strings.
			_columnType = Column::ColumnTypeNominalText;

		// Any cell a scalar? (but no strings - we've dealt with them!)
		else if (jaspTypes.find(Column::ColumnTypeScale) != jaspTypes.end())
			// Then it's all scalars.
			_columnType = Column::ColumnTypeScale;

		// Any cell an int? (but no scalars or strings - we;ve already dealt with them!)
		else if (jaspTypes.find(Column::ColumnTypeNominal) != jaspTypes.end())
			// Then it's all Nomioals.
			_columnType = Column::ColumnTypeNominal;

		else
		{
			// How did we get here? Something odd is going on.
			stringstream str;
			str << "Cannot find a suitable type for column " << _colNumberAsExcel(_columnNumber) << '.';
			throw runtime_error(str.str());
		}
		break;
	}

	// Set all the cells to passed type.
	_forceToType(_columnType);
}

// As per Importers.
void ODSImportColumn::fillSharedMemoryColumn(Column &column)
const
{
	bool isOrdinal = false;
	switch(getJASPColumnType())
	{
	case Column::ColumnTypeOrdinal:
		column.setColumnType(Column::ColumnTypeOrdinal);
		isOrdinal = true;
	case Column::ColumnTypeNominal:
		column.setColumnType(Column::ColumnTypeNominal);
	{
		vector<int> values;
		set <int> uValues;
		for (Cases::const_iterator i = _rows.begin(); i != _rows.end(); ++i)
		{
			int val = i->valueAsInt();
			values.push_back(val);
			if (val != INT_MIN)
				uValues.insert(val);
		}
		column.setColumnAsNominalOrOrdinal(values, uValues, isOrdinal);
	}
		break;

	case Column::ColumnTypeNominalText:
		column.setColumnType(Column::ColumnTypeNominalText);
	{
		vector<string> values;
		for (Cases::const_iterator i = _rows.begin(); i != _rows.end(); ++i)
			values.push_back(i->valueAsString());
		column.setColumnAsNominalString(values);
	}
		break;

	case Column::ColumnTypeScale:
		   column.setColumnType(Column::ColumnTypeScale);
	{
		vector<double> values;
		for (Cases::const_iterator i = _rows.begin(); i != _rows.end(); ++i)
			values.push_back(i->valueAsDouble());
		column.setColumnAsScale(values);
	}
		break;
	case Column::ColumnTypeUnknown:
		break;
	}
}

/**
 * @brief _forceToType Force all cells to the type.
 * @param type Type to force.
 */
void ODSImportColumn::_forceToType(Column::ColumnType type)
{
	for (Cases::iterator i = _rows.begin(); i != _rows.end(); ++i)
		i->forceCellToType(type);
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
