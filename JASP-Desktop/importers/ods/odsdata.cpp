
#include "odsdata.h"

#include <limits.h>
#include <sstream>
#include <cmath>

#include <QDate>

using namespace std;
using namespace ods;


const string Data::manifestPath("META-INF/manifest.xml"); //< manfiest file in archive.

// String for content file name: Case insensitve match for *content*.xml
const QString Data::contentRegExpression(".*content.*\\.xml");

Data::Data()
{

}

/**
 * @brief Post processes the data.
 * @return The JaspColumn meta data.
 */
vector<Data::JaspColumn>  Data::process()
{
	vector<Data::JaspColumn> result;
	// Process the columns one by one, saving column data.
	for (size_t c = 0L; c < _sheet.size(); ++c)
		result.push_back(_sheet[c].process());

	// No data?
	if ((sheet().numColumns() == 0) || (sheet().numRows() == 0))
		throw runtime_error("No data was found in the spread-sheet.");

	return result;
}


/*******************************************************************
 * class Data::SheetColumn
 *
 ******************************************************************/

const Data::SheetCellLong Data::SheetColumn::EmptyLongCell;


/**
	 * @brief insert Inserts one cell.
	 * @param row Row to insert
	 * @param type ODS data type.
	 * @param data ODS cell value.
	 */
void Data::SheetColumn::insert(int row, XmlDatatype type, const QString &data)
{
	_cells.push_back(SheetCellShort(this, row, type, data));
}

/**
	 * @brief strings finds all unique strings.
	 * @return A vector of strings.
	 */
vector<QString> Data::SheetColumn::strings() const
{
	vector<QString> result;

	for (ShortCellSet::const_iterator i = _cells.begin(); i != _cells.end(); i++)
		result.push_back(i->data());

	return result;
}


/**
 * @brief getLCell Gets a long cell by row.
 * @param row
 * @return Reference to long cell
 */
const Data::SheetCellLong& Data::SheetColumn::getLCell(int row)
const
{
	CellIndex::const_iterator ci = _index.find(row);
	if (ci == _index.end())
		return EmptyLongCell;
	else
		return _processedCells.at(ci->second);
}

Data::SheetCellLong* Data::SheetColumn::getLCell(int row)
{
	CellIndex::iterator ci = _index.find(row);
	if (ci == _index.end())
		return 0;
	else
		return &(_processedCells.at(ci->second));

}


/**
 * @brief value Finds the value for the row.
 * @param row The row to search for (0 - for column lable)
 * @return Value or Appropriate if none.
 */
int Data::SheetColumn::valueAsInt(int row)
const
{
	const SheetCellLong &cell = getLCell(row);
	switch(cell.jaspType())
	{
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
		return cell.valueAsInt();

	case Column::ColumnTypeNominalText:
	{ // Go to the index, and fetch out the index number.
		CellIndex::const_iterator i = _index.find(row);
		if (i == _index.end())
			return SheetCellLong::EmptyInt;
		else
			return static_cast<int>(i->second);
	}
		break;

	case Column::ColumnTypeScale:
	case Column::ColumnTypeUnknown:
		return SheetCellLong::EmptyInt;
		break;
	}
}

double Data::SheetColumn::valueAsDouble(int row)
const
{
	const SheetCellLong &cell = getLCell(row);

	return cell.valueAsDouble();
}


/**
 * @brief labelAt Fetches the string value for index value (NOT the row!)
 * @param index Cell index to fetch for/
 * @return Emptry string on invlide index.
 */
string Data::SheetColumn::labelAt(int index)
const
{
	if ((index < 0) || (index >= numberLabels()))
		return string();
	else
		return _processedCells[index].valueAsString();
}


/**
 * @brief type Finds the type for the row.
 * @param row The row to search for (0 - for column lable)
 * @return Type for the cell.
 */
Column::ColumnType Data::SheetColumn::type(int row)
const
{
	return getLCell(row).jaspType();
}

/**
 * @brief numRows Finds the min or maximum row number.
 * @return The number of index we have.
 */
int Data::SheetColumn::minRow()
const
{
	int result = INT_MAX;
	CellIndex::const_iterator i = _index.begin();
	if (i != _index.end())
		result = i->first;
	return result;
}

int Data::SheetColumn::maxRow()
const
{
	int result = INT_MIN;
	CellIndex::const_reverse_iterator i = _index.rbegin();
	if (i != _index.rend())
		result = i->first;
	return result;
}


/**
 * @brief process Finds the JaspType for the column
 * @return Found JAsp Type and label.
 */
Data::JaspColumn Data::SheetColumn::process()
{
	JaspColumn result;
	result.type(Column::ColumnTypeUnknown);
	{
		// Make a long cell for the first row.
		SheetCellLong lc(_cells[0]);
		if (lc.jaspType() != Column::ColumnTypeNominalText)
		{
			stringstream str;
			str << "No text for column heading found at row 1, column ";
			str << colNumberAsExcel() << '.';
			throw runtime_error(str.str());
		}
		// Save the label.
		result.label(lc.valueAsString());

		// Clear the cell.
		_cells.erase(_cells.begin());
	}

	// move from cells to processed.
	{
		map<SheetCellLong, vector<int> > inter; // rows indexed by data.
		for (size_t i = 0; i < _cells.size(); ++i)
		{
			SheetCellLong lcell(_cells[i]);
			if (inter.find(lcell) == inter.end())
				inter.insert( pair<SheetCellLong, vector<int> >(lcell, vector<int>()) );
			map<SheetCellLong, vector<int> >::iterator li = inter.find(lcell);
			li->second.push_back(_cells[i].rowNumber());

		}
		_cells.clear();

		// For every (unique) long cell, copy to result. and index.
		_index.clear();
		_processedCells.clear();
		for (map<SheetCellLong, vector<int> >::const_iterator i = inter.begin(); i != inter.end(); i++)
		{
			_processedCells.push_back(i->first);
			size_t celRef = _processedCells.size() - 1;
			for (size_t j = 0; j < i->second.size(); j++)
				_index.insert( pair< int, size_t>(i->second[j], celRef) );
		}
	}

	// Now attempt to find the different types of the columns.
	set<Column::ColumnType> jaspTypes;
	for (ProcessedCells::iterator i = _processedCells.begin(); i != _processedCells.end(); ++i)
	{
		if (i->isNotEmpty())
			jaspTypes.insert(i->jaspType());
	}

	result.type(Column::ColumnTypeUnknown);
	switch (jaspTypes.size())
	{
	case 1: // If only one type in column then done.
		result.type(*jaspTypes.begin());
		break;
	default:
		// If anything is a string, then the column becomes a string.
		if (jaspTypes.find(Column::ColumnTypeNominalText) != jaspTypes.end())
			result.type(Column::ColumnTypeNominalText);
		else if (   (jaspTypes.find(Column::ColumnTypeScale) != jaspTypes.end())
				 && (jaspTypes.find(Column::ColumnTypeNominal) != jaspTypes.end())
				 && (jaspTypes.size() == 2) )
			result.type(Column::ColumnTypeScale);
		else
		{
			stringstream str;
			str << "Cannot find a suitable type for column " << colNumberAsExcel() << '.';
			throw runtime_error(str.str());
		}

		// Force contents of cells to the found type.
		_forceToType(result.type());

		break;
	}

	return result;
}


/**
 * @brief colNumberAsExcel Returns the column number as a string (base 26 A-Z).
 * @return
 */
string Data::SheetColumn::colNumberAsExcel()
const
{
	string result;
	int divisor = 26 * 26 * 26 * 26 * 26;	// give up after col ZZZZZ
	int col = _columnNumber;

	bool found = false;
	while(divisor > 0)
	{
		int c = col / divisor;
		if ((c > 0) || (divisor == 1))
			found = true;
		if (found)
			result.push_back( (char) (c + 'A') );
		col = col % divisor;
		divisor = divisor / 26;
	}

	return result;
}


/**
 * @brief _forceToType Force all cells to the type.
 * @param type Type to force.
 */
void Data::SheetColumn::_forceToType(Column::ColumnType type)
{
	for (vector<SheetCellLong>::iterator i = _processedCells.begin();
		i != _processedCells.end(); i++)
		i->forceCellToType(type);
}

/*************************************************************
 * class Data::Sheet
 *
 ************************************************************/

/**
 * @brief createSpace Ensure that we have enough space for the cell at row.
 * @param column
 * @return The number of columns available. - Maybe greater than column.
 */
size_t Data::Sheet::createSpace(int column)
{
	// Got enough cols?
	while (size() <= static_cast<size_t>(column))
		push_back(SheetColumn(size()));
	return size();
}

int Data::Sheet::minRow() const
{
	int result = INT_MAX;
	for (size_t col = 0; col < size(); ++col)
	{
		// Find the shortest column.
		int cols = at(col).minRow();
		result = (result > cols) ? cols : result;
	}

	return result;
}

int Data::Sheet::maxRow() const
{
	int result = INT_MIN;
	for (size_t col = 0; col < size(); ++col)
	{
		// Find the longest column.
		int rows = at(col).maxRow();
		result = (result < rows) ? rows : result;
	}

	return result;
}


/**************************************************************************
 *
 * class Data::SheetCellShort
 *
 **************************************************************************/

Data::SheetCellShort::SheetCellShort(const Data::SheetColumn* col,
	int row, XmlDatatype xmlType,const QString& xmlData)
	: _type(xmlType)
	, _data(xmlData)
	, _numData(NAN)
	, _rowNumber(row)
	, _column(col)
	, _notEmpty(false)
{

	bool cvtOkay = false;
	switch(type())
	{
	// Doubles are doubles so evaluate them.
	case odsType_currency:
	case odsType_float:
	case odsType_percent:
		_numData = data().toDouble(&cvtOkay);
		if (cvtOkay == true)
		{
			_notEmpty = false;
			type(odsType_float);
		}
		break;

	// Can we convert this string to a double?
	case odsType_unknown:
	case odsType_string:
		_numData = data().toDouble(&cvtOkay);
		if (cvtOkay)
		{
			_notEmpty = false;
			type(odsType_float);
		}
		else
		{
			_numData = NAN;
			_notEmpty = (_data.size() > 0);
		}
		break;

	// Booleans become strings.
	case odsType_boolean:
		type(odsType_string);
		_notEmpty = (_data.size() > 0);
		break;

	// Dates  and times get reformatted.
	case odsType_date:
	{
		int years;
		int months;
		int days;
		int hours;
		int mins;
		int secs;
		sscanf(_data.toStdString().c_str(), "%d-%d-%dT%d:%d:%d",
			   &years, &months, &days, &hours, &mins, &secs);
		QDate date(years, months, days);
		if ((hours == 0) && (mins == 0) && (secs == 0))
			_data = date.toString(Qt::ISODate);
		else
		{
			QTime time(hours, mins, secs);
			QDateTime dt(date, time);
			_data = dt.toString(Qt::ISODate);
			_notEmpty = (_data.size() > 0);
		}
		type(odsType_string);
	}
		break;

	case odsType_time:
	{
		// Time have variable numbers of digits for HMS.
		int hours;
		int mins;
		int secs;
		sscanf(_data.toStdString().c_str(), "PT%dH%dM%dS'", &hours, &mins, &secs);
		QTime time(hours % 24, mins, secs);
		_data = time.toString(Qt::DefaultLocaleLongDate);
		_notEmpty = (_data.size() > 0);
		type(odsType_string);
	}
		break;
	} // end switch
}


/**************************************************************************
 *
 * class Data::SheetCellLong
 *
 **************************************************************************/

const int Data::SheetCellLong::EmptyInt = INT_MIN;
const double Data::SheetCellLong::EmptyDouble = NAN;
const string Data::SheetCellLong::EmptyString;



Data::SheetCellLong::SheetCellLong()
	: _xmlType(odsType_unknown)
	, _jaspType(Column::ColumnTypeUnknown)
	,_notEmpty(false)
	,_column(0)
{

}


Data::SheetCellLong::SheetCellLong(const SheetCellShort &cellShort)
 : _xmlType(cellShort.type())
 , _jaspType(Column::ColumnTypeUnknown)
 , _data(cellShort.data())
 ,_notEmpty(cellShort.isNotEmpty())

{
	_numData.dbl = cellShort.numData();

	// Convert type;
	switch(xmlType())
	{
	// Doubles are doubles so evaluate them.
	case odsType_currency:
	case odsType_float:
	case odsType_percent:
		_notEmpty = (!std::isnan(_numData.dbl));
		_jaspType = Column::ColumnTypeScale;
		if (_notEmpty)
			_data = QString::number(_numData.dbl);
		break;

	case odsType_unknown:
		_notEmpty = false;
		_jaspType = Column::ColumnTypeUnknown;
		_data.clear();
		break;

	case odsType_time:
	case odsType_date:
	case odsType_boolean:
	case odsType_string:
		_notEmpty = (_data.size() > 0);
		_jaspType = Column::ColumnTypeNominalText;
		break;
	} // end switch


	// if we have a scalar, can we make a nominal from it?
	if (jaspType() == Column::ColumnTypeScale)
	{
		if (_notEmpty)
		{
			double intBit;
			if (fabs(modf(_numData.dbl, &intBit)) == 0.0)
			{
				if ( (intBit <= INT_MAX) && (intBit >= INT_MIN) )
				{
					_numData.i = static_cast<int>(intBit);
					jaspType(Column::ColumnTypeNominal);
				}
			}
		}
		else
			_numData.dbl = NAN;
	}
}

bool Data::SheetCellLong::operator < (const SheetCellLong &rhs)
const
{
	// Empty are always equivilent.
	if (_notEmpty != rhs._notEmpty)
		return _notEmpty < rhs._notEmpty;
	else if (_notEmpty == false)
		return false;
	else if (jaspType() != rhs.jaspType())
		return jaspType() < rhs.jaspType();
	else if ((jaspType() == Column::ColumnTypeNominalText) && (_data != rhs._data) )
		return _data < rhs._data;
	else if ((jaspType() == Column::ColumnTypeScale) && (_numData.dbl != rhs._numData.dbl))
		return _numData.dbl < rhs._numData.dbl;
	else if ((jaspType() == Column::ColumnTypeNominal) && (_numData.i != rhs._numData.i))
		return _numData.i < rhs._numData.i;
	else if ((jaspType() == Column::ColumnTypeOrdinal) && (_numData.i != rhs._numData.i))
		return _numData.i < rhs._numData.i;
	else
		return _xmlType < rhs._xmlType;
}


int Data::SheetCellLong::valueAsInt()
const
{
	if (_notEmpty == false)
		return EmptyInt;

	switch(jaspType())
	{
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
		return _numData.i;

		// Send empty value!
	case Column::ColumnTypeScale:
	case Column::ColumnTypeUnknown:
	case Column::ColumnTypeNominalText:
		break;

	}

	return EmptyInt;
}

double Data::SheetCellLong::valueAsDouble()
const
{
	if (_notEmpty == false)
		return EmptyDouble;

	switch(jaspType())
	{
	case Column::ColumnTypeScale:
		return _numData.dbl;

		// Send empty value!
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
	case Column::ColumnTypeNominalText:
	case Column::ColumnTypeUnknown:
		break;

	}
	return EmptyDouble;
}

string Data::SheetCellLong::valueAsString()
const
{

	if (_notEmpty == false)
		return EmptyString;

	switch(jaspType())
	{
	case Column::ColumnTypeNominalText:
		return _data.toStdString();

		// Send empty value!
	case Column::ColumnTypeScale:
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
	case Column::ColumnTypeUnknown:
		break;
	}

	return EmptyString;
}

/**
 *
 * @brief forceCellToType Force Cell contents to type.
 * @param requiredType Type to force.
 * @param column int Colum number (for error reporting)
 *
 * Throws an exception on fail.
 */
void Data::SheetCellLong::forceCellToType(Column::ColumnType requiredType)
{
	if (_notEmpty == true)
	{
		switch (jaspType())
		{
		// We are an integer...
		case Column::ColumnTypeNominal:
		case Column::ColumnTypeOrdinal:
			switch(requiredType)
			{
			case Column::ColumnTypeNominal:
			case Column::ColumnTypeOrdinal:
				break;
			case Column::ColumnTypeNominalText:
				_convertToNominalText();
				break;
			case Column::ColumnTypeScale:
				_convertToScalar();
				break;
			case Column::ColumnTypeUnknown:
				goto convert_unknown;
			}
			break;

		// We are a string..
		case Column::ColumnTypeNominalText:
		case Column::ColumnTypeUnknown:
			switch(requiredType)
			{
			case Column::ColumnTypeNominal:
			case Column::ColumnTypeOrdinal:
				_convertToNominal();
				break;
			case Column::ColumnTypeNominalText:
				break;
			case Column::ColumnTypeScale:
				_convertToScalar();
				break;
			case Column::ColumnTypeUnknown:
				goto convert_unknown;
			}

			break;

		// We are a float.
		case Column::ColumnTypeScale:
			switch(requiredType)
			{
			case Column::ColumnTypeNominal:
			case Column::ColumnTypeOrdinal:
				_convertToNominal();
				break;
			case Column::ColumnTypeNominalText:
				_convertToNominalText();
				break;
			case Column::ColumnTypeScale:
				break;
			case Column::ColumnTypeUnknown:
				goto convert_unknown;
			}
			break;

		}
	}

	jaspType(requiredType);
	return;

convert_unknown:
	{
		stringstream str;
		str << "Can't convert to \"unkown\" type at column " << _column->colNumberAsExcel() << '.';
		throw runtime_error(str.str());
	}
}

void Data::SheetCellLong::_convertToScalar()
{
	bool convertOkay = false;
	switch(jaspType())
	{
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
		_numData.dbl = static_cast<int>(_numData.i);
		convertOkay = true;
		break;

	case Column::ColumnTypeUnknown:
	case Column::ColumnTypeNominalText:
		_numData.dbl = _data.toDouble(&convertOkay);
		break;

	case Column::ColumnTypeScale:
		return;
	}

	if (convertOkay == false)
	{
		stringstream str;
		str << "Cannot convert " << _data.toStdString() << " or " << _numData.i;
		str << " to a scalar, at column " << _column->colNumberAsExcel() << '.';
		throw runtime_error(str.str());
	}
}

void Data::SheetCellLong::_convertToNominal()
{
	bool convertOkay = false;
	double intPart;
	switch(jaspType())
	{
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
		return;

	case Column::ColumnTypeUnknown:
	case Column::ColumnTypeNominalText:
		_numData.i = _data.toInt(&convertOkay);
		convertOkay = true;
		break;

	case Column::ColumnTypeScale:
		if (modf(_numData.dbl, &intPart) == 0.0)
		{
			convertOkay = true;
			_numData.i  = static_cast<int>(intPart);
		}
		break;
	}

	if (convertOkay == false)
	{
		stringstream str;
		str << "Cannot convert " << _data.toStdString() << " or " << _numData.dbl;
		str << " to a nominal/ordinal, at column " << _column->colNumberAsExcel() << '.';
		throw runtime_error(str.str());
	}
}

void Data::SheetCellLong::_convertToNominalText()
{
	bool convertOkay = false;
	switch(jaspType())
	{
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
		_data = QString::number(_numData.i);
		convertOkay = true;
		break;

	case Column::ColumnTypeUnknown:
	case Column::ColumnTypeNominalText:
		return;

	case Column::ColumnTypeScale:
		_data = QString::number(_numData.dbl);
		convertOkay = true;
		break;
	}

	if (convertOkay == false)
	{
		stringstream str;
		str << "Cannot convert " << _numData.dbl << " or " << _numData.i;
		str << " to a nominal text value, at column " << _column->colNumberAsExcel() << '.';
	}
}
