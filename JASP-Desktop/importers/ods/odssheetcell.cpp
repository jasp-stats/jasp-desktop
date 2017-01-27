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


	File created by patrick, on 12-01-2017
	Original file name was
*/

#include "odssheetcell.h"

#include "QDate"


using namespace std;
using namespace ods;

const QRegExp	ODSSheetCell::_isEmptyRegExp("\\s+");

const int ODSSheetCell::EmptyInt(INT_MIN);
const double ODSSheetCell::EmptyDouble(NAN);


ODSSheetCell::ODSSheetCell()
	:_xmlType(odsType_unknown)
	, _jaspType(Column::ColumnTypeUnknown)
	, _numericSet(false)
{

}

bool ODSSheetCell::isEmpty() const
{
	switch(jaspType())
	{
	case Column::ColumnTypeNominal:
	case Column::ColumnTypeOrdinal:
	case Column::ColumnTypeScale:
		return (_numericSet == false);
		break;
	case Column::ColumnTypeNominalText:
		return (_string.empty() == false);
		break;
	case Column::ColumnTypeUnknown:
	default:
		return true;
		break;
	}
}



void ODSSheetCell::setTypeAndValue(XmlDatatype type, const QString &data)
{
	switch(type)
	{
	case odsType_percent:
	case odsType_currency:
	case odsType_float:
		setValue(data.toFloat(&_numericSet));
		_xmlType = odsType_float;
		_jaspType = Column::ColumnTypeScale;
		break;

	case odsType_unknown:
	case odsType_string:
	case odsType_boolean:
		// it says it's a string,
		// but can we convert to a number?
	{
		double flt = data.toFloat(&_numericSet);
		if (_numericSet)
		{
			setValue(flt);
			_xmlType = odsType_float;
			_jaspType = Column::ColumnTypeScale;
		}
		else
		{
			_xmlType = odsType_string;
			_jaspType = Column::ColumnTypeNominalText;
			setValue(data);
		}
	}
		break;

		// Dates become strings.
	case odsType_date:
	{
		int years;
		int months;
		int days;
		int hours;
		int mins;
		int secs;
		sscanf(data.toStdString().c_str(), "%d-%d-%dT%d:%d:%d",
			   &years, &months, &days, &hours, &mins, &secs);
		QDate date(years, months, days);
		if ((hours == 0) && (mins == 0) && (secs == 0))
			setValue(date.toString(Qt::DefaultLocaleShortDate));
		else
		{
			QTime time(hours, mins, secs);
			QDateTime dt(date, time);
			setValue(dt.toString(Qt::DefaultLocaleShortDate));
		}
		_xmlType = odsType_string;
		_jaspType = Column::ColumnTypeNominalText;
		_numericSet = false;
	}
		break;

	case odsType_time:
	{
		// Time have variable numbers of digits for HMS.
		int hours; int mins; int secs;
		sscanf(data.toStdString().c_str(), "PT%dH%dM%dS'", &hours, &mins, &secs);
		QTime time(hours % 24, mins, secs);
		setValue(time.toString(Qt::DefaultLocaleShortDate));
		_xmlType = odsType_string;
		_jaspType = Column::ColumnTypeNominalText;
		_numericSet = false;
	}
		break;
	} // end of switch(type)

	// Attempt to convert to an integer type.
	if (xmlType() == odsType_float)
	{
		if (_isIntValue(_numData.dbl))
		{
			setValue(_toInt(_numData.dbl));
			_jaspType = Column::ColumnTypeNominal;
		}
	}
}

void ODSSheetCell::setValue(double value)
{
	_numData.dbl = value;
	_numericSet = true;
}

void ODSSheetCell::setValue(int value)
{
	_numData.i = value;
	_numericSet = true;
}

void ODSSheetCell::setValue(const QString &value)
{
	_string = value.toStdString();
	// if no type set to date, then it becomes a string.
	if (jaspType() == Column::ColumnTypeUnknown)
		_jaspType = Column::ColumnTypeUnknown;
	if (xmlType() == odsType_unknown)
		_xmlType = odsType_string;
}

int ODSSheetCell::valueAsInt()
const
{
	if (isEmpty())
		return EmptyInt;
	else
		return _numData.i;
}

double ODSSheetCell::valueAsDouble()
const
{
	if (isEmpty())
		return EmptyDouble;
	else
		return _numData.dbl;
}

const string ODSSheetCell::valueAsString()
const
{
	return _string;
}

/**
 *
 * @brief forceCellToType Force Cell contents to type.
 * @param requiredType Type to force.
 * @param column int Colum number (for error reporting)
 *
 * Throws an exception on fail.
 */
void ODSSheetCell::forceCellToType(Column::ColumnType requiredType)
{
	// Given that we always have a string available and never convert unknowns,
	// is there any to do?
	if ((requiredType != jaspType())
	  &&(requiredType != Column::ColumnTypeNominalText)
	  &&(jaspType() != Column::ColumnTypeUnknown))
	{
		switch(requiredType)
		{
			case Column::ColumnTypeNominal:
			case Column::ColumnTypeOrdinal:
			if (jaspType() == Column::ColumnTypeScale)
			{
				if (_isIntValue(_numData.dbl))
				{
					setValue(_toInt(_numData.dbl));
					break;
				}
			}
			throw runtime_error("Cannot convert value to nominal or ordinal.");
			break;

		case Column::ColumnTypeScale:
			if ( (jaspType() == Column::ColumnTypeNominal)
			   ||(jaspType() == Column::ColumnTypeOrdinal) )
			{
				setValue(static_cast<double>(_numData.i));
				break;
			}
			throw runtime_error("Cannot convert value to scalar.");
			break;

		default:
			// Should never get here.
			throw runtime_error("Cannot convert value from unknown.");
			break;
		}
		_jaspType = requiredType;
	}
}

/**
 * @brief _isIntValue returns true if value ia an integer value.
 * @param value  Value to test.
 * @return true if value is an integer.
 */
bool ODSSheetCell::_isIntValue(double value)
{
	double intPart = 1.0;
	double fracPart = ::modf(value, &intPart);
	return (fracPart == 0.0);
}

/* @brief _toInt Returns the integer portion of the value.
* @param value Value to convert.
* @return Integer portion of value.
*/
int ODSSheetCell::_toInt(double value)
{
	if (value > 0.0)
		return static_cast<int>(floor(value));
	else
		return static_cast<int>(ceil(value));
}

