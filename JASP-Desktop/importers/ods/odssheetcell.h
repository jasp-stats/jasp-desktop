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

#ifndef ODSSHEETCELL_H
#define ODSSHEETCELL_H

#include "odstypes.h"

#include <column.h>

#include <QString>
#include <QRegExp>

namespace ods
{

class ODSSheetCell
{
	friend class ODSImportColumn;

public:

	const static int EmptyInt;
	const static double EmptyDouble;

	ODSSheetCell();
	~ODSSheetCell() {}

	// Getters and setters.
	const XmlDatatype& xmlType() const { return _xmlType; }

	const Column::ColumnType& jaspType() const { return _jaspType; }
	bool isEmpty() const;

	void setTypeAndValue(XmlDatatype type, const QString &data);
	void setValue(double value);
	void setValue(int value);
	void setValue(const QString &value);
	int valueAsInt() const;
	double valueAsDouble() const;
	const std::string valueAsString() const;

	/**
	 *
	 * @brief forceCellToType Force Cell contents to type.
	 * @param requiredType Type to force.
	 * @param column int Colum number (for error reporting)
	 *
	 * Throws an exception on fail.
	 */
	void forceCellToType(Column::ColumnType requiredType);


private:
	// The data types
	XmlDatatype				_xmlType;
	Column::ColumnType		_jaspType;
	std::string				_string;
	NumericValue			_numData;
	bool					_numericSet;

	static const QRegExp	_isEmptyRegExp;


	/**
	 * @brief _isIntValue returns true if value ia an integer value.
	 * @param value  Value to test.
	 * @return true if value is an integer.
	 */
	static bool _isIntValue(double value);

	/**
	 * @brief _toInt REturns the integer portion of the value.
	 * @param value Value to convert.
	 * @return Integer portion of value.
	 */
	static int _toInt(double value);
};

} // end namespace ods

#endif // sentinal ODSSHEETCELL_H
