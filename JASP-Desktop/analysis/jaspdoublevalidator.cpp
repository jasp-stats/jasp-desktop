//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "jaspdoublevalidator.h"


QValidator::State JASPDoubleValidator::validate(QString& s, int& pos) const
{
	if (s.isEmpty() || (s.startsWith("-") && s.length() == 1 && bottom() < 0))
	{
		// allow empty field or standalone minus sign
		return QValidator::Intermediate;
	}

	if (s.contains("-") && bottom() >= 0)
		return QValidator::Invalid; 
	
	// check length of decimal places
	QChar point = locale().decimalPoint();
	if (s.indexOf(point) != -1)
	{
		if (decimals() == 0)
			return QValidator::Invalid;
		int lengthDecimals = s.length() - s.indexOf(point) - 1;
		if (lengthDecimals > decimals())
			return QValidator::Invalid;
	}
	// check range of value
	bool isNumber;
	double value = locale().toDouble(s, &isNumber);
	if (!isNumber)
		return QValidator::Invalid;

	if (value >= 0)
	{
		if (value > top())
			return QValidator::Invalid;
		else if (value < bottom())
			return QValidator::Intermediate;
	}
	else
	{
		if (value < bottom())
			return QValidator::Invalid;
		else if (value > top())
			return QValidator::Intermediate;
	}

	return QValidator::Acceptable;
}

