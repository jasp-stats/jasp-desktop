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


	File created by patrick, on 12-01-2017
	Original file name was
*/

#include "odssheetcell.h"

#include "QDate"

#include "../importcolumn.h"

using namespace std;
using namespace ods;

ODSSheetCell::ODSSheetCell()
	: _xmlType(odsType_unknown)
{
}

void ODSSheetCell::setTypeAndValue(XmlDatatype type, const QString &data)
{
	switch(type)
	{
	case odsType_percent:
	case odsType_currency:
	case odsType_float:
	case odsType_unknown:
	case odsType_string:
	case odsType_boolean:
	{
		_xmlType = odsType_string;
		setValue(data);
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
	}
		break;
	} // end of switch(type)
}

void ODSSheetCell::setValue(const QString &value)
{
	_string = value.toStdString();
	// if no type set to date, then it becomes a string.
	if (xmlType() == odsType_unknown)
		_xmlType = odsType_string;
}

void ODSSheetCell::setValue(const string &value)
{
	_string = value;
	// if no type set to date, then it becomes a string.
	if (xmlType() == odsType_unknown)
		_xmlType = odsType_string;
}

const string &ODSSheetCell::valueAsString()
const
{
	return _string;
}
