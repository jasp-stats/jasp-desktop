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

#ifndef ODSXMLTYPE_H
#define ODSXMLTYPE_H

namespace ods {

// cell data types in sheet.
typedef enum e_odsDataType
{
	odsType_unknown	= 0,
	odsType_float,
	odsType_currency,
	odsType_percent,
	odsType_boolean,
	odsType_string,
	odsType_date,
	odsType_time
} XmlDatatype;

#ifdef JASP_DEBUG
extern const char * const ODSTYPE_STR[];
#endif


// Holds a numeric value.
typedef union s_numbers
{
	s_numbers() : i(-1) {}
	double	dbl;
	int		i;
} NumericValue;

} // namespace

#endif // ODSXMLTYPE_H

