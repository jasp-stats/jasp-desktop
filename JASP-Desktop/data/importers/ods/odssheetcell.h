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

#ifndef ODSSHEETCELL_H
#define ODSSHEETCELL_H

#include "odstypes.h"

#include <column.h>

#include <QString>

namespace ods
{

class ODSSheetCell
{
	friend class ODSImportColumn;

public:
	ODSSheetCell();
	~ODSSheetCell() {}

	// Getters and setters.
	const XmlDatatype& xmlType() const { return _xmlType; }

	void setTypeAndValue(XmlDatatype type, const QString &data);
	const std::string &valueAsString() const;


private:
	// The data types
	XmlDatatype				_xmlType;
	std::string				_string;

	void setValue(const QString &value);
	void setValue(const std::string &value);
};

} // end namespace ods

#endif // sentinal ODSSHEETCELL_H
