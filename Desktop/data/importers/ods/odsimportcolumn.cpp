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

#include "odstypes.h"
#include "odsimportdataset.h"

#include <set>
#include "log.h"

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

const stringvec & ODSImportColumn::allValuesAsStrings() const
{
	static stringvec values;
	values.resize(_rows.size());
	
	for(size_t i=0; i<_rows.size(); i++)
		values[i] = _rows[i].valueAsString();

	return values;
}

const stringvec & ODSImportColumn::allLabelsAsStrings() const
{
	static stringvec labels;
	labels.resize(_rows.size());
	
	for(size_t i=0; i<_rows.size(); i++)
		labels[i] = _rows[i].labelAsString();

	return labels;
}

void insert(int row, const std::string& data);

void ODSImportColumn::createSpace(size_t row)
{
	if(_rows.size() <= row)
		_rows.resize(row+1);
}

void ODSImportColumn::setValue(int row, const string &data)
{
	createSpace(row);

	ODSSheetCell & cell = _rows.at(row);
	cell.setValue(data);	
}

void ODSImportColumn::setComment(int row, const string &data)
{
	createSpace(row);

	ODSSheetCell & cell = _rows.at(row);
	cell.setComment(data);	
}

