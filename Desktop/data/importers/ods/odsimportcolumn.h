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

#ifndef ODSIMPORTCOLUMN_H
#define ODSIMPORTCOLUMN_H

#include "../importcolumn.h"
#include "odsimportdataset.h"
#include "odssheetcell.h"

#include <set>


namespace ods
{
class ODSImportDataSet;

class ODSImportColumn : public ImportColumn
{
public:
	typedef std::map< int, size_t > CellIndex;

	// The empty value for a cell.
	static const ODSSheetCell EmptySheetCell;

	// The constainer used to hold rows within the column.
	typedef std::vector<ODSSheetCell>	Cases;

	ODSImportColumn(ODSImportDataSet* importDataSet, int columnNumber, std::string name);
	virtual ~ODSImportColumn();

	size_t size() const override;

	const stringvec &	allValuesAsStrings()					const	override;
	const stringvec &	allLabelsAsStrings()					const	override;

	inline bool hasCell(size_t row) const
	{
		return (_index.find(row) != _index.end());
	}

	void createSpace(size_t row);

	void setValue(int row, const std::string& data);
	
	void setComment(int row, const std::string& data);

	const ODSSheetCell &getCell(int row) const { return _rows.at(row); }

	columnType	getColumnType() const override { return _columnType; }


private:
	Cases				_rows;

	
	CellIndex			_index;		///< cell indexes indexed by row.
	int					_columnNumber; //<- We know our own column number
	columnType			_columnType; // Our column type.


};

} // end namespace.
#endif // sentinel ODSIMPORTCOLUMN_H
