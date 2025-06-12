//
// Copyright (C) 2013-2025 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "rdataimportdataset.h"
#include "rdataimportcolumn.h"
#include "columnutils.h"
#include "timers.h"

RDataImportColumn::RDataImportColumn(RDataImportDataSet* importDataSet, const std::string & name, const stringvec & levels, columnType type)
: ImportColumn(static_cast<ImportDataSet*>(importDataSet), name), _levels(levels), _type(type)
{
}

RDataImportColumn::~RDataImportColumn()
{
	JASPTIMER_SCOPE(RDataImportColumn::~RDataImportColumn());
}

size_t RDataImportColumn::size() const
{
	return _data.size();
}

std::string RDataImportColumn::valueLookup(size_t row) const
{
	return _data[row];
}

std::string RDataImportColumn::labelLookup(size_t row) const
{
	if(_levels.size() == 0)
		return "";

	int level;

	if(ColumnUtils::getIntValue(_data[row], level) && _levels.size() >= level && level > 0)
		return 	_levels[level-1];

	return "";
}

void RDataImportColumn::addValue(const std::string &value, int index)
{
	if(index == -1 || index == _data.size())
		_data.push_back(value);
	else
	{
		if(_data.size() <= index)
			_data.resize(index+1);

		_data[index] = value;	
	}
}
