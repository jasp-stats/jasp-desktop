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

#include "rdataimportcolumn.h"
#include "timers.h"

RDataImportColumn::RDataImportColumn(ImportDataSet* importDataSet, std::string name) : ImportColumn(importDataSet, name)
{
}

RDataImportColumn::RDataImportColumn(ImportDataSet *importDataSet, std::string name, long reserve) : ImportColumn(importDataSet, name)
{
	_data.reserve(reserve);
}

RDataImportColumn::~RDataImportColumn()
{
	JASPTIMER_SCOPE(RDataImportColumn::~RDataImportColumn());
}

size_t RDataImportColumn::size() const
{
	return _data.size();
}

void RDataImportColumn::addValue(const std::string &value)
{
	_data.push_back(value);
}

const std::vector<std::string> &RDataImportColumn::getValues() const
{
	return _data;
}
