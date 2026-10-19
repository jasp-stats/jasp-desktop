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

#include "filtereddata.h"
#include "dataset.h"

FilteredData::FilteredData(Filter * filter) 
: QSortFilterProxyModel(filter),
  _filter(filter)
{	
	setFilterRole(int(dataPkgRoles::filter));
}

bool FilteredData::filterAcceptsRow(int source_row, const QModelIndex & source_parent)	const
{
	return !_filter || _filter->filtered().size() <= source_row || _filter->filtered()[source_row];
}

