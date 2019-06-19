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

#ifndef SORTABLE_H
#define SORTABLE_H

class SortMenuModel;

class Sortable
{

public:
	enum SortType {
		None = 0,
		SortByName,
		SortByNameAZ,
		SortByNameZA,
		SortByType,
		SortByDate,
		SortBySize
	};

	virtual ~Sortable() {}

	virtual void sortWithType(SortType sortType, bool ascending = true) = 0;
	void setSortModel(SortMenuModel* menu) { _sortMenuModel = menu; }

	void sortItems();
	SortType currentSortType();

private:
	SortMenuModel* _sortMenuModel = nullptr;
};

#endif // SORTABLE_H
