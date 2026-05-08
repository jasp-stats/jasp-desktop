//
// Copyright (C) 2013-2025 University of Amsterdam
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

#include "datafolder.h"

namespace Modules {

DataFolder::DataFolder(QQuickItem * parent) : DataLibraryEntry(parent) {}

void DataFolder::addChild(DataLibraryEntry * child)
{
	if (!_children.contains(child))
	{
		_children.append(child);
		connect(child, &DataLibraryEntry::somethingChanged, this, &DataFolder::somethingChanged, Qt::UniqueConnection);
		emit somethingChanged();
	}
}

void DataFolder::removeChild(DataLibraryEntry * child)
{
	if (_children.removeAll(child) > 0)
	{
		disconnect(child, &DataLibraryEntry::somethingChanged, this, &DataFolder::somethingChanged);
		emit somethingChanged();
	}
}

} // namespace Modules
