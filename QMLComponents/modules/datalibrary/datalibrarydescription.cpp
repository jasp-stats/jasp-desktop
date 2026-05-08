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

#include "datalibrarydescription.h"

namespace Modules {

DataLibraryDescription * DataLibraryDescription::_builtIn = nullptr;

DataLibraryDescription::DataLibraryDescription(QQuickItem * parent) : QQuickItem(parent)
{
	setVisible(false);

	_timer.setSingleShot(true);
	_timer.setInterval(300);
	connect(&_timer, &QTimer::timeout, this, &DataLibraryDescription::delayedUpdate);
}

void DataLibraryDescription::addChild(DataLibraryEntry * entry)
{
	if (!_entries.contains(entry))
	{
		_entries.append(entry);
		connect(entry, &DataLibraryEntry::somethingChanged, this, [this]() { _timer.start(); }, Qt::UniqueConnection);
		_timer.start();
	}
}

void DataLibraryDescription::removeChild(DataLibraryEntry * entry)
{
	if (_entries.removeAll(entry) > 0)
	{
		disconnect(entry, &DataLibraryEntry::somethingChanged, this, nullptr);
		_timer.start();
	}
}

void DataLibraryDescription::delayedUpdate()
{
	emit iShouldBeUpdated(this);
}

} // namespace Modules
