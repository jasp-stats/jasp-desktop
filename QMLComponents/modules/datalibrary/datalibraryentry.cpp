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

#include "datalibraryentry.h"
#include "datafolder.h"
#include "datalibrarydescription.h"

namespace Modules {

DataLibraryEntry::DataLibraryEntry(QQuickItem * parent) : QQuickItem(parent)
{
	connect(this, &QQuickItem::parentChanged, this, &DataLibraryEntry::registerParent);
}

void DataLibraryEntry::setName(const QString & name)
{
	if (_name == name) return;
	_name = name;
	emit nameChanged();
	emit somethingChanged();
}

void DataLibraryEntry::setDescription(const QString & desc)
{
	if (_description == desc) return;
	_description = desc;
	emit descriptionChanged();
	emit somethingChanged();
}

void DataLibraryEntry::registerParent(QQuickItem * newParent)
{
	if (_parentDescription) _parentDescription->removeChild(this);
	if (_parentFolder)      _parentFolder->removeChild(this);

	_parentDescription = nullptr;
	_parentFolder      = nullptr;

	if ((_parentDescription = qobject_cast<DataLibraryDescription *>(newParent)))
		_parentDescription->addChild(this);
	else if ((_parentFolder = qobject_cast<DataFolder *>(newParent)))
		_parentFolder->addChild(this);
}

} // namespace Modules
