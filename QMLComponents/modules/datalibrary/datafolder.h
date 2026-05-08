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

#ifndef DATAFOLDER_H
#define DATAFOLDER_H

#include "datalibraryentry.h"

namespace Modules {

/// Folder node in DataLibrary{} — groups DataFile and nested DataFolder children.
/// Acts as both a DataLibraryEntry (it registers with its own parent) and a
/// container that its children register with.
class DataFolder : public DataLibraryEntry
{
	Q_OBJECT

public:
	explicit DataFolder(QQuickItem * parent = nullptr);

	void addChild(   DataLibraryEntry * child);
	void removeChild(DataLibraryEntry * child);

	const QList<DataLibraryEntry *> & children() const { return _children; }

private:
	QList<DataLibraryEntry *> _children;
};

} // namespace Modules

#endif // DATAFOLDER_H
