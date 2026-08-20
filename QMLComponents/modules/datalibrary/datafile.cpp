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

#include "datafile.h"

namespace Modules {

DataFile::DataFile(QQuickItem * parent) : DataLibraryEntry(parent) {}

void DataFile::setPath(const QString & path)
{
	if (_path == path) return;
	_path = path;
	emit pathChanged();
	emit somethingChanged();
}

void DataFile::setDataFile(const QString & dataFile)
{
	if (_dataFile == dataFile) return;
	_dataFile = dataFile;
	emit dataFileChanged();
	emit somethingChanged();
}

} // namespace Modules
