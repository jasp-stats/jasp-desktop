//
// Copyright (C) 2017 University of Amsterdam
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

#include "fsbmcomputer.h"

#include <QStandardPaths>
#include <QDir>

#include "fsentry.h"

FSBMComputer::FSBMComputer()
{
	_rootPath = _path = QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).at(0);
}

void FSBMComputer::refresh()
{
	QStringList filterDataFiles;
	filterDataFiles.append("*.jasp");
	filterDataFiles.append("*.csv");

	QDir dir(_path);

	QStringList folders   = dir.entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name | QDir::IgnoreCase);
	QStringList dataFiles = dir.entryList(filterDataFiles, QDir::Files, QDir::Name | QDir::IgnoreCase);

	_entries.clear();

	foreach (const QString &folder, folders)
	{
		QString path = _path + "/" + folder;

		_entries.append(createEntry(path, FSEntry::Folder));
	}

	foreach (const QString &file, dataFiles)
	{
		QString path = _path + "/" + file;

		if (file.endsWith(".jasp", Qt::CaseInsensitive))
			_entries.append(createEntry(path, FSEntry::JASP));
		else
			_entries.append(createEntry(path));
	}

	emit entriesChanged();
}

