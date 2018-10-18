//
// Copyright (C) 2018 University of Amsterdam
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

#include "fsbmcurrentfile.h"

#include <QStringList>
#include <QFileInfo>
#include <QEvent>
#include <QDebug>

FSBMCurrentFile::FSBMCurrentFile(QObject *parent)
	: FSBModel(parent)
{
	parent->installEventFilter(this);
	_current = QString();
}

void FSBMCurrentFile::refresh()
{
}

void FSBMCurrentFile::setCurrent(const QString &path)
{
	if (path.endsWith(".jasp", Qt::CaseInsensitive))
		return;

	_current = path;

	_entries.clear();
	FSEntry::EntryType entryType = FSEntry::Other;
	FSEntry entry = createEntry(path, entryType);
	_entries.append(entry);

	emit entriesChanged();
}

QString FSBMCurrentFile::getCurrent() const {
	return _current;
}

bool FSBMCurrentFile::isOnlineFile() const {
	return _current.startsWith("http");
}

