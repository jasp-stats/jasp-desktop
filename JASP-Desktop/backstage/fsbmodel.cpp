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

#include "fsbmodel.h"

FSBModel::FSBModel(QObject *parent) : QObject(parent)
{

}

const FSBModel::FileSystemEntryList &FSBModel::entries() const
{
	return _entries;
}

void FSBModel::setPath(QString path)
{
	_path = path;

	refresh();

	emit pathChanged(path);
}

const QString &FSBModel::path() const
{
	return _path;
}

const QString &FSBModel::rootPath() const
{
	return _rootPath;
}

bool FSBModel::contains(const QString &path) const
{
	foreach (const FSEntry &entry, _entries)
	{
		if (entry.path == path)
			return true;
	}

	return false;
}

bool FSBModel::hasFileEntry(QString name, QString &path)
{
	for (int i =0; i < _entries.length(); i++)
	{
		if (_entries[i].entryType != FSEntry::Folder && _entries[i].name.toLower() == name) {
			path = _entries[i].path;
			return true;
		}
	}
	return false;
}

bool FSBModel::hasFolderEntry(QString name)
{
	for (int i =0; i < _entries.length(); i++)
	{
		if (_entries[i].entryType == FSEntry::Folder && _entries[i].name.toLower() == name)
			return true;
	}
	return false;
}

FSEntry FSBModel::createEntry(const QString &path, FSEntry::EntryType type)
{
	FSEntry entry;
	entry.entryType = type;

	int index = path.lastIndexOf("/");
	if (index == -1) {
		index = path.lastIndexOf("\\");
	}
	if (index != -1)
	{
		entry.name = path.mid(index + 1);
		entry.path = path;
		entry.description = path.mid(0, index);
	}
	else
	{
		entry.name = path;
		entry.path = path;
		entry.description = "";
	}

	return entry;
}

FSEntry FSBModel::createEntry(const QString &path, const QString &name, const QString &description, FSEntry::EntryType type)
{
	FSEntry entry;

	entry.name = name;
	entry.path = path;
	entry.description = description;
	entry.entryType = type;

	return entry;
}

