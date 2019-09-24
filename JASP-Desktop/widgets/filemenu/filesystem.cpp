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

#include "filesystem.h"

FileSystem::FileSystem(QObject *parent) : QObject(parent)
{

}

const FileSystem::FileSystemEntryList &FileSystem::entries() const
{
	return _entries;
}

void FileSystem::setPath(QString path)
{
	_path = path;

	refresh();

	emit pathChanged(path);
}

const QString &FileSystem::path() const
{
	return _path;
}

const QString &FileSystem::rootPath() const
{
	return _rootPath;
}

bool FileSystem::contains(const QString &path) const
{
	for (const FileSystemEntry &entry : _entries)
	{
		if (entry.path == path)
			return true;
	}

	return false;
}

bool FileSystem::hasFileEntry(QString name, QString &path)
{
	for (int i =0; i < _entries.length(); i++)
	{
		if (_entries[i].entryType != FileSystemEntry::Folder && _entries[i].name.toLower() == name) {
			path = _entries[i].path;
			return true;
		}
	}
	return false;
}

bool FileSystem::hasFolderEntry(QString name)
{
	for (int i =0; i < _entries.length(); i++)
	{
		if (_entries[i].entryType == FileSystemEntry::Folder && _entries[i].name.toLower() == name)
			return true;
	}
	return false;
}

void FileSystem::sortEntries(Sortable::SortType  sortOrder)
{
	switch (sortOrder)
	{
		case Sortable::SortType::None:
			break;

		case Sortable::SortType::SortByNameAZ:
			qSort(_entries.begin(), _entries.end(), FileSystemEntry::compareNames);
			break;

		case Sortable::SortType::SortByNameZA:
			qSort(_entries.begin(), _entries.end(), FileSystemEntry::compareNamesReversed);
			break;

		case Sortable::SortType::SortByDate:
			qSort(_entries.begin(), _entries.end(), FileSystemEntry::compareDateTime);
			break;

		//case Sortable::SortType::SortByDate:
		//	qSort(_entries.begin(), _entries.end(), FileSystemEntry::compareDateTimeReversed);
		//	break;

	}
}

FileSystemEntry FileSystem::createEntry(const QString &path, FileSystemEntry::EntryType type)
{
	FileSystemEntry entry;
	entry.entryType = type;

	int index = path.lastIndexOf("/");
	if (index == -1)
		index = path.lastIndexOf("\\");

	if (index != -1)
	{
		entry.name			= path.mid(index + 1);
		entry.path			= path;
		entry.description	= path.mid(0, index);
	}
	else
	{
		entry.name			= path;
		entry.path			= path;
	}

	entry.description	= "";
	entry.associatedDataFile = "";

	return entry;
}

FileSystemEntry FileSystem::createEntry(const QString &path, const QString &name, const QString &description, FileSystemEntry::EntryType type, const QString &associated_datafile)
{
	FileSystemEntry entry;

	entry.name					= name;
	entry.path					= path;
	entry.description			= description;
	entry.entryType				= type;
	entry.associatedDataFile	= associated_datafile;

	return entry;
}

