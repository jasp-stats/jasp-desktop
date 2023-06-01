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

#ifndef FYLESYSTEM_H
#define FYLESYSTEM_H

#include <QObject>
#include <QList>
#include <QStringList>

#include "filesystementry.h"
#include "sortable.h"

class FileSystem : public QObject
{
	Q_OBJECT

public:
	explicit FileSystem(QObject *parent = NULL);
	typedef QList<FileSystemEntry> FileSystemEntryList;

	virtual void refresh() = 0;

	const FileSystemEntryList &entries() const;

	const QString &path() const;
	const QString &rootPath() const;

	bool contains(const QString &path) const;

	virtual bool requiresAuthentication() const { return false; }
	virtual bool isAuthenticated() const { return false; }
	virtual void authenticate(const QString &, const QString &) { }
	virtual void clearAuthentication() { }

	bool hasFileEntry(QString name, QString &path);
	bool hasFolderEntry(QString name);

	virtual void sortEntries(Sortable::SortType sortOrder);

public slots:
	void setPath(QString path);

signals:
	void processingEntries();
	void entriesChanged();
	void pathChanged(QString path);

	void authenticationFailed(const QString &message);
	void authenticationSucceeded();
	void authenticationClear();

protected:

	QString _rootPath;
	QString _path;

	static FileSystemEntry createEntry(const QString &path, FileSystemEntry::EntryType type = FileSystemEntry::Other);
	static FileSystemEntry createEntry(const QString &path, const QString &name, const QString &description, FileSystemEntry::EntryType type = FileSystemEntry::Other, const QString &associated_datafile = "");

	FileSystemEntryList _entries;

};

#endif // FYLESYSTEM_H
