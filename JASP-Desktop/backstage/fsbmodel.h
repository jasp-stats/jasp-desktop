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

#ifndef FSBMODEL_H
#define FSBMODEL_H

#include <QObject>
#include <QList>
#include <QStringList>

#include "fsentry.h"

class FSBModel : public QObject
{
	Q_OBJECT

	friend class FileSystemTableModel;

public:
	explicit FSBModel(QObject *parent = NULL);
	typedef QList<FSEntry> FileSystemEntryList;

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

public slots:
	void setPath(QString path);

signals:
	void processingEntries();
	void entriesChanged();
	void pathChanged(QString path);

	void authenticationFail(const QString &message);
	void authenticationSuccess();
	void authenticationClear();

protected:

	QString _rootPath;
	QString _path;

	static FSEntry createEntry(const QString &path, FSEntry::EntryType type = FSEntry::Other);
	static FSEntry createEntry(const QString &path, const QString &name, const QString &description, FSEntry::EntryType type = FSEntry::Other);

	FileSystemEntryList _entries;

};

#endif // FSBMODEL_H
