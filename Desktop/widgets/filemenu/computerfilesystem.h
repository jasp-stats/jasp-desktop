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

#ifndef COMPUTERFILESYSTEM_H
#define COMPUTERFILESYSTEM_H

#include "filesystem.h"
#include "common.h"

class ComputerFileSystem : public FileSystem
{
public:
	explicit ComputerFileSystem(QObject *parent = NULL);

	void refresh() OVERRIDE;

	QString mostRecent() const;

public slots:
	void addRecent(QString path);

private:

	QStringList readRecents();
	void setRecents(const QStringList &recents);
	void setAndSaveRecents(const QStringList &recents);
	void saveRecents();

	QStringList _recents;
};

#endif // COMPUTERFILESYSTEM_H
