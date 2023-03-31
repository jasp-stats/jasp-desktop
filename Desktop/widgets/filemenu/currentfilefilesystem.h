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

#ifndef CURRENTFILEFILESYSTEM_H
#define CURRENTFILEFILESYSTEM_H

#include "filesystem.h"
#include "common.h"

class CurrentFileFileSystem : public FileSystem
{
public:
	CurrentFileFileSystem(QObject *parent = NULL);

	void refresh() OVERRIDE;

	void setCurrent(const QString &path);
	QString getCurrent() const;
	bool isOnlineFile() const;

private:
	QString _current;
};

#endif // CURRENTFILEFILESYSTEM_H
