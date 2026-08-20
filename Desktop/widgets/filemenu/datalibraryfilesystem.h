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

#ifndef FSBMDATALIBRARY_H
#define FSBMDATALIBRARY_H

#include <QMap>
#include <QPointer>
#include "filesystem.h"
#include "common.h"
#include "modules/datalibrary/datalibrarydescription.h"
#include "modules/datalibrary/datafolder.h"
#include "modules/datalibrary/datafile.h"

class DataLibraryFileSystem : public FileSystem
{
	Q_OBJECT

public:
	DataLibraryFileSystem(QObject * parent = nullptr, QString root = "");
	void refresh() OVERRIDE;

	static const QString rootelementname;

private:
	struct FolderInfo
	{
		QPointer<QObject> container;  ///< DataLibraryDescription* or DataFolder*
		QString           rootPath;   ///< Absolute base path for files in this subtree
	};

	void tryLoadBuiltIn();
	void loadRootElements();
	void loadFilesAndFolders(const QString & path);

	void addEntriesFromContainer(const QList<Modules::DataLibraryEntry *> & children,
	                             const QString & currentPath,
	                             const QString & rootPath);

	QMap<QString, FolderInfo>  _folderIndex;
};

#endif // FSBMDATALIBRARY_H
