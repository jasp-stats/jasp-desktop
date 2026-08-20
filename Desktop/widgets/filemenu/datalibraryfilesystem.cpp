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

#include "datalibraryfilesystem.h"

#include <QDir>
#include <QFile>
#include <QUrl>
#include <QFileInfo>
#include <QQmlContext>

#include "utilities/appdirs.h"
#include "utilities/qutils.h"
#include "utilities/qmlutils.h"
#include "mainwindow.h"
#include "modules/dynamicmodules.h"
#include "log.h"

const QString DataLibraryFileSystem::rootelementname = "Categories";

DataLibraryFileSystem::DataLibraryFileSystem(QObject * parent, QString root) : FileSystem(parent)
{
	_rootPath = _path = root;
}

void DataLibraryFileSystem::refresh()
{
	_folderIndex.clear();
	emit processingEntries();
	_entries.clear();

	if (_path == DataLibraryFileSystem::rootelementname)
		loadRootElements();
	else
		loadFilesAndFolders(_path);
}

void DataLibraryFileSystem::tryLoadBuiltIn()
{
	if (Modules::DataLibraryDescription::builtIn())
		return;

	if (!MainWindow::singleton())
		return;

	QQmlContext * context = MainWindow::singleton()->giveRootQmlContext();
	if (!context)
		return;

	const QString qmlPath = AppDirs::examples() + "/DataLibrary.qml";
	if (!QFileInfo(qmlPath).exists())
	{
		Log::log() << "DataLibraryFileSystem: " << qmlPath << " not found." << std::endl;
		return;
	}

	auto * desc = qobject_cast<Modules::DataLibraryDescription *>(
		instantiateQml(QUrl::fromLocalFile(qmlPath), "BuiltIn", context));

	if (!desc)
	{
		Log::log() << "DataLibraryFileSystem: DataLibrary.qml root must be a DataLibrary item." << std::endl;
		return;
	}

	Modules::DataLibraryDescription::setBuiltIn(desc);

	connect(desc, &Modules::DataLibraryDescription::iShouldBeUpdated,
	        this, [this](Modules::DataLibraryDescription *) { refresh(); });
}

void DataLibraryFileSystem::loadRootElements()
{
	tryLoadBuiltIn();

	// Built-in library: each top-level child becomes a root entry
	if (auto * builtIn = Modules::DataLibraryDescription::builtIn())
	{
		const QString builtInRoot = AppDirs::examples() + "/Data Library/";
		addEntriesFromContainer(builtIn->entries(), _path, builtInRoot);
	}

	// Module libraries: each module with a DataLibrary.qml gets a top-level folder
	if (DynamicModules::dynMods())
		for (auto & [name, mod] : DynamicModules::dynMods()->modules())
			if (mod && mod->dataLibraryDescription())
			{
				const Modules::DataLibraryDescription * desc = mod->dataLibraryDescription();
				const QString modTitle   = desc->moduleTitle().isEmpty() ? mod->titleQ() : desc->moduleTitle();
				const QString modPath    = _path + QDir::separator() + modTitle;
				const QString modRoot    = tq(mod->examplesFolder());

				_folderIndex[modPath] = { const_cast<Modules::DataLibraryDescription *>(desc), modRoot };
				_entries.append(createEntry(modPath, modTitle, "", FileSystemEntry::Folder, ""));
			}

	emit entriesChanged();
}

void DataLibraryFileSystem::loadFilesAndFolders(const QString & path)
{
	auto it = _folderIndex.find(path);
	if (it != _folderIndex.end())
	{
		const FolderInfo & info = it.value();
		_entries.clear();

		QList<Modules::DataLibraryEntry *> children;
		if (auto * desc = qobject_cast<Modules::DataLibraryDescription *>(info.container.data()))
			children = desc->entries();
		else if (auto * folder = qobject_cast<Modules::DataFolder *>(info.container.data()))
			children = folder->children();

		addEntriesFromContainer(children, path, info.rootPath);
	}

	emit entriesChanged();
}

void DataLibraryFileSystem::addEntriesFromContainer(
        const QList<Modules::DataLibraryEntry *> & children,
        const QString & currentPath,
        const QString & rootPath)
{
	for (Modules::DataLibraryEntry * entry : children)
	{
		if (!entry) continue;

		const QString entryPath = currentPath + QDir::separator() + entry->name();

		if (auto * folder = qobject_cast<Modules::DataFolder *>(entry))
		{
			_folderIndex[entryPath] = { folder, rootPath };
			_entries.append(createEntry(entryPath, entry->name(), entry->description(),
			                            FileSystemEntry::Folder, ""));
		}
		else if (auto * file = qobject_cast<Modules::DataFile *>(entry))
		{
			const QString filePath     = rootPath + file->path();
			const QString dataFilePath = file->dataFile().isEmpty() ? "" : rootPath + file->dataFile();
			_entries.append(createEntry(filePath, file->name(), file->description(),
			                            FileSystemEntry::getEntryTypeFromPath(filePath), dataFilePath));
		}
	}
}
