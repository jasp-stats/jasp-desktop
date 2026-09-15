//
// Copyright (C) 2013-2026 University of Amsterdam
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

#include "batchinputsmodel.h"

#include <QDir>
#include <QFileInfo>

#include "parsedarguments.h"
#include "utilities/qutils.h"

BatchInputsModel::BatchInputsModel(QObject * parent) : QAbstractListModel(parent)
{
}

int BatchInputsModel::rowCount(const QModelIndex & parent) const
{
	return parent.isValid() ? 0 : int(_rows.size());
}

QHash<int, QByteArray> BatchInputsModel::roleNames() const
{
	static const QHash<int, QByteArray> roles =
	{
		{ IsFolderRole,			"isFolder"		},
		{ LabelRole,			"label"			},
		{ PathRole,				"path"			},
		{ SelectedRole,			"selected"		},
		{ InFolderRole,			"inFolder"		},
		{ RemovableRole,		"removable"		},
		{ SelectedCountRole,	"selectedCount"	},
		{ DataFileCountRole,	"dataFileCount"	}
	};

	return roles;
}

QVariant BatchInputsModel::data(const QModelIndex & index, int role) const
{
	if(!index.isValid() || index.row() >= int(_rows.size()))
		return QVariant();

	const Row		&	row			= _rows[index.row()];
	const Source	&	source		= _sources[row.source];
	const bool			folderRow	= row.dataFile < 0;
	const QString		path		= folderRow ? source.path : source.dataFiles[row.dataFile];

	switch(role)
	{
	case IsFolderRole:		return folderRow;
	case PathRole:			return path;
	case LabelRole:			return !folderRow && source.folder ? QDir(source.path).relativeFilePath(path) : path;
	case SelectedRole:		return folderRow ? !source.dataFiles.isEmpty() && selectedCountOf(source) == int(source.dataFiles.size()) : !_deselected.contains(path);
	case InFolderRole:		return !folderRow && source.folder;
	case RemovableRole:		return folderRow || !source.folder;
	case SelectedCountRole:	return selectedCountOf(source);
	case DataFileCountRole:	return int(source.dataFiles.size());
	}

	return QVariant();
}

void BatchInputsModel::setSelected(int rowNr, bool selected)
{
	if(rowNr < 0 || rowNr >= int(_rows.size()))
		return;

	const Row		&	row		= _rows[rowNr];
	const Source	&	source	= _sources[row.source];
	bool				changed	= false;

	for(int i = 0; i < int(source.dataFiles.size()); i++)
		if(row.dataFile < 0 || row.dataFile == i)
		{
			const QString & dataFile = source.dataFiles[i];

			if(selected == _deselected.contains(dataFile))
			{
				if(selected)	_deselected.remove(dataFile);
				else			_deselected.insert(dataFile);

				changed = true;
			}
		}

	if(changed)
		selectionOfAllRowsChanged();
}

void BatchInputsModel::remove(int rowNr)
{
	if(rowNr < 0 || rowNr >= int(_rows.size()) || !data(index(rowNr), RemovableRole).toBool())
		return;

	//Its data files might be in a folder added after it, where they are listed now
	_sources.erase(_sources.begin() + _rows[rowNr].source);
	rebuild();
}

QStringList BatchInputsModel::addDataFiles(const QStringList & paths)
{
	QStringList	notDataFiles;
	bool		added		= false,
				reselected	= false;

	for(const QString & path : paths)
	{
		QFileInfo info(path);

		if(path.isEmpty() || !info.isFile())
			continue;

		if(!ParsedArguments::isDataFileType(path))
		{
			notDataFiles.push_back(info.fileName());
			continue;
		}

		const QString dataFile = info.absoluteFilePath();

		if(_listed.contains(dataFile))
		{
			//Already listed, for instance in a folder, choosing it again says you do want it
			if(_deselected.remove(dataFile))
				reselected = true;
		}
		else
		{
			_sources.push_back({ dataFile, false, {} });
			_listed.insert(dataFile);
			added = true;
		}
	}

	if(added)			rebuild();
	else if(reselected)	selectionOfAllRowsChanged();

	return notDataFiles;
}

void BatchInputsModel::addFolder(const QString & path)
{
	QFileInfo info(path);

	if(path.isEmpty() || !info.isDir())
		return;

	const QString folder = info.absoluteFilePath();

	for(const Source & source : _sources)
		if(source.folder && source.path == folder)
			return;

	_sources.push_back({ folder, true, {} });
	rebuild();
}

void BatchInputsModel::refresh()
{
	if(!_sources.empty())
		rebuild();
}

void BatchInputsModel::rebuild()
{
	beginResetModel();

	_listed.clear();
	_rows.clear();

	for(int s = 0; s < int(_sources.size()); s++)
	{
		Source					&	source = _sources[s];
		std::vector<QFileInfo>		found;

		if(source.folder)	ParsedArguments::addDataFilesInFolder(QFileInfo(source.path), found); //Exactly the data files --inputDataDir would use
		else				found.push_back(QFileInfo(source.path));

		source.dataFiles.clear();

		for(const QFileInfo & dataFile : found)
			if(!_listed.contains(dataFile.absoluteFilePath()))
			{
				_listed.insert(dataFile.absoluteFilePath());
				source.dataFiles.push_back(dataFile.absoluteFilePath());
			}

		if(source.folder)
			_rows.push_back({ s, -1 });

		for(int f = 0; f < int(source.dataFiles.size()); f++)
			_rows.push_back({ s, f });
	}

	//A data file that is not listed anymore is forgotten, so should it come back it is selected like any new data file
	_deselected.intersect(_listed);

	endResetModel();

	emit selectionChanged();
}

void BatchInputsModel::selectionOfAllRowsChanged()
{
	if(!_rows.empty())
		emit dataChanged(index(0), index(int(_rows.size()) - 1), { SelectedRole, SelectedCountRole });

	emit selectionChanged();
}

QStringList BatchInputsModel::arguments() const
{
	QStringList	dataFiles,
				inputDataDirs;

	for(const Source & source : _sources)
	{
		if(source.folder && !source.dataFiles.isEmpty() && !anyDeselectedIn(source.path))
			inputDataDirs << tq(ParsedArguments::inputDataDirArg) << source.path;
		else
			for(const QString & dataFile : source.dataFiles)
				if(!_deselected.contains(dataFile))
					dataFiles.push_back(dataFile);
	}

	//The data files have to come right after the jasp-file, an inputDataDir can go anywhere
	return dataFiles + inputDataDirs;
}

bool BatchInputsModel::anyDeselectedIn(const QString & folder) const
{
	const QString inFolder = folder.endsWith('/') ? folder : folder + '/';

	for(const QString & dataFile : _deselected)
		if(dataFile.startsWith(inFolder))
			return true;

	return false;
}

int BatchInputsModel::selectedCount() const
{
	int count = 0;

	for(const Source & source : _sources)
		count += selectedCountOf(source);

	return count;
}

int BatchInputsModel::selectedCountOf(const Source & source) const
{
	int count = 0;

	for(const QString & dataFile : source.dataFiles)
		if(!_deselected.contains(dataFile))
			count++;

	return count;
}

QString BatchInputsModel::missingPath() const
{
	for(const Source & source : _sources)
		if(!QFileInfo::exists(source.path))
			return source.path;

	return QString();
}
