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

#include "computerfilesystem.h"

#include <QFileInfo>
#include <QStandardPaths>
#include <QDir>

#include "utilities/settings.h"
#include "log.h"

ComputerFileSystem::ComputerFileSystem(QObject *parent)
	: FileSystem(parent)
{

}

void ComputerFileSystem::refresh()
{
	setRecents(readRecents());
}

QString ComputerFileSystem::mostRecent() const
{
	if (_recents.length() > 0)
		return _recents.first();
	else
		return QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).at(0);
}

void ComputerFileSystem::addRecent(QString path)
{
	QStringList recents = readRecents();

	QString dirPath = QFileInfo(path).absoluteDir().absolutePath();

	recents.removeOne(dirPath);
	recents.prepend(dirPath);

	setAndSaveRecents(recents);
}

QStringList ComputerFileSystem::readRecents()
{
	Settings::sync();

	QVariant v = Settings::value(Settings::RECENT_FOLDERS);
	if (v.type() != QVariant::StringList && v.type() != QVariant::String)
	{
		// oddly, under linux, loading a setting value of type StringList which has
		// only a single string in it, gives you just a string. we QVariant::String is acceptable too

		Log::log()  << "FSBrowserModelRecentFolders::refresh();  setting 'recentFolders' is not a QStringList" << std::endl;
	}

	QStringList recents = v.toStringList();

	for (int i = 0; i < recents.size(); i++)
	{
		if ( ! QFileInfo::exists(recents[i]))
		{
			recents.removeAt(i);
			i -= 1;
		}
	}

	while (recents.size() > 5)
		recents.removeLast();

	if (recents.length() == 0)
	{
		recents.append(QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation));
		recents.append(QStandardPaths::standardLocations(QStandardPaths::DesktopLocation));

		Settings::setValue(Settings::RECENT_FOLDERS, recents);
	}

	return recents;
}

void ComputerFileSystem::setRecents(const QStringList &recents)
{
	_recents = recents;

	_entries.clear();

	for (const QString &path : recents)
		_entries.append(createEntry(path, FileSystemEntry::Folder));

	emit entriesChanged();
}

void ComputerFileSystem::setAndSaveRecents(const QStringList &recents)
{
	if (recents == _recents)
		return;

	setRecents(recents);

	Settings::setValue(Settings::RECENT_FOLDERS, recents);
	Settings::sync();
}


