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

#include "recentfilesfilesystem.h"
#include <QStringList>
#include <QFileInfo>
#include <QEvent>
#include "log.h"
#include "utilities/settings.h"

RecentFilesFileSystem::RecentFilesFileSystem(QObject *parent)
	: FileSystem(parent)
{
	parent->installEventFilter(this);
}

void RecentFilesFileSystem::refresh()
{
	populate(load());
}

bool RecentFilesFileSystem::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::Show || event->type() == QEvent::WindowActivate)
		refresh();

	return QObject::eventFilter(object, event);
}


void RecentFilesFileSystem::addRecent(const QString &path)
{
	QStringList recents = load();
	recents.removeAll(path);

	recents.prepend(path);
	while (recents.size() > 5)
		recents.removeLast();

	Settings::setValue(Settings::RECENT_ITEMS, recents);
	Settings::sync();

	populate(recents);
}

void RecentFilesFileSystem::filter(bool (*filterFunction)(QString))
{
	QStringList recents = load();

	for (int i = 0; i < recents.length(); i++)
	{
		if (filterFunction(recents.at(i)) == false)
		{
			recents.removeAt(i);
			i -= 1;
		}
	}

	Settings::setValue(Settings::RECENT_ITEMS, recents);
	Settings::sync();

	populate(recents);
}

void RecentFilesFileSystem::populate(const QStringList &paths)
{
	_entries.clear();

	for (int i = 0; i < 5 && i < paths.length(); i++)
	{
		QString path = paths.at(i);

		FileSystemEntry::EntryType entryType = FileSystemEntry::Other;
		if (path.endsWith(".jasp", Qt::CaseInsensitive))
			entryType = FileSystemEntry::JASP;

		FileSystemEntry entry = createEntry(path, entryType);

		_entries.append(entry);
	}

	emit entriesChanged();
}

bool RecentFilesFileSystem::isUrl(const QString &path) const {
	return path.startsWith("http");
}

QStringList RecentFilesFileSystem::load()
{
	Settings::sync();

	QVariant v = Settings::value(Settings::RECENT_ITEMS);
	if (v.type() != QVariant::StringList && v.type() != QVariant::String)
	{
		// oddly, under linux, loading a setting value of type StringList which has
		// only a single string in it, gives you just a string. we QVariant::String is acceptable too

		Log::log()  << "BackStageForm::loadRecents();  setting 'recentItems' is not a QStringList" << std::endl;
		return QStringList();
	}

	QStringList recents = v.toStringList();

	for (int i = 0; i < recents.size(); i++)
	{
		QString path = recents[i];

		if (isUrl(path))
			continue;

		if ( ! QFileInfo::exists(recents[i]))
		{
			recents.removeAt(i);
			i -= 1;
		}
	}

	return recents;
}

