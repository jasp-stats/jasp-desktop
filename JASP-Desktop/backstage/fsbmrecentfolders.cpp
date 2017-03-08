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

#include "fsbmrecentfolders.h"

#include <QFileInfo>
#include <QStandardPaths>
#include <QDir>
#include <QDebug>

FSBMRecentFolders::FSBMRecentFolders(QObject *parent)
	: FSBModel(parent)
{

}

void FSBMRecentFolders::refresh()
{
	setRecents(readRecents());
}

QString FSBMRecentFolders::mostRecent() const
{
	if (_recents.length() > 0)
		return _recents.first();
	else
		return QStandardPaths::standardLocations(QStandardPaths::DocumentsLocation).at(0);
}

void FSBMRecentFolders::addRecent(QString path)
{
	QStringList recents = readRecents();

	QString dirPath = QFileInfo(path).absoluteDir().absolutePath();

	recents.removeOne(dirPath);
	recents.prepend(dirPath);

	setAndSaveRecents(recents);
}

QStringList FSBMRecentFolders::readRecents()
{
	_settings.sync();

	QVariant v = _settings.value("recentFolders");
	if (v.type() != QVariant::StringList && v.type() != QVariant::String)
	{
		// oddly, under linux, loading a setting value of type StringList which has
		// only a single string in it, gives you just a string. we QVariant::String is acceptable too

		qDebug() << "FSBrowserModelRecentFolders::refresh();  setting 'recentFolders' is not a QStringList";
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

		_settings.setValue("recentFolders", recents);
	}

	return recents;
}

void FSBMRecentFolders::setRecents(const QStringList &recents)
{
	_recents = recents;

	_entries.clear();

	foreach (const QString &path, recents)
		_entries.append(createEntry(path, FSEntry::Folder));

	emit entriesChanged();
}

void FSBMRecentFolders::setAndSaveRecents(const QStringList &recents)
{
	if (recents == _recents)
		return;

	setRecents(recents);

	_settings.setValue("recentFolders", recents);
	_settings.sync();
}


