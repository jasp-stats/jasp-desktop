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

#include "fsbmrecent.h"

#include <QStringList>
#include <QFileInfo>
#include <QEvent>
#include <QDebug>

FSBMRecent::FSBMRecent(QObject *parent)
	: FSBModel(parent)
{
	parent->installEventFilter(this);
}

void FSBMRecent::refresh()
{
	populate(load());
}

bool FSBMRecent::eventFilter(QObject *object, QEvent *event)
{
	if (event->type() == QEvent::Show || event->type() == QEvent::WindowActivate)
		refresh();

	return QObject::eventFilter(object, event);
}


void FSBMRecent::addRecent(const QString &path)
{
	QStringList recents = load();
	recents.removeAll(path);

	recents.prepend(path);
	while (recents.size() > 5)
		recents.removeLast();

	_settings.setValue("recentItems", recents);
	_settings.sync();

	populate(recents);
}

void FSBMRecent::filter(bool (*filterFunction)(QString))
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

	_settings.setValue("recentItems", recents);
	_settings.sync();

	populate(recents);
}

void FSBMRecent::populate(const QStringList &paths)
{
	_entries.clear();

	for (int i = 0; i < 5 && i < paths.length(); i++)
	{
		QString path = paths.at(i);

		FSEntry::EntryType entryType = FSEntry::Other;
		if (path.endsWith(".jasp", Qt::CaseInsensitive))
			entryType = FSEntry::JASP;

		FSEntry entry = createEntry(path, entryType);

		_entries.append(entry);
	}

	emit entriesChanged();
}

bool FSBMRecent::isUrl(const QString &path) const {
	return path.startsWith("http");
}

QStringList FSBMRecent::load()
{
	_settings.sync();

	QVariant v = _settings.value("recentItems");
	if (v.type() != QVariant::StringList && v.type() != QVariant::String)
	{
		// oddly, under linux, loading a setting value of type StringList which has
		// only a single string in it, gives you just a string. we QVariant::String is acceptable too

		qDebug() << "BackStageForm::loadRecents();  setting 'recentItems' is not a QStringList";
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

