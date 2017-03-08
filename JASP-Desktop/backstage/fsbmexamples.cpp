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

#include "fsbmexamples.h"

#include <QFile>
#include <QDir>

#include <QJsonDocument>
#include <QJsonParseError>
#include <QJsonArray>
#include <QJsonObject>

#include <QDebug>

#include "appdirs.h"

FSBMExamples::FSBMExamples(QObject *parent)
	: FSBModel(parent)
{

}

void FSBMExamples::refresh()
{
#ifdef QT_NO_DEBUG
	QFile index(AppDirs::examples() + QDir::separator() + "index.json");
#else
	// when debug is built we want the debug dataset to be available!
	QFile index(AppDirs::examples() + QDir::separator() + "indexdebug.json");
#endif

	if ( ! index.exists())
	{
		qDebug() << "BackStageForm::loadExamples();  index not found\n";
		return;
	}

	index.open(QFile::ReadOnly);
	if ( ! index.isOpen())
	{
		qDebug() << "BackStageForm::loadExamples();  index could not be opened\n";
		return;
	}

	QByteArray bytes = index.readAll();
	QJsonParseError error;

	QJsonDocument doc = QJsonDocument::fromJson(bytes, &error);

	if (error.error != QJsonParseError::NoError)
	{
		qDebug() << "BackStageForm::loadExamples();  JSON parse error : " << error.errorString() << "\n";
		return;
	}

	QJsonArray examples = doc.array();

	for (int i = examples.size() - 1; i >= 0; i--)
	{
		QJsonObject example = examples.at(i).toObject();
		QString path = AppDirs::examples() + QDir::separator() + example["path"].toString();
		QString name = example["name"].toString();
		QString description = example["description"].toString();

		_entries.append(createEntry(path, name, description, FSEntry::JASP));
	}
}

