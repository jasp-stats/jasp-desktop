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

#include "fsbmexamples.h"

#include <QFile>
#include <QDir>

#include <QJsonDocument>
#include <QJsonParseError>
#include <QJsonArray>
#include <QJsonObject>

#include <QDebug>

#include "appdirs.h"
#include "utils.h"

FSBMExamples::FSBMExamples(QObject *parent)
	: FSBModel(parent)
{
	_rootPath = _path = "Categories";
	_doc = NULL;

}

FSBMExamples::~FSBMExamples()
{
	if (_doc)
		delete _doc;
}

void FSBMExamples::refresh()
{

	if (_doc == NULL)
		_doc = getJsonDocument();

	emit processingEntries();

	_entries.clear();

	if (_path == "Categories")
		loadCategories();
	else
		loadFilesAndFolders(_path);

}

void FSBMExamples::loadCategories()
{

	if (_doc == NULL)
		return;

	QJsonObject jsonroot = _doc->object();
	QJsonArray children = jsonroot.value("children").toArray();

	_entries.clear();

	//Loop over different children
	foreach (const QJsonValue & value, children)
	{
		QJsonObject file_or_folder = value.toObject();

		QString path = file_or_folder["path"].toString();
		QString name = file_or_folder["name"].toString();
		QString kind = file_or_folder["kind"].toString();
		QString description = file_or_folder["description"].toString();

		if (isFolder(kind))
		{
			path = _path + QDir::separator()  + path;
			_entries.append(createEntry(path, name, description, FSEntry::Folder));
		}
		else
		{
			path = AppDirs::examples() + QDir::separator() + path;
			_entries.append(createEntry(path, name, description,FSEntry::getEntryTypeFromPath(path)));

		}

	}

	emit entriesChanged();

}

void FSBMExamples::loadFilesAndFolders(const QString &docpath)
{

	bool found = false;
	QString relpath = QDir::separator();

	if (_doc == NULL)
		return;

	QJsonObject jsonroot = _doc->object();
	QJsonArray folder = jsonroot.value("children").toArray();

	QStringList list = docpath.split(QChar( QDir::separator() ), QString::SkipEmptyParts);

	foreach (QString itm, list)
	{
		if (itm == _rootPath)
			continue;

		relpath += itm + QDir::separator();

		found = false;

		foreach (const QJsonValue & value, folder)
		{
			QJsonObject file_or_folder = value.toObject();
			QString path = file_or_folder["path"].toString();
			if (path == itm)
			{
				found = true;
				folder = file_or_folder.value("children").toArray();
				break;
			}
		}

	}

	if (found)
	{
		_entries.clear();

		//Loop over different children
		foreach (const QJsonValue & value, folder)
		{
			QJsonObject file_or_folder = value.toObject();

			QString path = file_or_folder["path"].toString();
			QString name = file_or_folder["name"].toString();
			QString kind = file_or_folder["kind"].toString();
			QString description = file_or_folder["description"].toString();

			if (isFolder(kind))
			{
				path = _path + QDir::separator()  + path;
				_entries.append(createEntry(path, name, description, FSEntry::Folder));
			}
			else
			{
				path = AppDirs::examples() +  QDir::separator() + relpath +  QDir::separator() + path;
				_entries.append(createEntry(path, name, description, FSEntry::getEntryTypeFromPath(path)));
			}

		}

		emit entriesChanged();

	}

}

QJsonDocument *FSBMExamples::getJsonDocument()
{

	QString filename = "index";

#ifdef JASP_DEBUG
	filename += "debug";
#endif

	QString fn = AppDirs::examples() + QDir::separator() + filename + ".json";
	QFile index(fn);

	if ( ! index.exists())
	{
		qDebug() << "BackStageForm::loadExamples();  index not found\n";
		return NULL;
	}

	index.open(QFile::ReadOnly);
	if ( ! index.isOpen())
	{
		qDebug() << "BackStageForm::loadExamples();  index could not be opened\n";
		return NULL;
	}

	QByteArray bytes = index.readAll();
	QJsonParseError error;
	QJsonDocument doc = QJsonDocument::fromJson(bytes, &error);

	if (error.error != QJsonParseError::NoError)
	{
		qDebug() << "BackStageForm::loadExamples();  JSON parse error : " << error.errorString() << "\n";
		return NULL;
	}

	QJsonDocument *d = new QJsonDocument;
	*d = doc;
	return  d;

}

bool FSBMExamples::isFolder(const QString &kind)
{
	return (kind.toLower() == "folder" ? true : false);
}
