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

#include "fsbmdatalibrary.h"

#include <QFile>
#include <QDir>

#include <QJsonDocument>
#include <QJsonParseError>
#include <QJsonArray>
#include <QJsonObject>

#include <QDebug>

#include "appdirs.h"
#include "utils.h"

const QString FSBMDataLibrary::rootelementname = "Categories";

FSBMDataLibrary::FSBMDataLibrary(QObject *parent, QString root)
	: FSBModel(parent)
{
	_rootPath = _path = root;
	_dataLibraryRootPath = "";
	_doc = NULL;

}

FSBMDataLibrary::~FSBMDataLibrary()
{
	if (_doc)
		delete _doc;
}

void FSBMDataLibrary::refresh()
{

	if (_doc == NULL)
		_doc = getJsonDocument();
	
	emit processingEntries();

	_entries.clear();

	if (_path == FSBMDataLibrary::rootelementname)
		loadRootElements();
	else
		loadFilesAndFolders(_path);

}

void FSBMDataLibrary::loadRootElements()
{

	if (_doc == NULL)
		return;

	QJsonObject jsonroot = _doc->object();
	QJsonArray children = jsonroot.value("children").toArray();
	
	if (_dataLibraryRootPath == "")
		_dataLibraryRootPath = AppDirs::examples() +  QDir::separator()  + jsonroot["path"].toString() + QDir::separator(); 

	_entries.clear();

	//Loop over different children
	foreach (const QJsonValue & value, children)
	{
		QJsonObject file_or_folder = value.toObject();

		QString path = file_or_folder["path"].toString();
		QString name = file_or_folder["name"].toString();
		QString description = file_or_folder["description"].toString();
		QString type = file_or_folder["kind"].toString();
		QString debug =  file_or_folder["debug"].toString();
#ifndef JASP_DEBUG
		if (debug.toLower() == "true") continue;		
#endif	
		QString associated_datafile = file_or_folder["associated_datafile"].toString();
		if (isFolder(type))
		{
			path = _path + QDir::separator()  + path;
			_entries.append(createEntry(path, name, description, FSEntry::Folder, associated_datafile));
		}
		else
		{
			path = _dataLibraryRootPath  + path;
			if (associated_datafile != "") associated_datafile = _dataLibraryRootPath + associated_datafile;
			_entries.append(createEntry(path, name, description, FSEntry::getEntryTypeFromPath(path), associated_datafile));
		}
	}

	emit entriesChanged();

}

void FSBMDataLibrary::loadFilesAndFolders(const QString &docpath)
{

	bool found = false;
	QString relpath = "";

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
			QString description = file_or_folder["description"].toString();
			QString type = file_or_folder["kind"].toString();
			QString associated_datafile = file_or_folder["associated_datafile"].toString();
			QString debug =  file_or_folder["debug"].toString();
#ifndef JASP_DEBUG
			if (debug.toLower() == "true") continue;		
#endif		
			if (isFolder(type))
			{
				path = _path + QDir::separator()  + path;
				_entries.append(createEntry(path, name, description, FSEntry::Folder, ""));
			}
			else
			{
				path = _dataLibraryRootPath + relpath +  QDir::separator() + path;
				if (associated_datafile != "") associated_datafile = _dataLibraryRootPath + relpath + associated_datafile;
				_entries.append(createEntry(path, name, description, FSEntry::getEntryTypeFromPath(path), associated_datafile));
			}
		}
	}

	emit entriesChanged();

}

QJsonDocument *FSBMDataLibrary::getJsonDocument()
{

	QString filename = "index";

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

bool FSBMDataLibrary::isFolder(const QString &kind)
{
	return (kind.toLower() == "folder" ? true : false);
}

ExtendedFSEntry FSBMDataLibrary::createEntry(const QString &path, const QString &name, const QString &description, FSEntry::EntryType type, const QString &associated_datafile)
{
	ExtendedFSEntry entry;

	entry.name = name;
	entry.path = path;
	entry.description = description;
	entry.entryType = type;
	entry.associated_datafile = associated_datafile;

	return entry;
}

const FSBMDataLibrary::FileSystemExtendedEntryList &FSBMDataLibrary::entries() const
{
	return _entries;
}
