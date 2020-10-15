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

#include <QFile>
#include <QDir>
#include <QJsonDocument>
#include <QJsonParseError>
#include <QJsonArray>
#include <QJsonObject>

#include "utilities/appdirs.h"
#include "utilities/qutils.h"
#include "log.h"
#include "utilities/languagemodel.h"


const QString DataLibraryFileSystem::rootelementname = "Categories";

DataLibraryFileSystem::DataLibraryFileSystem(QObject *parent, QString root)	: FileSystem(parent)
{
	_rootPath = _path = root;
	_dataLibraryRootPath = "";
	_doc = NULL;

}

DataLibraryFileSystem::~DataLibraryFileSystem()
{
	if (_doc)
		delete _doc;
}

void DataLibraryFileSystem::refresh()
{

	if (_doc != NULL) delete _doc;  //When language is changed;

	_doc = getJsonDocument();

	emit processingEntries();

	_entries.clear();

	if (_path == DataLibraryFileSystem::rootelementname)
		loadRootElements();
	else
		loadFilesAndFolders(_path);

}

void DataLibraryFileSystem::loadRootElements()
{

	if (_doc == NULL)
		return;

	QJsonObject jsonroot = _doc->object();
	QJsonArray children = jsonroot.value("children").toArray();
	
	if (_dataLibraryRootPath == "")
		_dataLibraryRootPath = AppDirs::examples() +  QDir::separator()  + jsonroot["path"].toString() + QDir::separator(); 

	_entries.clear();

	//Loop over different children
	for(const QJsonValue & value : children)
	{
		QJsonObject file_or_folder = value.toObject();

		QString path		= file_or_folder["path"].toString(),
				name		= file_or_folder["name"].toString(),
				description = file_or_folder["description"].toString(),
				type		= file_or_folder["kind"].toString(),
				debug		= file_or_folder["debug"].toString();
#ifndef JASP_DEBUG
		if (debug.toLower() == "true") continue;		
#endif	
		QString associated_datafile = file_or_folder["associated_datafile"].toString();
		if (isFolder(type))
		{
			path = _path + QDir::separator()  + path;
			_entries.append(createEntry(path, name, description, FileSystemEntry::Folder, associated_datafile));
		}
		else
		{
			path = _dataLibraryRootPath  + path;
			if (associated_datafile != "") associated_datafile = _dataLibraryRootPath + associated_datafile;
			_entries.append(createEntry(path, name, description, FileSystemEntry::getEntryTypeFromPath(path), associated_datafile));
		}
	}

	emit entriesChanged();

}

void DataLibraryFileSystem::loadFilesAndFolders(const QString &docpath)
{

	bool found = false;
	QString relpath = "";

	if (_doc == NULL)
		return;

	QJsonObject jsonroot = _doc->object();
	QJsonArray folder = jsonroot.value("children").toArray();

	QStringList list = docpath.split(QChar( QDir::separator() ), QString::SkipEmptyParts);

	for(QString itm : list)
	{
		if (itm == _rootPath)
			continue;

		relpath += itm + QDir::separator();

		found = false;

		for(const QJsonValue & value : folder)
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
		for(const QJsonValue & value : folder)
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
				_entries.append(createEntry(path, name, description, FileSystemEntry::Folder, ""));
			}
			else
			{
				path = _dataLibraryRootPath + relpath +  QDir::separator() + path;
				if (associated_datafile != "") associated_datafile = _dataLibraryRootPath + relpath + associated_datafile;
				_entries.append(createEntry(path, name, description, FileSystemEntry::getEntryTypeFromPath(path), associated_datafile));
			}
		}
	}

	emit entriesChanged();

}

QJsonDocument *DataLibraryFileSystem::getJsonDocument()
{
	QFile index(AppDirs::examples() + QDir::separator() + "index" + LanguageModel::currentTranslationSuffix() + ".json");
	
	bool exists = index.exists();
	
	if (!exists)
	{
		//Fall back to English
		if (!(LanguageModel::lang()->currentLanguage() == QLocale::English))
		{	
			index.setFileName(AppDirs::examples() + QDir::separator() + "index.json");
			exists = index.exists();		
		}
	}
	
	if (!exists)
	{		
		Log::log()  << "BackStageForm::loadExamples();  index not found" << std::endl;
		return nullptr;
	}

	index.open(QFile::ReadOnly);
	if ( ! index.isOpen())
	{
		Log::log()  << "BackStageForm::loadExamples();  index could not be opened" << std::endl;
		return nullptr;
	}


	QByteArray bytes = index.readAll();
	QJsonParseError error;
	QJsonDocument doc = QJsonDocument::fromJson(bytes, &error);

	if (error.error != QJsonParseError::NoError)
	{
		Log::log()  << "BackStageForm::loadExamples();  JSON parse error : " << error.errorString().toStdString()  << std::endl;
		return nullptr;
	}

	QJsonDocument *d = new QJsonDocument;
	*d = doc;
	return  d;

}

bool DataLibraryFileSystem::isFolder(const QString &kind)
{
	return (kind.toLower() == "folder" ? true : false);
}

