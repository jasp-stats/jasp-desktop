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

#include "osffilesystem.h"

#include <QNetworkAccessManager>
#include <QUrl>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QJsonArray>
#include <QFile>
#include <QDateTime>


#include "filesystementry.h"
#include "osf/onlinedatamanager.h"
#include "osf/onlineusernodeosf.h"
#include "widgets/filemenu/osf.h"
#include "utilities/simplecrypt.h"
#include "utilities/settings.h"
#include "utilities/qutils.h"
#include "utilities/messageforwarder.h"
#include <iostream>

const QString OSFFileSystem::rootelementname = "Projects";

OSFFileSystem::OSFFileSystem(QObject *parent, QString root)
	:FileSystem (parent)
{
	_dataManager = NULL;
	_manager = NULL;
	_isAuthenticated = false;
	_rootPath = _path = root;
}

OSFFileSystem::~OSFFileSystem()
{

}

void OSFFileSystem::setOnlineDataManager(OnlineDataManager *odm)
{
	_dataManager = odm;

	_manager = odm->getNetworkAccessManager(OnlineDataManager::OSF);

}

void OSFFileSystem::attemptToConnect()
{
	QString password = _dataManager->getPassword(OnlineDataManager::OSF);
	QString username = _dataManager->getUsername(OnlineDataManager::OSF);

	if ( username=="" || password =="" )
	{
		emit stopProcessing();
		return;
	}

	if ( _isAuthenticated == false && _dataManager != NULL )
	{
		emit processingEntries();

		OnlineUserNodeOSF *userNode = (OnlineUserNodeOSF *)_dataManager->getOnlineUserData("https://staging2-api.osf.io/v2/users/me/", "fsbmosf");
		userNode->login();
		connect(userNode, &OnlineUserNodeOSF::authenticationResult, this, &OSFFileSystem::handleAuthenticationResult);
	}
	else emit stopProcessing();
}

void OSFFileSystem::handleAuthenticationResult(bool authsuccess)
{
	setAuthenticated(authsuccess);
	if (!authsuccess)
		emit entriesChanged();
	emit stopProcessing();
}

void OSFFileSystem::updateAuthentication(bool authenticated)
{
	setAuthenticated(authenticated);
}


bool OSFFileSystem::requiresAuthentication() const
{
	return true;
}

void OSFFileSystem::setAuthenticated(bool value)
{
	if (value)
	{
		_isAuthenticated = true;
		emit authenticationSucceeded();
		refresh();
	}
	else
	{
		emit stopProcessing();
		_isAuthenticated = false;
		emit authenticationFailed("Username and/or password are not correct. Please try again.");
	}
}

bool OSFFileSystem::isAuthenticated() const
{
	return _isAuthenticated;
}

void OSFFileSystem::clearAuthentication()
{
	_isAuthenticated = false;
	_dataManager->clearAuthentication(OnlineDataManager::OSF);
	_entries.clear();
	_pathUrls.clear();
	setPath(_rootPath);
	emit entriesChanged();
	emit authenticationClear();
}

void OSFFileSystem::refresh()
{
	if (_manager == NULL || _isAuthenticated == false)
		return;

	emit processingEntries();

	_entries.clear();

	if (_path == "Projects")
		loadProjects();
	else {
		OnlineNodeData nodeData = _pathUrls[_path];
		if (nodeData.isFolder)
			loadFilesAndFolders(QUrl(nodeData.contentsPath), nodeData.level + 1);
		if (nodeData.isComponent)
			loadFilesAndFolders(QUrl(nodeData.childrenPath), nodeData.level + 1);
	}
}

OSFFileSystem::OnlineNodeData OSFFileSystem::currentNodeData()
{
	return _pathUrls[_path];
}

void OSFFileSystem::sortItems(Sortable::SortType sortOrder)
{
	Settings::setValue(Settings::OSF_SORTORDER, int(sortOrder));
	if (sortOrder == SortType::None) _entries =_unsortedEntries;
	sortEntries(sortOrder);
	emit entriesChanged();
}

void OSFFileSystem::loadProjects() {
	QUrl url("https://api.osf.io/v2/users/me/nodes/");
	parseProjects(url);
}

void OSFFileSystem::parseProjects(QUrl url, bool recursive) {

	_isProjectPaginationCall = recursive;
	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, &QNetworkReply::finished, this, &OSFFileSystem::gotProjects);
}

void OSFFileSystem::handleNetworkReplyError(QNetworkReply* reply)
{
	OSF::checkErrorMessageOSF(reply);
}

void OSFFileSystem::gotProjects()
{
	QNetworkReply *reply = (QNetworkReply*)this->sender();

	if (reply->error() == QNetworkReply::NoError)
	{
		QByteArray data = reply->readAll();
		QString dataString = (QString) data;
	
		QJsonParseError error;
		QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

		QJsonObject json = doc.object();
		QJsonArray dataArray = json.value("data").toArray();

		if ( dataArray.size() > 0 && !_isProjectPaginationCall)
			_entries.clear();

		for (const QJsonValue & value : dataArray) {
			QJsonObject nodeObject = value.toObject();

			QJsonObject attrObj = nodeObject.value("attributes").toObject();

			QString category = attrObj.value("category").toString();
			if (category != "project")
				continue;

			OnlineNodeData nodeData;

			nodeData.name = attrObj.value("title").toString();
			nodeData.isFolder = true;
			nodeData.isComponent = true;

			nodeData.contentsPath = getRelationshipUrl(nodeObject, "files");
			nodeData.childrenPath = getRelationshipUrl(nodeObject, "children");

			QJsonObject topLinksObj = nodeObject.value("links").toObject();

			nodeData.nodePath = topLinksObj.value("self").toString();
			nodeData.level = 1;
			nodeData.canCreateFolders = false;
			nodeData.canCreateFiles = false;

			QString path = _path + "/" + nodeData.name;
			_entries.append(createEntry(path, FileSystemEntry::Folder));

			_pathUrls[path] = nodeData;

		}

		QJsonObject links = json.value("links").toObject();

		QJsonValue nextProjects = links.value("next");
		if (nextProjects.isNull() == false)
			parseProjects(QUrl(nextProjects.toString()), true);
		else
		{
			_unsortedEntries = _entries;
			if (currentSortType() != SortType::None)
				Sortable::sortItems();
			else
				emit entriesChanged();
		}
	}
	else
	{
		handleNetworkReplyError(reply);
		emit newLoginRequired();
		emit stopProcessing();
	}

	reply->deleteLater();
}

void OSFFileSystem::loadFilesAndFolders(QUrl url, int level)
{
	parseFilesAndFolders(url, level);
}

void OSFFileSystem::parseFilesAndFolders(QUrl url, int level, bool recursive)
{
	_level = level;
	_isPaginationCall = recursive;

	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, &QNetworkReply::finished, this, &OSFFileSystem::gotFilesAndFolders);
}

void OSFFileSystem::gotFilesAndFolders()
{
	QNetworkReply *reply = (QNetworkReply*)this->sender();

	if (reply->error() == QNetworkReply::NoError)
	{
		QByteArray	data			= reply->readAll();
		QString		dataString		= data,
					date_created	= "",
					date_modified	= "";
		QDateTime	created,
					modified;

		QJsonParseError error;
		QJsonDocument	doc			= QJsonDocument::fromJson(dataString.toUtf8(), &error);

		QJsonObject		json		= doc.object();
		QJsonArray		dataArray	= json.value("data").toArray();

		if ( dataArray.size() > 0 && !_isPaginationCall)
			_entries.clear();

		for (const QJsonValue & value : dataArray) {
			QJsonObject nodeObject = value.toObject();

			OnlineNodeData nodeData;

			nodeData.isComponent = nodeObject.value("type").toString() == "nodes";

			FileSystemEntry::EntryType entryType	= FileSystemEntry::Other;
			QJsonObject attrObj						= nodeObject.value("attributes").toObject();

			if (nodeData.isComponent == false)
			{
				QString kind = attrObj.value("kind").toString();
				if (kind != "folder" && kind != "file")
					continue;

				date_created	= attrObj.value("date_created").toString();
				date_modified	= attrObj.value("date_modified").toString();

				modified = created = QDateTime();

				if (date_modified	!= "") modified = osfJsonToDateTime(date_modified);
				if (date_created	!= "") created  = osfJsonToDateTime(date_created);

				nodeData.name = attrObj.value("name").toString();

				auto endsWith = [&](QStringList exts)
				{
					for(QString ext : exts) if(nodeData.name.endsWith(ext, Qt::CaseInsensitive)) return true;
					return false;
				};

				static const QStringList readStatFormats = {".sav", ".sas7bdat", ".sas7bcat", ".por", ".xpt", ".dta"};

				if (kind == "folder")									entryType = FileSystemEntry::Folder;
				else if (endsWith({".jasp"}))							entryType = FileSystemEntry::JASP;
				else if (endsWith({".csv", ".txt", ".tsv", ".ods"})) 	entryType = FileSystemEntry::CSV;
				else if (endsWith({".html", ".pdf"}))					entryType = FileSystemEntry::Other;
				else if (endsWith(readStatFormats))						entryType = FileSystemEntry::ReadStat;
				else													continue;
			}
			else
			{
				entryType = FileSystemEntry::Folder;
				nodeData.name = attrObj.value("title").toString();
			}

			if (entryType == FileSystemEntry::Folder)
			{
				nodeData.contentsPath = getRelationshipUrl(nodeObject, "files");
				nodeData.childrenPath = getRelationshipUrl(nodeObject, "children");

				QJsonObject topLinksObj = nodeObject.value("links").toObject();

				if (nodeData.isComponent)	nodeData.nodePath = topLinksObj.value("self").toString();
				else						nodeData.nodePath = topLinksObj.value("info").toString();

				nodeData.uploadPath			= topLinksObj.value("upload").toString();
				nodeData.isFolder			= true;

				nodeData.canCreateFolders	= topLinksObj.contains("new_folder");
				nodeData.canCreateFiles		= topLinksObj.contains("upload");

				if (nodeData.nodePath == "")
					nodeData.nodePath = reply->url().toString() + "#folder://" + nodeData.name;
			}
			else
			{
				QJsonObject linksObj		= nodeObject.value("links").toObject();
				nodeData.isFolder			= false;

				nodeData.uploadPath			= linksObj.value("upload").toString();
				nodeData.downloadPath		= linksObj.value("download").toString();
				nodeData.nodePath			= linksObj.value("info").toString();

				nodeData.canCreateFolders	= false;
				nodeData.canCreateFiles		= false;
			}


			QString path = _path + "/" + nodeData.name;

			nodeData.level = _level;

			FileSystemEntry fe = createEntry(path, entryType);
			fe.created = created;
			fe.modified = modified;

			_entries.append(fe);
			_pathUrls[path] = nodeData;

		}

		QJsonObject contentLevelLinks = json.value("links").toObject();

		QJsonValue nextContentList = contentLevelLinks.value("next");

		if (nextContentList.isNull() == false)
			parseFilesAndFolders(QUrl(nextContentList.toString()), _level + 1, true);
		else
		{
			_unsortedEntries = _entries;
			if (currentSortType() != SortType::None)
				Sortable::sortItems();
			else
				emit entriesChanged();
		}
	}
	else
	{
		handleNetworkReplyError(reply);
		emit newLoginRequired();
		emit stopProcessing();
	}

	reply->deleteLater();
}


QString OSFFileSystem::getRelationshipUrl(QJsonObject nodeObject, QString name)
{
	QJsonObject relationshipsObj = nodeObject.value("relationships").toObject();

	if (relationshipsObj.contains(name) == false)
		return "";

	QJsonObject filesObj	= relationshipsObj.value(name).toObject();
	QJsonObject linksObj	= filesObj.value("links").toObject();
	QJsonObject relatedObj	= linksObj.value("related").toObject();

	return relatedObj.value("href").toString();
}

OSFFileSystem::OnlineNodeData OSFFileSystem::getNodeData(QString key)
{
	return _pathUrls[key];
}

QDateTime OSFFileSystem::osfJsonToDateTime(const QString &input)
{
	// JSON OSF modified file format is e.g. 2016-08-23T15:11:56.857000Z
    QRegularExpression		separator("(-|T|:|Z)");
    QStringList             list = input.split(separator);
    QDate                   date(list[0].toInt(),list[1].toInt(),list[2].toInt());
    QTime                   time(list[3].toInt(),list[4].toInt(),static_cast<int>(list[5].toDouble()));
    QDateTime               datetime(date,time);

	return datetime;

}
