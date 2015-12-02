#include "fsbmosf.h"

#include <QGridLayout>
#include <QLabel>
#include <QNetworkAccessManager>
#include <QUrl>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QJsonArray>
#include <QFile>

#include "fsentry.h"
#include "onlinedatamanager.h"

FSBMOSF::FSBMOSF()
{
	_rootPath = _path = "Projects";
}

FSBMOSF::~FSBMOSF()
{

}

void FSBMOSF::setOnlineDataManager(OnlineDataManager *odm)
{
	_manager = odm->getNetworkAccessManager(OnlineDataManager::OSF);

	getUserDetails();
	refresh();
}

void FSBMOSF::refresh()
{
	if (_manager == NULL)
		return;

	if (_path == "Projects")
		loadProjects();
	else {
		OnlineNodeData nodeData = _pathUrls[_path];
		if (nodeData.isFolder)
			loadFilesAndFolders(QUrl(nodeData.contentsPath));
	}
}

void FSBMOSF::getUserDetails()
{
	QUrl url("https://staging2-api.osf.io/v2/users/me/");
	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(gotUserDetails()));
}

void FSBMOSF::gotUserDetails()
{
	QNetworkReply *reply = (QNetworkReply*)this->sender();

	if (reply->error() != QNetworkReply::NoError)
		return;

	QByteArray data = reply->readAll();
	QString dataString = (QString) data;

	QJsonParseError error;
	QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

	QJsonObject json = doc.object();

	QJsonObject dataObj = json.value("data").toObject();
	_userId = dataObj.value("id").toString();

	QJsonObject attributes = dataObj.value("attributes").toObject();
	_fullname = attributes.value("full_name").toString();

	reply->deleteLater();
}

void FSBMOSF::loadProjects() {

	_entries.clear();
	emit entriesChanged();

	QUrl url("https://staging2-api.osf.io/v2/users/me/nodes/");
	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(gotProjects()));
}

void FSBMOSF::gotProjects()
{
	QNetworkReply *reply = (QNetworkReply*)this->sender();

	_entries.clear();

	if (reply->error() != QNetworkReply::NoError)
		return;

	QByteArray data = reply->readAll();
	QString dataString = (QString) data;

	QJsonParseError error;
	QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

	QJsonObject json = doc.object();
	QJsonArray dataArray = json.value("data").toArray();

	foreach (const QJsonValue & value, dataArray) {
		QJsonObject nodeObject = value.toObject();

		QJsonObject attrObj = nodeObject.value("attributes").toObject();

		QString category = attrObj.value("category").toString();
		if (category != "project")
			continue;

		OnlineNodeData nodeData;

		nodeData.name = attrObj.value("title").toString();
		nodeData.isFolder = true;

		QJsonObject relationshipsObj = nodeObject.value("relationships").toObject();
		QJsonObject filesObj = relationshipsObj.value("files").toObject();
		QJsonObject linksObj = filesObj.value("links").toObject();
		QJsonObject relatedObj = linksObj.value("related").toObject();

		nodeData.contentsPath = relatedObj.value("href").toString();// + "/osfstorage/";

		QString path = _path + "/" + nodeData.name;
		_entries.append(createEntry(path, FSEntry::Folder));

		_pathUrls[path] = nodeData;

	}

	emit entriesChanged();
	reply->deleteLater();
}

void FSBMOSF::loadFilesAndFolders(QUrl url) {

	_entries.clear();
	emit entriesChanged();

	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(gotFilesAndFolders()));
}

void FSBMOSF::gotFilesAndFolders() {

	QNetworkReply *reply = (QNetworkReply*)this->sender();

	_entries.clear();

	if (reply->error() != QNetworkReply::NoError)
		return;

	QByteArray data = reply->readAll();
	QString dataString = (QString) data;

	QJsonParseError error;
	QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

	QJsonObject json = doc.object();
	QJsonArray dataArray = json.value("data").toArray();

	foreach (const QJsonValue & value, dataArray) {
		QJsonObject nodeObject = value.toObject();

		QJsonObject attrObj = nodeObject.value("attributes").toObject();

		QString kind = attrObj.value("kind").toString();
		if (kind != "folder" && kind != "file")
			continue;

		OnlineNodeData nodeData;
		nodeData.name = attrObj.value("name").toString();

		FSEntry::EntryType entryType = FSEntry::Other;

		if (kind == "folder")
			entryType = FSEntry::Folder;
		else if (nodeData.name.endsWith(".jasp", Qt::CaseInsensitive))
			entryType = FSEntry::JASP;
		else if (nodeData.name.endsWith(".csv", Qt::CaseInsensitive))
			entryType = FSEntry::CSV;
#ifdef QT_DEBUG
		else if (nodeData.name.endsWith(".spss", Qt::CaseInsensitive))
			entryType = FSEntry::SPSS;
#endif
		else
			continue;


		QString path = _path + "/" + nodeData.name;

		if (entryType == FSEntry::Folder) {
			QJsonObject relationshipsObj = nodeObject.value("relationships").toObject();
			QJsonObject filesObj = relationshipsObj.value("files").toObject();
			QJsonObject linksObj = filesObj.value("links").toObject();
			QJsonObject relatedObj = linksObj.value("related").toObject();

			nodeData.contentsPath = relatedObj.value("href").toString();
			nodeData.isFolder = true;
		}
		else {
			QJsonObject linksObj = nodeObject.value("links").toObject();
			nodeData.isFolder = false;

			nodeData.uploadPath = linksObj.value("upload").toString();
			nodeData.downloadPath = linksObj.value("download").toString();
			nodeData.nodePath = linksObj.value("info").toString();
		}

		_entries.append(createEntry(path, entryType));
		_pathUrls[path] = nodeData;

	}

	emit entriesChanged();
	reply->deleteLater();
}



FSBMOSF::OnlineNodeData FSBMOSF::getNodeData(QString key)
{
	return _pathUrls[key];
}
