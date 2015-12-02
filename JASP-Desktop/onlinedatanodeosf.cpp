#include "onlinedatanodeosf.h"

#include <QJsonDocument>
#include <QJsonObject>
#include <QUrl>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QByteArray>

using namespace std;

OnlineDataNodeOSF::OnlineDataNodeOSF(QNetworkAccessManager *manager, QString id, QObject *parent):
	OnlineDataNode(manager, id, parent)
{
	_uploadConnectionType = OnlineDataNode::Put;
}

void OnlineDataNodeOSF::getNodeInfo() const {

	QUrl url = QUrl(_path);

	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(nodeInfoReceived()));
}


void OnlineDataNodeOSF::nodeInfoReceived() {

	QNetworkReply *reply = (QNetworkReply*)this->sender();

	_error = false;
	_errorMsg = "";

	if (reply->error() != QNetworkReply::NoError)
	{
		_error = true;
		_errorMsg = reply->errorString();
	}
	else
	{
		QByteArray data = reply->readAll();
		QString dataString = (QString) data;

		QJsonParseError error;
		QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

		QJsonObject json = doc.object();
		QJsonObject nodeObject = json.value("data").toObject();

		QJsonObject attrObj = nodeObject.value("attributes").toObject();

		QString kind = attrObj.value("kind").toString();


		_name = attrObj.value("name").toString();

		if (kind == "folder")
			_kind = OnlineDataNode::Folder;
		else if (kind == "file")
			_kind = OnlineDataNode::File;

		QJsonObject linksObj = nodeObject.value("links").toObject();


		if (_kind == OnlineDataNode::File)
		{
			_uploadPath = linksObj.value("upload").toString();
			_downloadPath = linksObj.value("download").toString();
			_deletePath = linksObj.value("delete").toString();
		}
		else if (_kind == OnlineDataNode::Folder)
		{
			_uploadPath = linksObj.value("upload").toString();
			_newFolderPath = linksObj.value("new_folder").toString();
			_deletePath = linksObj.value("delete").toString();
		}

	}

	reply->deleteLater();

	emit finished();
}

QString OnlineDataNodeOSF::getUploadPath() const
{
	if (_kind == OnlineDataNode::File)
		return _uploadPath + "?kind=file";
	else if (_kind == OnlineDataNode::Folder)
		throw runtime_error("Must provide a filename when uploading to a folder.");
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}

QString OnlineDataNodeOSF::getUploadPath(QString filename) const
{
	if (_kind == OnlineDataNode::File)
		throw runtime_error("Please use 'uploadPath()' for uploading an existing file.");
	else if (_kind == OnlineDataNode::Folder)
		return _uploadPath + "?kind=folder&name=" + filename;
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}

QString OnlineDataNodeOSF::getDownloadPath() const
{
	if (_kind == OnlineDataNode::File)
		return _downloadPath;
	else
		throw runtime_error("This node kind does not handle the 'new folder' action.");
}

QString OnlineDataNodeOSF::getNewFolderPath(QString folderName) const
{
	if (_kind == OnlineDataNode::Folder)
		return _newFolderPath + "?kind=folder&name=" + folderName;
	else
		throw runtime_error("This node kind does not handle the 'new folder' action.");
}

QString OnlineDataNodeOSF::getDeletePath() const
{
	return _deletePath;
}
