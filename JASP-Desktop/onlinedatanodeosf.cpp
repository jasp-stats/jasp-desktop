#include "onlinedatanodeosf.h"

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QUrl>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QByteArray>
#include <QUrlQuery>

#include <stdexcept>

using namespace std;

OnlineDataNodeOSF::OnlineDataNodeOSF(QString localPath, QNetworkAccessManager *manager, QString id, QObject *parent):
	OnlineDataNode(localPath, manager, id, parent)
{
	_uploadConnectionType = OnlineDataNode::Put;
}

void OnlineDataNodeOSF::initialise() {

	startInit();

	QUrl url = QUrl(_path);

	if (url.fragment().startsWith("file://"))
		_kind = OnlineDataNode::File;
	else if (url.fragment().startsWith("folder://"))
		_kind = OnlineDataNode::Folder;
	else
		_kind = OnlineDataNode::Unknown;

	if (_kind == OnlineDataNode::File || _kind == OnlineDataNode::Folder)
		_expectedName = _path.right(_path.length() - _path.lastIndexOf("/") - 1);

	processUrl(url);
}


void OnlineDataNodeOSF::processUrl(QUrl url)
{
	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(nodeInfoReceived()));
}


void OnlineDataNodeOSF::nodeInfoReceived() {

	QNetworkReply *reply = (QNetworkReply*)this->sender();

	bool success = false;
	bool finished = true;

	if (reply->error() != QNetworkReply::NoError)
		setError(true, reply->errorString());
	else
	{
		QByteArray data = reply->readAll();
		QString dataString = (QString) data;

		QJsonParseError error;
		QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

		QJsonObject json = doc.object();

		QJsonObject nodeObject;
		bool found = true;
		if (json.value("data").isArray() && _expectedName != "")
		{
			found = false;
			QJsonArray arrayObject = json.value("data").toArray();
			foreach (const QJsonValue & value, arrayObject)
			{

				if (value.isObject() == false)
					continue;

				nodeObject = value.toObject();
				QJsonObject attrObj = nodeObject.value("attributes").toObject();
				QString name = attrObj.value("name").toString();
				QString dataKindString = attrObj.value("kind").toString();

				OnlineDataNode::Kind dataKind = OnlineDataNode::Unknown;
				if (dataKindString == "folder")
					dataKind = OnlineDataNode::Folder;
				else if (dataKindString == "file")
					dataKind = OnlineDataNode::File;

				if (name == _expectedName && _kind == dataKind)
				{
					found = true;
					break;
				}
			}

			if (found == false)
			{
				QJsonObject contentLevelLinks = json.value("links").toObject();

				QJsonValue nextContentList = contentLevelLinks.value("next");
				if (nextContentList.isNull() == false)
				{
					processUrl(QUrl(nextContentList.toString()));
					finished = false;
				}
			}
		}
		else if (json.value("data").isObject())
			nodeObject = json.value("data").toObject();
		else
		{
			setError(true, "Online node data in unknown form.");
			found = false;
		}

		if (found)
			finished = interpretNode(nodeObject);

		success = found;
	}

	reply->deleteLater();

	if (finished)
		endInit(success);
}

OnlineDataNode::Kind OnlineDataNodeOSF::parseKind(QString kind)
{
	if (kind == "folder")
		return OnlineDataNode::Folder;
	else if (kind == "file")
		return OnlineDataNode::File;
	else
		return OnlineDataNode::Unknown;
}

bool OnlineDataNodeOSF::interpretNode(QJsonObject nodeObject)
{
	QJsonObject attrObj = nodeObject.value("attributes").toObject();

	QString dataKind = attrObj.value("kind").toString();

	_name = attrObj.value("name").toString();

	_dataKind = parseKind(dataKind);

	if (_kind == OnlineDataNode::Unknown)
		_kind = _dataKind;

	QJsonObject linksObj = nodeObject.value("links").toObject();

	if (_dataKind == OnlineDataNode::File)
	{
		_uploadPath = linksObj.value("upload").toString();
		_downloadPath = linksObj.value("download").toString();
		_newFolderPath = "";
		_deletePath = linksObj.value("delete").toString();
	}
	else if (_dataKind == OnlineDataNode::Folder)
	{
		_uploadPath = linksObj.value("upload").toString();
		_downloadPath = "";
		_newFolderPath = linksObj.value("new_folder").toString();
		_deletePath = linksObj.value("delete").toString();
	}

	if (_dataKind == OnlineDataNode::Folder && _expectedName != "")
	{
		QJsonObject a = nodeObject.value("relationships").toObject();
		QJsonObject ab = a.value("files").toObject();
		QJsonObject abc = ab.value("links").toObject();
		QJsonObject abcd = abc.value("related").toObject();

		QString contentsPath = abcd.value("href").toString();

		OnlineDataNodeOSF *checkNode = new OnlineDataNodeOSF("", _manager, _id, this);

		QUrl url = QUrl(_path);
		checkNode->setPath(contentsPath + "#" + url.fragment());

		connect(checkNode, SIGNAL(finished()), this, SLOT(checkFinished()));
		checkNode->initialise();

		return false;
	}

	return true;
}



void OnlineDataNodeOSF::checkFinished()
{
	OnlineDataNodeOSF *checkNode = qobject_cast<OnlineDataNodeOSF *>(sender());

	if (checkNode->_inited)
		extractNodeData(checkNode);

	checkNode->deleteLater();

	endInit(true);
}


QString OnlineDataNodeOSF::getUploadPath() const
{
	if (_dataKind == OnlineDataNode::File)
	{
		 QUrlQuery query(_uploadPath + "?");
		 query.addQueryItem("kind", "file");
		 return query.toString(QUrl::FullyEncoded);
	}
	else if (_dataKind == OnlineDataNode::Folder && _kind == OnlineDataNode::File)
	{
		QUrlQuery query(_uploadPath + "?");
		query.addQueryItem("kind", "file");
		query.addQueryItem("name", _expectedName);
		return query.toString(QUrl::FullyEncoded);
	}
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}

QString OnlineDataNodeOSF::getUploadPath(QString filename) const
{
	if (_dataKind == OnlineDataNode::File)
		throw runtime_error("Please use 'uploadPath()' for uploading an existing file.");
	else if (_dataKind == OnlineDataNode::Folder)
	{
		QUrlQuery query(_uploadPath + "?");
		query.addQueryItem("kind", "file");
		query.addQueryItem("name", filename);
		return query.toString(QUrl::FullyEncoded);
	}
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}

QString OnlineDataNodeOSF::getDownloadPath() const
{
	if (_dataKind == OnlineDataNode::File)
		return _downloadPath;
	else
		throw runtime_error("This node kind does not handle the 'download' action.");
}

QString OnlineDataNodeOSF::getNewFolderPath(QString folderName) const
{
	if (_dataKind == OnlineDataNode::Folder)
	{
		QUrlQuery query(_newFolderPath + "?");
		query.addQueryItem("kind", "folder");
		query.addQueryItem("name", folderName);
		return query.toString(QUrl::FullyEncoded);
	}
	else
		throw runtime_error("This node kind does not handle the 'new folder' action.");
}

QString OnlineDataNodeOSF::getDeletePath() const
{
	return _deletePath;
}


void OnlineDataNodeOSF::beginDownloadFile() {

	if (_dataKind == OnlineDataNode::File)
		connection()->beginAction(QUrl(getDownloadPath()), OnlineDataConnection::Get, &_localFile);
	else
		throw runtime_error("This node kind does not handle the 'download' action.");
}

void OnlineDataNodeOSF::beginUploadFile() {

	if (_dataKind == OnlineDataNode::File || (_dataKind == OnlineDataNode::Folder && _kind == OnlineDataNode::File)) {
		if (_localFile.exists())
			connection()->beginAction(QUrl(getUploadPath()), OnlineDataConnection::Put, &_localFile);
		else
			throw runtime_error("Local file cannot be found.");
	}
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}

void OnlineDataNodeOSF::beginUploadFile(QString name) {

	if (_dataKind == OnlineDataNode::Folder) {
		if (_localFile.exists())
			connection()->beginAction(QUrl(getUploadPath(name)), OnlineDataConnection::Put, &_localFile);
		else
			throw runtime_error("Local file cannot be found.");
	}
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}

void OnlineDataNodeOSF::beginNewFolder(QString name) {

	if (_dataKind == OnlineDataNode::Folder)
			connection()->beginAction(QUrl(getNewFolderPath(name)), OnlineDataConnection::Put, NULL);
	else
		throw runtime_error("This node kind does not handle the 'upload' action.");
}


void OnlineDataNodeOSF::extractNodeData(OnlineDataNodeOSF * node)
{
	_uploadPath = node->_uploadPath;
	_downloadPath = node->_downloadPath;
	_newFolderPath = node->_newFolderPath;
	_deletePath = node->_deletePath;
	_dataKind = node->_dataKind;
	_kind = node->_kind;
	_name = node->_name;
}
