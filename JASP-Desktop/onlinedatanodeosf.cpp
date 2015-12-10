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
}

void OnlineDataNodeOSF::initialise() {

	startInit();

	QUrl url = QUrl(_path);

	_subPath = url.fragment().split('#');

	if (_subPath.last().startsWith("file://"))
		_kind = OnlineDataNode::File;
	else if (_subPath.last().startsWith("folder://"))
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
	bool finished = false;

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


		if (json.value("data").isArray() && _expectedName != "")
		{
			bool lastItem = _subPath.count() == 1;
			QString searchName =_subPath.first();
			searchName = searchName.right(searchName.length() - searchName.lastIndexOf("/") - 1);

			QJsonArray arrayObject = json.value("data").toArray();

			bool found = searchList(searchName, lastItem ? _kind : OnlineDataNode::Folder, arrayObject, nodeObject);

			if ( ! found)
			{
			   QJsonObject contentLevelLinks = json.value("links").toObject();

				QJsonValue nextContentList = contentLevelLinks.value("next");
				if (nextContentList.isNull() == false)
				{
					processUrl(QUrl(nextContentList.toString()));
					finished = false;
				}
				else if (lastItem)
				{
					finished = true;
					success = true;
				}
				else
				{
					finished = true;
					success = false;
				}
			}
			else if (_subPath.count() > 1)
			{
				_subPath.removeFirst();
				QString basePath = "";
				if (_subPath.count() == 1)
				{
					populateNodeData(nodeObject);
					basePath = getBaseUrl(nodeObject);
				}
				else
					basePath = getContentsUrl(nodeObject);
				processUrl(QUrl(basePath));
				finished = false;
			}
			else
			{
				_subPath.removeFirst();
				finished = interpretNode(nodeObject);
				success = true;
			}
		}
		else if (json.value("data").isObject())
		{
			nodeObject = json.value("data").toObject();
			finished = interpretNode(nodeObject);
			success = true;
		}
		else
		{
			setError(true, "Online node data in unknown form.");
			finished = true;
			success = false;
		}
	}

	reply->deleteLater();

	if (finished)
		endInit(success);
}

bool OnlineDataNodeOSF::searchList(QString searchName, OnlineDataNode::Kind kind, QJsonArray arrayObject, QJsonObject &nodeObject)
{
	bool found = false;
	foreach (const QJsonValue & value, arrayObject)
	{

		if (value.isObject() == false)
			continue;

		QJsonObject s1 = value.toObject();
		QJsonObject attrObj = s1.value("attributes").toObject();
		QString name = attrObj.value("name").toString();
		QString dataKindString = attrObj.value("kind").toString();

		OnlineDataNode::Kind dataKind = OnlineDataNode::Unknown;
		if (dataKindString == "folder")
			dataKind = OnlineDataNode::Folder;
		else if (dataKindString == "file")
			dataKind = OnlineDataNode::File;

		if (name == searchName && kind == dataKind)
		{
			nodeObject = s1;
			found = true;
			break;
		}
	}

	return found;
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
	populateNodeData(nodeObject);

	if (_dataKind == OnlineDataNode::Folder && _subPath.count() == 1)
	{	
		processUrl(QUrl(getContentsUrl(nodeObject)));
		return false;
	}

	return true;
}

void OnlineDataNodeOSF::populateNodeData(QJsonObject nodeObject)
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
}

QString OnlineDataNodeOSF::getContentsUrl(QJsonObject nodeObject)
{
	QJsonObject a = nodeObject.value("relationships").toObject();
	QJsonObject ab = a.value("files").toObject();
	QJsonObject abc = ab.value("links").toObject();
	QJsonObject abcd = abc.value("related").toObject();

	return abcd.value("href").toString();
}

QString OnlineDataNodeOSF::getBaseUrl(QJsonObject nodeObject)
{
	QJsonObject linksObj = nodeObject.value("links").toObject();

	QString url = linksObj.value("info").toString();

	if (url == "")
		url = getContentsUrl(nodeObject);

	return url;
}

/*void OnlineDataNodeOSF::checkFinished()
{
	OnlineDataNodeOSF *childNode = qobject_cast<OnlineDataNodeOSF *>(sender());

	if (childNode->_inited)
		extractNodeData(childNode);

	childNode->deleteLater();

	endInit(true);
}*/


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
