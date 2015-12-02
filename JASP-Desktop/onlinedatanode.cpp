#include "onlinedatanode.h"

OnlineDataNode::OnlineDataNode(QNetworkAccessManager *manager, QString id, QObject *parent):
	QObject(parent)
{
	_manager = manager;
	_id = id;
}

QString OnlineDataNode::id() const
{
	return _id;
}

bool OnlineDataNode::error() const
{
	return _error;
}

QString OnlineDataNode::errorMessage() const
{
	return _errorMsg;
}

QNetworkAccessManager* OnlineDataNode::manager() const
{
	return _manager;
}

QString OnlineDataNode::getUploadPath() const
{
	return _uploadPath;
}

QString OnlineDataNode::getDeletePath() const
{
	return _deletePath;
}

QString OnlineDataNode::getDownloadPath() const
{
	return _downloadPath;
}

QString OnlineDataNode::name() const
{
	return _name;
}

bool OnlineDataNode::kind() const
{
	return _kind;
}

QString OnlineDataNode::path() const
{
	return _path;
}

void OnlineDataNode::setPath(const QString &path)
{
	_path = path;
}

OnlineDataNode::ConnectionType OnlineDataNode::downloadConnectionType() const {

	return _downloadConnectionType;
}

OnlineDataNode::ConnectionType OnlineDataNode::uploadConnectionType() const {

	return _uploadConnectionType;
}

OnlineDataConnection* OnlineDataNode::connection()
{
	if (_connection == NULL)
		_connection = new OnlineDataConnection(_id, _manager);

	return _connection;
}
