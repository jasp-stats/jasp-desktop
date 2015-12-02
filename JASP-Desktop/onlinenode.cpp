#include "onlinenode.h"


OnlineNode::OnlineNode(QNetworkAccessManager *manager, QString id, QObject *parent):
	QObject(parent)
{
	_manager = manager;
	_id = id;
}

QString OnlineNode::id() const
{
	return _id;
}

bool OnlineNode::error() const
{
	return _error;
}

QString OnlineNode::errorMessage() const
{
	return _errorMsg;
}

QNetworkAccessManager* OnlineNode::manager() const
{
	return _manager;
}

QString OnlineNode::path() const
{
	return _path;
}

void OnlineNode::setPath(const QString &path)
{
	_path = path;
}

OnlineDataConnection* OnlineNode::connection()
{
	if (_connection == NULL)
		_connection = new OnlineDataConnection(_id, _manager);

	return _connection;
}
