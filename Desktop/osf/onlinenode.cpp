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

QString OnlineNode::nodeId() const
{
	return _nodeId;
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
	{
		_connection = new OnlineDataConnection(_manager, this);

		connect(_connection, &OnlineDataConnection::finished, this, &OnlineNode::connectionFinished);
	}

	return _connection;
}

void OnlineNode::connectionFinished()
{
	OnlineDataConnection *conn = connection();
	if (conn->error())
		setError(true, conn->errorMessage());
	else if (_reinitialise)
	{
		initialise();
		return;
	}

	_reinitialise = false;
	emit finished();
}

void OnlineNode::startInit()
{
	setError(false, "");

	_inited = false;
}

void OnlineNode::endInit(bool success)
{
	_inited = success;

	if (success == false || _reinitialise || beginAction() == false)
	{
		_reinitialise = false;
		emit finished();
	}
}

void OnlineNode::setError(bool value, QString message)
{
	_error = value;
	_errorMsg = message;
}
