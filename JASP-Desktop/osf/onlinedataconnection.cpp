#include "onlinedataconnection.h"

#include <QNetworkRequest>
#include <QNetworkReply>

OnlineDataConnection::OnlineDataConnection(QNetworkAccessManager *manager, QObject *parent):
	QObject(parent)
{
	_manager = manager;
}


QNetworkAccessManager* OnlineDataConnection::manager() const
{
	return _manager;
}

bool OnlineDataConnection::error() const
{
	return _error;
}

QString OnlineDataConnection::errorMessage() const
{
	return _errorMsg;
}


void OnlineDataConnection::beginAction(QUrl url, OnlineDataConnection::Type type, QIODevice *data)
{
	setError(false, "");

	_uploadFile = data;
	_actionType = type;

	if ((type == OnlineDataConnection::Put || type == OnlineDataConnection::Post) && data != NULL)
	{
		if (_uploadFile != NULL && _uploadFile->isOpen() == false && _uploadFile->open(QIODevice::ReadOnly) == false)
			setError(true, "File cannot be opened for online data action.");
		else if (_uploadFile != NULL && _uploadFile->isOpen() == true && _uploadFile->isReadable() == false)
			setError(true, "File is not readable for online data action.");
		else if (_uploadFile != NULL && _uploadFile->pos() != 0 && _uploadFile->reset() == false)
			setError(true, "File cannot be reset for online data action.");
	}

	QNetworkRequest request(url);

	QNetworkReply *reply;

	if (_error == false)
	{
		if (type == OnlineDataConnection::Put)
			reply = _manager->put(request, data);
		else if (type == OnlineDataConnection::Post)
			reply = _manager->post(request, data);
		else
			reply = _manager->get(request);

		connect(reply, SIGNAL(finished()), this, SLOT(actionFinished()));
	}
	else
		actionFinished(NULL);
}

void OnlineDataConnection::beginAction(QUrl url, OnlineDataConnection::Type type, const QByteArray &data)
{
	setError(false, "");

	_uploadFile = NULL;
	_actionType = type;

	QNetworkRequest request(url);

	QNetworkReply *reply;

	if (_error == false)
	{
		if (type == OnlineDataConnection::Put)
			reply = _manager->put(request, data);
		else if (type == OnlineDataConnection::Post)
			reply = _manager->post(request, data);
		else
			reply = _manager->get(request);

		connect(reply, SIGNAL(finished()), this, SLOT(actionFinished()));
	}
	else
		actionFinished(NULL);
}

void OnlineDataConnection::actionFinished()
{
	QNetworkReply *reply = (QNetworkReply*)this->sender();

	if(reply->error())
		setError(true, reply->errorString());
	else
	{
		int status = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
		if (status == 301 || status == 302)
		{
			 QUrl url = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
			 beginAction(url, _actionType, _uploadFile);
			 reply->deleteLater();
			 return;
		}
	}

	actionFinished(reply);
}

void OnlineDataConnection::actionFinished(QNetworkReply *reply)
{
	if (_error == false && reply != NULL)
	{
		if (_actionType == OnlineDataConnection::Get && _uploadFile != NULL)
		{
			if ((_uploadFile->isOpen() || _uploadFile->open(QIODevice::WriteOnly)) && _uploadFile->isWritable())
				_uploadFile->write(reply->readAll());
			else
				setError(true, "Object not writable for saving downloaded data.");
		}
	}

	if (_uploadFile != NULL && _uploadFile->isOpen())
		_uploadFile->close();

	_uploadFile = NULL;

	if (reply != NULL)
		reply->deleteLater();

	emit finished();
}

void OnlineDataConnection::setError(bool value, QString msg)
{
	_error = value;
	_errorMsg = msg;
}

