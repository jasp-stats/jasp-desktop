//
// Copyright (C) 2013-2018 University of Amsterdam
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

	if ((type == OnlineDataConnection::Put || type == OnlineDataConnection::Post) && data != nullptr)
	{
		if (_uploadFile != nullptr && _uploadFile->isOpen() == false && _uploadFile->open(QIODevice::ReadOnly) == false)
			setError(true, "File cannot be opened for online data action.");
		else if (_uploadFile != nullptr && _uploadFile->isOpen() == true && _uploadFile->isReadable() == false)
			setError(true, "File is not readable for online data action.");
		else if (_uploadFile != nullptr && _uploadFile->pos() != 0 && _uploadFile->reset() == false)
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
		actionFinished(nullptr);
}

void OnlineDataConnection::beginAction(QUrl url, OnlineDataConnection::Type type, const QByteArray &data)
{
	setError(false, "");

	_uploadFile = nullptr;
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
		actionFinished(nullptr);
}

void OnlineDataConnection::actionFinished()
{
	QNetworkReply *reply = qobject_cast<QNetworkReply *>(this->sender());

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
	if (_error == false && reply != nullptr)
	{
		if (_actionType == OnlineDataConnection::Get && _uploadFile != nullptr)
		{
			if ((_uploadFile->isOpen() || _uploadFile->open(QIODevice::WriteOnly)) && _uploadFile->isWritable())
				_uploadFile->write(reply->readAll());
			else
				setError(true, "Object not writable for saving downloaded data.");
		}
	}

	if (_uploadFile != nullptr && _uploadFile->isOpen())
		_uploadFile->close();

	_uploadFile = nullptr;

	if (reply != nullptr)
		reply->deleteLater();

	emit finished();
}

void OnlineDataConnection::setError(bool value, QString msg)
{
	_error = value;
	_errorMsg = msg;
}
