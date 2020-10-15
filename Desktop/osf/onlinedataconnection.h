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


#ifndef ONLINEDATACONNECTION_H
#define ONLINEDATACONNECTION_H

#include <QUrl>
#include <QNetworkAccessManager>
#include <QIODevice>
#include <QByteArray>

class OnlineDataConnection: public QObject
{
	Q_OBJECT

public:

	enum Type { Put, Post, Get };

	OnlineDataConnection(QNetworkAccessManager *manager, QObject *parent = 0);

	void beginAction(QUrl url, OnlineDataConnection::Type type, QIODevice *data);
	void beginAction(QUrl url, OnlineDataConnection::Type type, const QByteArray &data);

	bool error() const;
	QString errorMessage() const;

	QNetworkAccessManager* manager() const;

private slots:
	void actionFinished();

signals:
	void finished();
	void progress(const QString &status, int progress);

private:

	void actionFinished(QNetworkReply *reply);

	void setError(bool value, QString msg);

	bool _error = false;
	QString _errorMsg = "";

	QIODevice *_uploadFile = NULL;

	OnlineDataConnection::Type _actionType;

	QNetworkAccessManager* _manager = NULL;
};

#endif // ONLINEDATACONNECTION_H
