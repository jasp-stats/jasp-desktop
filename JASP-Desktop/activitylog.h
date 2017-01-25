//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef ACTIVITYLOG_H
#define ACTIVITYLOG_H

#include <QString>
#include <QVariant>
#include <QNetworkAccessManager>
#include <QFile>
#include <QLockFile>

class ActivityLog : public QObject
{
	Q_OBJECT

public:
	ActivityLog(QObject *parent = 0);

	void log(const QString &action, const QString &info = "");

public slots:
	void flushLogToServer();

private slots:
	void networkResponse();

private:

	QString _uid;

	QFile _logFile;
	qint64 _logFilePos;

	QStringList _logWaiting;
	QLockFile _lockFile;

	QNetworkAccessManager _network;
	QNetworkReply *_reply;

};

#endif // ACTIVITYLOG_H
