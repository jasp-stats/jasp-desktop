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

#include "activitylog.h"

#include <QJsonDocument>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QTimer>
#include <QDir>
#include <QSettings>

#include "utils.h"
#include "processinfo.h"
#include "dirs.h"
#include "appinfo.h"
#include "qutils.h"

using namespace std;

ActivityLog::ActivityLog(QObject *parent)
	: QObject(parent), _lockFile(tq(Dirs::appDataDir()) + "/log.csv.lock")
{
	_reply = NULL;
	_logFile.setFileName(tq(Dirs::appDataDir()) + "/log.csv");

	QSettings settings;
	QVariant uid = settings.value("uid");

	if (uid.canConvert<QString>())
	{
		_uid = uid.toString();
	}
	else
	{
		_uid = QString("%1").arg(Utils::currentMillis());
		settings.setValue("uid", _uid);
		settings.sync();
	}
}

void ActivityLog::log(const QString &action, const QString &info)
{
	QString line("%1,%2,%3,%4,%5,%6\n");

	line = line.arg(tq(AppInfo::getShortDesc()));
	line = line.arg(_uid);
	line = line.arg(ProcessInfo::currentPID());
	line = line.arg(Utils::currentMillis());
	line = line.arg(action);
	line = line.arg(info);

	if (_lockFile.tryLock())
	{	
		if (_logFile.open(QFile::Append))
		{
			QTextStream s(&_logFile);

			foreach (const QString &waitingLine, _logWaiting)
				s << waitingLine;

			s << line;

			_logFile.close();
		}

		_lockFile.unlock();
	}
	else
	{
		_logWaiting.append(line);
	}
}

void ActivityLog::flushLogToServer()
{
	QFileInfo info(_logFile);

	if (info.size() == 0)
		return;

	const int CHUNK_SIZE = 65536;

	try
	{
		if (_lockFile.tryLock())
		{
			_logFile.open(QFile::ReadOnly);
			_logFilePos = 0;

			if (info.size() > CHUNK_SIZE)
			{
				// if the log file is > 64K, then it is sent in 64K chunks

				_logFilePos = info.size() - CHUNK_SIZE;
				_logFile.seek(_logFilePos);

				bool done = false;

				while (done == false)
				{
					// round the chunk down to the nearest new line character

					char buffer[1024];

					int read = _logFile.read(buffer, sizeof(buffer));

					if (read <= 0)
					{
						_logFile.close();
						_lockFile.unlock();
						return;
					}

					for (int i = 0; i < read; i++)
					{
						_logFilePos++;

						if (buffer[i] == '\n')
						{
							done = true;
							break;
						}
					}
				}

				_logFile.seek(_logFilePos);
			}

			QNetworkRequest request(QUrl("https://services.jasp-stats.org/logs/"));
			request.setHeader(QNetworkRequest::ContentTypeHeader, "text/csv");

			_reply = _network.post(request, &_logFile);
			connect(_reply, SIGNAL(finished()), this, SLOT(networkResponse()));
		}
	}
	catch (const std::exception &e)
	{
		_logFile.close();
		_lockFile.unlock();
		throw e;
	}
}

void ActivityLog::networkResponse()
{
	_logFile.close();

	if (_reply->error() == QNetworkReply::NoError)
	{
		_logFile.resize(_logFilePos);
		if (_logFilePos > 0)
			QTimer::singleShot(1000, this, SLOT(flushLogToServer()));
	}
	else if (_reply->error() == QNetworkReply::HostNotFoundError)
	{
		// do nothing, probably no internet connection
	}
	else
	{
		qDebug() << _reply->errorString();
		qDebug() << QString::fromUtf8(_reply->readAll());
	}

	_lockFile.unlock();
	_reply->deleteLater();
	_reply = NULL;
}

