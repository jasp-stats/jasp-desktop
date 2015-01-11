
#include "activitylog.h"

#include <QJsonDocument>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QTimer>
#include <QDir>

#include "utils.h"
#include "appdirs.h"
#include "process.h"

using namespace std;

ActivityLog::ActivityLog(QObject *parent)
	: QObject(parent), _lockFile(AppDirs::tempDir() + "/log.csv.lock")
{
	_reply = NULL;
	_logFile.setFileName(AppDirs::tempDir() + "/log.csv");
}

void ActivityLog::log(const QString &action, const QVariant &info)
{
	QString line("%1,%2,%3,%4,%5\n");

	line = line.arg("JASP 0.6 Alpha");
	line = line.arg(Process::currentPID());
	line = line.arg(Utils::currentMillis());
	line = line.arg(action);

	QString json;
	if (info.canConvert<QString>())
	{
		json = info.toString();
	}
	else
	{
		QJsonDocument doc = QJsonDocument::fromVariant(info);
		json = QString::fromUtf8(doc.toJson(QJsonDocument::Compact));
	}

	if (json == "")
		json = "null";

	line = line.arg(json);

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

	_lockFile.unlock();
	_reply->deleteLater();
	_reply = NULL;
}

