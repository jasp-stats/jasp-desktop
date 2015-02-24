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
