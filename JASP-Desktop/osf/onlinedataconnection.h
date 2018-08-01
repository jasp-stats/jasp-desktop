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
