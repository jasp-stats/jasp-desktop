#ifndef ONLINEDATACONNECTION_H
#define ONLINEDATACONNECTION_H

#include <QUrl>
#include <QNetworkAccessManager>
#include <QFile>

class OnlineDataConnection: public QObject
{
	Q_OBJECT

public:

	OnlineDataConnection(QString id, QNetworkAccessManager *manager, QObject *parent = 0);

	void beginUploadFile(QUrl url, QString sourcePath);
	bool retryUploadFile(QUrl url);

	void beginDownloadFile(QUrl url, QString destination);

	bool error() const;
	QString errorMessage() const;

	QString id() const;

	QNetworkAccessManager* manager() const;

private slots:
	void uploadFinished();
	void downloadFinished();

signals:
	void finished();

private:

	//OnlineDataNode::ConnectionType _uploadConnectionType;

	void setError(QString msg);

	bool _error = false;
	QString _errorMsg = "";

	QFile *_uploadFile;

	QString _downloadDest;

	QString _id;

	QNetworkAccessManager* _manager;
};

#endif // ONLINEDATACONNECTION_H
