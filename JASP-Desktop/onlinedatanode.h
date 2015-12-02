#ifndef ONLINEDATANODE_H
#define ONLINEDATANODE_H

#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include "onlinedataconnection.h"

class OnlineDataNode: public QObject
{
	Q_OBJECT

public:

	enum ConnectionType {Put, Post, Get};
	enum Kind {File, Folder};

	OnlineDataNode(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	QString path() const;
	virtual QString getUploadPath() const;
	virtual QString getUploadPath(QString filename) const = 0;
	virtual QString getDownloadPath() const;
	virtual QString getNewFolderPath(QString folderName) const = 0;
	virtual QString getDeletePath() const;
	QString name() const;
	bool kind() const;

	QString id() const;
	QNetworkAccessManager* manager() const;

	OnlineDataNode::ConnectionType uploadConnectionType() const;
	OnlineDataNode::ConnectionType downloadConnectionType() const;

	void setPath(const QString &path);

	virtual void getNodeInfo() const = 0;

	bool error() const;
	QString errorMessage() const;

	OnlineDataConnection* connection();

signals:
	void finished();

protected:
	QString _path;
	QString _uploadPath;
	QString _downloadPath;
	QString _newFolderPath;
	QString _deletePath;
	QString _name;
	OnlineDataNode::Kind _kind;

	bool _error = false;
	QString _errorMsg = "";

	QNetworkAccessManager *_manager;
	QString _id;

	OnlineDataNode::ConnectionType _uploadConnectionType = OnlineDataNode::Post;
	OnlineDataNode::ConnectionType _downloadConnectionType = OnlineDataNode::Get;

	OnlineDataConnection* _connection = NULL;

};

#endif // ONLINEDATANODE_H
