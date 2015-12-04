#ifndef ONLINEDATANODE_H
#define ONLINEDATANODE_H

#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include "onlinenode.h"

class OnlineDataNode: public OnlineNode
{
	Q_OBJECT

public:

	enum ConnectionType {Put, Post, Get};
	enum Kind {File, Folder};

	OnlineDataNode(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual QString getUploadPath() const;
	virtual QString getUploadPath(QString filename) const = 0;
	virtual QString getDownloadPath() const;
	virtual QString getNewFolderPath(QString folderName) const = 0;
	virtual QString getDeletePath() const;
	QString name() const;
	bool kind() const;

	OnlineDataNode::ConnectionType uploadConnectionType() const;
	OnlineDataNode::ConnectionType downloadConnectionType() const;

protected:
	QString _uploadPath;
	QString _downloadPath;
	QString _newFolderPath;
	QString _deletePath;
	QString _name;
	OnlineDataNode::Kind _kind;

	OnlineDataNode::ConnectionType _uploadConnectionType = OnlineDataNode::Post;
	OnlineDataNode::ConnectionType _downloadConnectionType = OnlineDataNode::Get;

};

#endif // ONLINEDATANODE_H
