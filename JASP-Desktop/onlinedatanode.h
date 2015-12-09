#ifndef ONLINEDATANODE_H
#define ONLINEDATANODE_H

#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include <QFile>
#include "onlinenode.h"
#include "common.h"

class OnlineDataNode: public OnlineNode
{
	Q_OBJECT

public:

	enum ConnectionType {Put, Post, Get};
	enum Kind {Unknown, File, Folder};
	enum Action {None, NewFile, NewFolder, Upload, Download};

	OnlineDataNode(QString localPath, QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual QString getUploadPath() const;
	virtual QString getUploadPath(QString filename) const = 0;
	virtual QString getDownloadPath() const;
	virtual QString getNewFolderPath(QString folderName) const = 0;
	virtual QString getDeletePath() const;

	virtual void beginDownloadFile() = 0;
	virtual void beginUploadFile() = 0;
	virtual void beginUploadFile(QString name) = 0;
	virtual void beginNewFolder(QString name) = 0;

	virtual bool beginAction() OVERRIDE;

	bool processAction(Action action, const QString &data);

	QString name() const;
	bool kind() const;

	OnlineDataNode::ConnectionType uploadConnectionType() const;
	OnlineDataNode::ConnectionType downloadConnectionType() const;

	void prepareAction(Action action, const QString &data);

	virtual QString getActionPath() const OVERRIDE;

	QString getLocalPath();

protected:
	QString _uploadPath;
	QString _downloadPath;
	QString _newFolderPath;
	QString _deletePath;
	QString _name;
	OnlineDataNode::Kind _kind;

	OnlineDataNode::ConnectionType _uploadConnectionType = OnlineDataNode::Post;
	OnlineDataNode::ConnectionType _downloadConnectionType = OnlineDataNode::Get;

	Action _preparedAction = OnlineDataNode::None;
	QString _preparedData;

	QString _localPath;
	QFile _localFile;

};

#endif // ONLINEDATANODE_H
