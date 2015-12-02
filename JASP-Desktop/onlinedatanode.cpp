#include "onlinedatanode.h"

OnlineDataNode::OnlineDataNode(QNetworkAccessManager *manager, QString id, QObject *parent):
	OnlineNode(manager, id, parent)
{
}

QString OnlineDataNode::getUploadPath() const
{
	return _uploadPath;
}

QString OnlineDataNode::getDeletePath() const
{
	return _deletePath;
}

QString OnlineDataNode::getDownloadPath() const
{
	return _downloadPath;
}

QString OnlineDataNode::name() const
{
	return _name;
}

bool OnlineDataNode::kind() const
{
	return _kind;
}

OnlineDataNode::ConnectionType OnlineDataNode::downloadConnectionType() const {

	return _downloadConnectionType;
}

OnlineDataNode::ConnectionType OnlineDataNode::uploadConnectionType() const {

	return _uploadConnectionType;
}
