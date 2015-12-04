#include "onlineusernode.h"

OnlineUserNode::OnlineUserNode(QNetworkAccessManager *manager, QString id, QObject *parent):
	OnlineNode(manager, id, parent)
{
}

QString OnlineUserNode::getFullname() const
{
	return _fullname;
}
