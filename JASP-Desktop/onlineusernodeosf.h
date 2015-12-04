#ifndef ONLINEUSERNODEOSF_H
#define ONLINEUSERNODEOSF_H

#include "onlineusernode.h"
#include "common.h"

class OnlineUserNodeOSF: public OnlineUserNode
{
	Q_OBJECT

public:
	OnlineUserNodeOSF(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual void getNodeInfo() const OVERRIDE;

private slots:
	void nodeInfoReceived();

};

#endif // ONLINEUSERNODEOSF_H
