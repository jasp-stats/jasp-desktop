#ifndef ONLINEUSERNODEOSF_H
#define ONLINEUSERNODEOSF_H

#include "onlineusernode.h"
#include "common.h"

///
/// Representing a user specifically for/on OSF
class OnlineUserNodeOSF: public OnlineUserNode
{
	Q_OBJECT

public:
	OnlineUserNodeOSF(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual void initialise() OVERRIDE;
	void login() ;

signals:
	void authenticationResult(bool);

private slots:
	void nodeInfoReceived();
	void handleLogin();

};

#endif // ONLINEUSERNODEOSF_H
