#ifndef ONLINEUSERNODE_H
#define ONLINEUSERNODE_H


#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include "onlinenode.h"
#include "common.h"

class OnlineUserNode: public OnlineNode
{
	Q_OBJECT

public:
	OnlineUserNode(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual QString getFullname() const;

	virtual QString getActionPath() const OVERRIDE;

	virtual bool beginAction() OVERRIDE;

	//virtual void signin() = 0;

protected:

	QString _fullname;

};


#endif // ONLINEUSERNODE_H


