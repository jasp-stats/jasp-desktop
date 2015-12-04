#ifndef ONLINEUSERNODE_H
#define ONLINEUSERNODE_H


#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include "onlinenode.h"

class OnlineUserNode: public OnlineNode
{
	Q_OBJECT

public:
	OnlineUserNode(QNetworkAccessManager *manager, QString id, QObject *parent = 0);


	virtual QString getFullname() const;

protected:

	QString _fullname;

};


#endif // ONLINEUSERNODE_H


