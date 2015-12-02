#ifndef ONLINENODE_H
#define ONLINENODE_H


#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include "onlinedataconnection.h"

class OnlineNode: public QObject
{
	Q_OBJECT

public:
	OnlineNode(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	QString path() const;

	QString id() const;
	QNetworkAccessManager* manager() const;

	void setPath(const QString &path);

	virtual void getNodeInfo() const = 0;

	bool error() const;
	QString errorMessage() const;

	OnlineDataConnection* connection();

signals:
	void finished();

protected:
	QString _path;

	bool _error = false;
	QString _errorMsg = "";

	QNetworkAccessManager *_manager;
	QString _id;

	OnlineDataConnection* _connection = NULL;
};

#endif // ONLINENODE_H
