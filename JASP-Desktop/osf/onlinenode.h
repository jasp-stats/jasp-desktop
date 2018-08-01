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

	QString nodeId() const;

	void setPath(const QString &path);

	virtual void initialise() = 0;
	virtual bool beginAction() = 0;

	bool error() const;
	QString errorMessage() const;

	virtual QString getActionPath() const = 0;

	void setError(bool value, QString message);

protected slots:
	void connectionFinished();

signals:
	void finished();

protected:

	void startInit();
	void endInit(bool success);
	OnlineDataConnection* connection();

	QString _path;
	bool _inited = false;

	QNetworkAccessManager *_manager;
	QString _id;
	QString _nodeId;

	bool _reinitialise = false;


private:

	OnlineDataConnection* _connection = NULL;
	bool _error;
	QString _errorMsg;
};

#endif // ONLINENODE_H
