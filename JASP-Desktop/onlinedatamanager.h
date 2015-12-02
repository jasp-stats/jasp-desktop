#ifndef ONLINEDATAMANAGER_H
#define ONLINEDATAMANAGER_H

#include "osfnam.h"

#include "onlinedataconnection.h"
#include "onlinedatanode.h"
#include "onlineusernode.h"

class OnlineDataManager : public QObject
{
	Q_OBJECT

public:

	enum Provider { None, OSF };

	OnlineDataManager(QObject *parent = 0);
	~OnlineDataManager();

	typedef struct {
		QString username;
		QString password;
	} AuthData;

	void setAuthentication(OnlineDataManager::Provider provider, QString username, QString password);
	QNetworkAccessManager* getNetworkAccessManager(OnlineDataManager::Provider provider);
	void setNetworkAccessManager(OnlineDataManager::Provider provider, QNetworkAccessManager*);

	OnlineDataConnection* uploadFileAsync(QString nodePath, QString id);
	OnlineDataConnection* downloadFileAsync(QString nodePath, QString id);

	OnlineUserNode* getOnlineUserData(QString nodePath, QString id);

	QString getLocalPath(QString nodePath) const;

public slots:
	void beginUploadFile(QString nodePath, QString id);
	void beginDownloadFile(QString nodePath, QString id);

signals:
	void downloadFileFinished(QString id);
	void uploadFileFinished(QString id);
	void error(QString msg, QString id);

private slots:
	void uploadFileFinished();
	void downloadFileFinished();
	void downloadDataReceived();
	void uploadDataReceived();

private:

	QMap<OnlineDataManager::Provider, QNetworkAccessManager *> _providers;
	QMap<OnlineDataManager::Provider, AuthData> _authList;

	OnlineDataNode *getOnlineNodeData(QString nodePath, QString id);
	OnlineDataManager::Provider determineProvider(QString nodePath);


};

#endif // ONLINEDATAMANAGER_H
