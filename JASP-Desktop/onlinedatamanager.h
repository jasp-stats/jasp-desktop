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
	bool authenticationSuccessful(OnlineDataManager::Provider provider) const;
	QNetworkAccessManager* getNetworkAccessManager(OnlineDataManager::Provider provider) const;
	void setNetworkAccessManager(OnlineDataManager::Provider provider, QNetworkAccessManager*);

	OnlineDataNode* uploadFileAsync(QString nodePath, QString id);
	OnlineDataNode* downloadFileAsync(QString nodePath, QString id);
	OnlineDataNode* createNewFileAsync(QString nodePath, QString filename, QString id);
	OnlineDataNode* createNewFolderAsync(QString nodePath, QString name, QString id);

	OnlineUserNode* getOnlineUserData(QString nodePath, QString id);

	QString getLocalPath(QString nodePath) const;

public slots:
	void beginUploadFile(QString nodePath, QString id);
	void beginDownloadFile(QString nodePath, QString id);

signals:
	void newFolderFinished(QString id);
	void newFileFinished(QString id);
	void downloadFileFinished(QString id);
	void uploadFileFinished(QString id);
	void error(QString msg, QString id);

private slots:
	void newFolderFinished();
	void newFileFinished();
	void uploadFileFinished();
	void downloadFileFinished();

private:

	QMap<OnlineDataManager::Provider, QNetworkAccessManager *> _providers;
	QMap<OnlineDataManager::Provider, AuthData> _authList;

	OnlineDataNode *getOnlineNodeData(QString nodePath, QString id);
	OnlineDataManager::Provider determineProvider(QString nodePath);


};

#endif // ONLINEDATAMANAGER_H
