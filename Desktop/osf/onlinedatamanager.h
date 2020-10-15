#ifndef ONLINEDATAMANAGER_H
#define ONLINEDATAMANAGER_H

#include "osfnam.h"

#include "onlinedataconnection.h"
#include "onlinedatanode.h"
#include "onlineusernode.h"

#include <map>

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

	void		setAuthentication(OnlineDataManager::Provider provider, QString username, QString password);
	void		initAuthentication(OnlineDataManager::Provider provider);
	void		clearAuthenticationOnExit(OnlineDataManager::Provider provider);
	AuthData	getAuthData(OnlineDataManager::Provider provider);	
	void		savePasswordFromAuthData(OnlineDataManager::Provider provider);
	void		savePassword(OnlineDataManager::Provider provider, QString password);
	void		removePassword(OnlineDataManager::Provider provider);
	QString		getPassword(OnlineDataManager::Provider provider);

	void		saveUsername(OnlineDataManager::Provider provider, QString username);
	QString		getUsername(OnlineDataManager::Provider provider);

	QNetworkAccessManager*	getNetworkAccessManager(OnlineDataManager::Provider provider) const;
	void					setNetworkAccessManager(OnlineDataManager::Provider provider, QNetworkAccessManager*);
	void					clearAuthentication(OnlineDataManager::Provider provider);

	OnlineDataNode* uploadFileAsync(		QString nodePath, QString id, OnlineDataNode::ActionFilter *filter = NULL);
	OnlineDataNode* downloadFileAsync(		QString nodePath, QString id, OnlineDataNode::ActionFilter *filter = NULL);
	OnlineDataNode* createNewFileAsync(		QString nodePath, QString filename, QString id);
	OnlineDataNode* createNewFolderAsync(	QString nodePath, QString name, QString id);

	OnlineUserNode* getOnlineUserData(QString nodePath, QString id);

	static OnlineDataManager::Provider determineProvider(QString nodePath);

	QString getLocalPath(QString nodePath) const;

	OnlineDataNode* getActionDataNode(QString id);
	void deleteActionDataNode(QString id);

public slots:
	void beginUploadFile(QString nodePath, QString actionId, QString oldFileId, QString oldMD5);
	void beginDownloadFile(QString nodePath, QString actionId);

signals:
	void newFolderFinished(QString id);
	void newFileFinished(QString id);
	void downloadFileFinished(QString id);
	void uploadFileFinished(QString id);
	void progress(const QString &status, int progress);
	void startUploading();
	void finishedUploading();

private slots:
	void newFolderFinished();
	void newFileFinished();
	void uploadFileFinished();
	void downloadFileFinished();

private:

	std::map<OnlineDataManager::Provider, QNetworkAccessManager *> _providers;
	QMap<OnlineDataManager::Provider, AuthData> _authList;
	QMap<QString, OnlineDataNode*> _actionNodes;

	OnlineDataNode *getOnlineNodeData(QString nodePath, QString id);

	static bool md5UploadFilter(OnlineDataNode *dataNode, OnlineDataNode::ActionFilter *filter);
};

#endif // ONLINEDATAMANAGER_H
