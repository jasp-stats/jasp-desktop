#include "onlinedatamanager.h"


#include <QCryptographicHash>
#include "tempfiles.h"
#include "qutils.h"
#include "onlinedataconnection.h"
#include "onlinedatanodeosf.h"

OnlineDataManager::OnlineDataManager(QObject *parent):
	QObject(parent)
{
	setNetworkAccessManager(OnlineDataManager::OSF, new OSFNAM(this));
}

OnlineDataManager::~OnlineDataManager()
{
}


void OnlineDataManager::setAuthentication(OnlineDataManager::Provider provider, QString username, QString password)
{
	OnlineDataManager::AuthData authData;

	authData.username = username;
	authData.password = password;

	_authList[provider] = authData;

	OSFNAM* manager = qobject_cast<OSFNAM*>(getNetworkAccessManager(provider));
	if (manager != NULL)
		manager->osfAuthentication(authData.username, authData.password);
}

void OnlineDataManager::setNetworkAccessManager(OnlineDataManager::Provider provider, QNetworkAccessManager* manager) {

	_providers[provider] = manager;
}

QNetworkAccessManager* OnlineDataManager::getNetworkAccessManager(OnlineDataManager::Provider provider)
{
	return _providers[provider];
}


void OnlineDataManager::beginUploadFile(QString nodePath, QString id)
{
	uploadFileAsync(nodePath, id);
}

OnlineDataConnection *OnlineDataManager::uploadFileAsync(QString nodePath, QString id) {

	OnlineDataNode *nodeData = getOnlineNodeData(nodePath, id);

	if (nodeData != NULL)
	{
		connect(nodeData, SIGNAL(finished()), this, SLOT(uploadDataReceived()));

		nodeData->getNodeInfo();

		return nodeData->connection();
	}

	return NULL;
}

void OnlineDataManager::uploadDataReceived()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	if (dataNode->error())
		emit error(dataNode->errorMessage(), dataNode->id());
	else
	{
		OnlineDataConnection *connection = dataNode->connection();

		connect(connection, SIGNAL(finished()), this, SLOT(uploadFileFinished()));

		connection->beginUploadFile(QUrl(dataNode->getUploadPath()), getLocalPath(dataNode->path()));
	}

	dataNode->deleteLater();
}

void OnlineDataManager::uploadFileFinished()
{
	OnlineDataConnection *connection = qobject_cast<OnlineDataConnection*>(sender());
	if (connection->error())
		emit error(connection->errorMessage(), connection->id());
	else
		emit uploadFileFinished(connection->id());

	connection->deleteLater();
}


void OnlineDataManager::beginDownloadFile(QString nodePath, QString id) {

	downloadFileAsync(nodePath, id);
}

OnlineDataConnection *OnlineDataManager::downloadFileAsync(QString nodePath, QString id)
{
	OnlineDataNode *nodeData = getOnlineNodeData(nodePath, id);

	if (nodeData != NULL)
	{
		connect(nodeData, SIGNAL(finished()), this, SLOT(downloadDataReceived()));
		nodeData->getNodeInfo();

		return nodeData->connection();
	}

	return NULL;
}

void OnlineDataManager::downloadDataReceived()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	if (dataNode->error())
		emit error(dataNode->errorMessage(), dataNode->id());
	else
	{
		OnlineDataConnection *connection = dataNode->connection();

		connect(connection, SIGNAL(finished()), this, SLOT(downloadFileFinished()));

		connection->beginDownloadFile(QUrl(dataNode->getDownloadPath()), getLocalPath(dataNode->path()));
	}

	dataNode->deleteLater();
}

void OnlineDataManager::downloadFileFinished()
{
	OnlineDataConnection *connection = qobject_cast<OnlineDataConnection*>(sender());

	if (connection->error())
		emit error(connection->errorMessage(), connection->id());
	else
		emit downloadFileFinished(connection->id());

	connection->deleteLater();
}


OnlineDataManager::Provider OnlineDataManager::determineProvider(QString nodePath) {

	if (nodePath.contains(".osf."))
		return OnlineDataManager::OSF;

	return OnlineDataManager::None;
}

OnlineDataNode *OnlineDataManager::getOnlineNodeData(QString nodePath, QString id)
{
	OnlineDataManager::Provider provider = determineProvider(nodePath);

	QNetworkAccessManager *manager = getNetworkAccessManager(provider);

	if (provider == OnlineDataManager::OSF) {

		OnlineDataNodeOSF *nodeData = new OnlineDataNodeOSF(manager, id, this);
		nodeData->setPath(nodePath);
		return nodeData;
	}

	return NULL;
}

QString OnlineDataManager::getLocalPath(QString nodePath) const {

	QString name = QString(QCryptographicHash::hash(nodePath.toLatin1(),QCryptographicHash::Md5).toHex());
	std::string tempFile = tempfiles_createSpecific("online", fq(name));
	return tq(tempFile);
}
