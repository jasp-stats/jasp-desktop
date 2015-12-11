#include "onlinedatamanager.h"


#include <QCryptographicHash>
#include "tempfiles.h"
#include "qutils.h"
#include "onlinedataconnection.h"
#include "onlinedatanodeosf.h"
#include "onlineusernodeosf.h"


OnlineDataManager::OnlineDataManager(QObject *parent):
	QObject(parent)
{
	setNetworkAccessManager(OnlineDataManager::OSF, new OSFNAM(this));
}

OnlineDataManager::~OnlineDataManager()
{
}

bool OnlineDataManager::authenticationSuccessful(OnlineDataManager::Provider provider) const
{
	if (_authList.contains(provider) == false)
		return false;

	if (provider == OnlineDataManager::OSF)
		return OnlineUserNodeOSF::authenticationSuccessful(getNetworkAccessManager(provider));

	return false;
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

QNetworkAccessManager* OnlineDataManager::getNetworkAccessManager(OnlineDataManager::Provider provider) const
{
	return _providers[provider];
}


void OnlineDataManager::beginUploadFile(QString nodePath, QString id)
{
	uploadFileAsync(nodePath, id);
}


OnlineDataNode *OnlineDataManager::createNewFolderAsync(QString nodePath, QString name, QString id) {

	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		connect(dataNode, SIGNAL(finished()), this, SLOT(newFolderFinished()));

		dataNode->processAction(OnlineDataNode::NewFolder, name);

		return dataNode;
	}

	return NULL;
}

void OnlineDataManager::newFolderFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	if (dataNode->error())
		emit error(dataNode->errorMessage(), dataNode->id());
	else
		emit newFolderFinished(dataNode->id());

	dataNode->deleteLater();
}


OnlineDataNode *OnlineDataManager::createNewFileAsync(QString nodePath, QString filename, QString id) {

	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		connect(dataNode, SIGNAL(finished()), this, SLOT(newFileFinished()));

		dataNode->processAction(OnlineDataNode::NewFile, filename);

		return dataNode;
	}

	return NULL;
}

void OnlineDataManager::newFileFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	if (dataNode->error())
		emit error(dataNode->errorMessage(), dataNode->id());
	else
		emit newFileFinished(dataNode->id());

	dataNode->deleteLater();
}


OnlineDataNode *OnlineDataManager::uploadFileAsync(QString nodePath, QString id) {

	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		connect(dataNode, SIGNAL(finished()), this, SLOT(uploadFileFinished()));

		dataNode->processAction(OnlineDataNode::Upload, "");

		return dataNode;
	}

	return NULL;
}

void OnlineDataManager::uploadFileFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());
	if (dataNode->error())
		emit error(dataNode->errorMessage(), dataNode->id());
	else
		emit uploadFileFinished(dataNode->id());

	dataNode->deleteLater();
}


void OnlineDataManager::beginDownloadFile(QString nodePath, QString id) {

	downloadFileAsync(nodePath, id);
}

OnlineDataNode *OnlineDataManager::downloadFileAsync(QString nodePath, QString id)
{
	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		connect(dataNode, SIGNAL(finished()), this, SLOT(downloadFileFinished()));

		dataNode->processAction(OnlineDataNode::Download, "");

		return dataNode;
	}

	return NULL;
}


void OnlineDataManager::downloadFileFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	if (dataNode->error())
		emit error(dataNode->errorMessage(), dataNode->id());
	else
		emit downloadFileFinished(dataNode->id());

	dataNode->deleteLater();
}


OnlineUserNode *OnlineDataManager::getOnlineUserData(QString nodePath, QString id)
{
	OnlineDataManager::Provider provider = determineProvider(nodePath);

	QNetworkAccessManager *manager = getNetworkAccessManager(provider);

	if (provider == OnlineDataManager::OSF) {

		OnlineUserNodeOSF *nodeData = new OnlineUserNodeOSF(manager, id, this);
		nodeData->setPath(nodePath);
		return nodeData;
	}

	return NULL;
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

		OnlineDataNodeOSF *nodeData = new OnlineDataNodeOSF(getLocalPath(nodePath), manager, id, this);
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
