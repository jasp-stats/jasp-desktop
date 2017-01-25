//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "onlinedatamanager.h"


#include <QCryptographicHash>
#include "tempfiles.h"
#include "qutils.h"
#include "onlinedataconnection.h"
#include "onlinedatanodeosf.h"
#include "onlineusernodeosf.h"
#include <QMessageBox>
#include "simplecrypt.h"
#include "simplecryptkey.h"

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

void OnlineDataManager::savePasswordFromAuthData(OnlineDataManager::Provider provider)
{
	if (provider == OnlineDataManager::OSF)
	{
		AuthData sec = getAuthData(provider);
		if (sec.password!="")
			savePassword(provider, sec.password);
	}
}

void OnlineDataManager::savePassword(OnlineDataManager::Provider provider, QString password)
{

	long long key = SIMPLECRYPTKEY;
	SimpleCrypt crypto(key); //some random number

	if (provider == OnlineDataManager::OSF)
	{
		_settings.setValue("OSFPassword", crypto.encryptToString(password));
		_settings.setValue("OSFEncryption", SimpleCryptEncryption);
		_settings.sync();
	}
}

void OnlineDataManager::saveUsername(OnlineDataManager::Provider provider, QString username)
{
	if (provider == OnlineDataManager::OSF)
	{
		_settings.setValue("OSFUsername", username);
		_settings.sync();
	}
}

QString OnlineDataManager::getUsername(OnlineDataManager::Provider provider)
{
	QString username = "";

	if (provider == OnlineDataManager::OSF)
		username = _settings.value("OSFUsername", "").toString();

	return username;

}

QString OnlineDataManager::getPassword(OnlineDataManager::Provider provider)
{
	QString password = "";

	long long key = SIMPLECRYPTKEY;
	SimpleCrypt crypto(key); //some random number

	if (provider == OnlineDataManager::OSF)
	{
		if (_settings.value("OSFEncryption", 0).toInt() == SimpleCryptEncryption)
			password = crypto.decryptToString(_settings.value("OSFPassword", "").toString());
		else
		{
			password = _settings.value("OSFPassword", "").toString();
			if (password!="") savePassword(OnlineDataManager::OSF, password);
		}
	}

	return password;
}

void OnlineDataManager::removePassword(OnlineDataManager::Provider provider)
{
	if (provider == OnlineDataManager::OSF)
	{
		_settings.remove("OSFPassword");
		_settings.sync();
	}
}

void OnlineDataManager::clearAuthenticationOnExit(OnlineDataManager::Provider provider)
{
	if (provider == OnlineDataManager::OSF)
	{
		//If User switch 'Remember me' is off remove OSF settings
		if (_settings.value("OSFRememberMe", false).toBool() == false)
			_settings.remove("OSFPassword");
		_settings.sync();
	}
}

void OnlineDataManager::initAuthentication(OnlineDataManager::Provider provider)
{

	QString username = getUsername(provider);
	QString password = getPassword(provider);

	setAuthentication(provider, username, password);
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

OnlineDataManager::AuthData OnlineDataManager::getAuthData(OnlineDataManager::Provider provider)
{
	return _authList[provider];
}

void OnlineDataManager::clearAuthentication(OnlineDataManager::Provider provider)
{
	setAuthentication(provider, "", "");

	removePassword(provider);

	emit authenticationCleared((int)provider);
}

void OnlineDataManager::setNetworkAccessManager(OnlineDataManager::Provider provider, QNetworkAccessManager* manager) {

	_providers[provider] = manager;
}

QNetworkAccessManager* OnlineDataManager::getNetworkAccessManager(OnlineDataManager::Provider provider) const
{
	return _providers.find(provider)->second;
}


OnlineDataNode *OnlineDataManager::createNewFolderAsync(QString nodePath, QString name, QString id) {

	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		//connect(dataNode, SIGNAL(finished()), this, SLOT(newFolderFinished()));
		dataNode->processAction(OnlineDataNode::NewFolder, name);
	}

	return dataNode;
}

void OnlineDataManager::newFolderFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	emit newFolderFinished(dataNode->id());
}


OnlineDataNode *OnlineDataManager::createNewFileAsync(QString nodePath, QString filename, QString id) {

	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		//connect(dataNode, SIGNAL(finished()), this, SLOT(newFileFinished()));
		dataNode->processAction(OnlineDataNode::NewFile, filename);
	}

	return dataNode;
}


void OnlineDataManager::newFileFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());

	emit newFileFinished(dataNode->id());
}

OnlineDataNode* OnlineDataManager::getActionDataNode(QString id)
{
	return _actionNodes[id];
}

void OnlineDataManager::deleteActionDataNode(QString id)
{
	OnlineDataNode *dataNode = _actionNodes[id];
	_actionNodes.remove(id);

	dataNode->deleteActionFilter();
	dataNode->deleteLater();
}

OnlineDataNode *OnlineDataManager::uploadFileAsync(QString nodePath, QString id, OnlineDataNode::ActionFilter *filter) {

	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		dataNode->setActionFilter(filter);
		dataNode->processAction(OnlineDataNode::Upload, "");
	}

	return dataNode;
}

bool OnlineDataManager::md5UploadFilter(OnlineDataNode *dataNode, OnlineDataNode::ActionFilter *filter)
{
	if (dataNode->exists() && dataNode->nodeId() == filter->arg1.toString() && dataNode->md5() != filter->arg2.toString())
	{
		int pressed = QMessageBox::warning(NULL, "File Changed", "The online copy of this file has changed since it was opened.\n\nWould you like to override the online file?", QMessageBox::Yes, QMessageBox::No);
		return pressed == QMessageBox::Yes;
	}
	return true;
}

void OnlineDataManager::beginUploadFile(QString nodePath, QString actionId, QString oldFileId, QString oldMD5)
{
	OnlineDataNode::ActionFilter *filter = new OnlineDataNode::ActionFilter(OnlineDataManager::md5UploadFilter, oldFileId, oldMD5);
	OnlineDataNode *dataNode = uploadFileAsync(nodePath, actionId, filter);
	_actionNodes[actionId] = dataNode;
	connect(dataNode, SIGNAL(finished()), this, SLOT(uploadFileFinished()));
}

void OnlineDataManager::uploadFileFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());
	emit uploadFileFinished(dataNode->id());
}


void OnlineDataManager::beginDownloadFile(QString nodePath, QString actionId) {

	OnlineDataNode *dataNode = downloadFileAsync(nodePath, actionId);
	_actionNodes[actionId] = dataNode;
	connect(dataNode, SIGNAL(finished()), this, SLOT(downloadFileFinished()));
}

OnlineDataNode *OnlineDataManager::downloadFileAsync(QString nodePath, QString id, OnlineDataNode::ActionFilter *filter)
{
	OnlineDataNode *dataNode = getOnlineNodeData(nodePath, id);

	if (dataNode != NULL)
	{
		dataNode->setActionFilter(filter);
		dataNode->processAction(OnlineDataNode::Download, "");
	}

	return dataNode;
}


void OnlineDataManager::downloadFileFinished()
{
	OnlineDataNode *dataNode = qobject_cast<OnlineDataNode *>(sender());
	emit downloadFileFinished(dataNode->id());
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
