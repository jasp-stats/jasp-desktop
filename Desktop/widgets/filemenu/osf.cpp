//
// Copyright (C) 2018 University of Amsterdam
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

#include "osf.h"
#include <QRegularExpression>
#include <QQmlEngine>
#include <QFileInfo>


#include "utilities/settings.h"
#include "utilities/qutils.h"
#include "utilities/messageforwarder.h"
#include "osf/onlineusernodeosf.h"
#include "filemenu.h"

OSF::OSF(FileMenu *parent): FileMenuObject(parent)
{

	setBreadCrumbs(new OSFBreadCrumbsListModel(this, QChar('/')));

	_osfFileSystem = new OSFFileSystem(parent , OSFFileSystem::rootelementname);

	setListModel(new OSFListModel(this, _osfFileSystem, _osfBreadCrumbsListModel));

	connect(_osfFileSystem,				&OSFFileSystem::authenticationSucceeded,		this,			&OSF::updateUserDetails);
	connect(_osfFileSystem,				&OSFFileSystem::authenticationFailed,			this,			&OSF::authenticationeFailed);
	connect(_osfFileSystem,				&OSFFileSystem::authenticationClear,			this,			&OSF::updateUserDetails);
	connect(_osfFileSystem,				&OSFFileSystem::entriesChanged,					this,			&OSF::resetOSFListModel);
	connect(_osfFileSystem,				&OSFFileSystem::stopProcessing,					this,			&OSF::stopProcessing);
	connect(_osfListModel,				&OSFListModel::startProcessing,					this,			&OSF::startProcessing);
	connect(_osfFileSystem,				&OSFFileSystem::newLoginRequired,				this,			&OSF::newLoginRequired);

	connect(this,						&OSF::openFileRequest,							this,			&OSF::notifyDataSetOpened);
	connect(_osfBreadCrumbsListModel,	&OSFBreadCrumbsListModel::crumbIndexChanged,	_osfListModel,	&OSFListModel::changePathCrumbIndex);


	_mRememberMe	= Settings::value(Settings::OSF_REMEMBER_ME).toBool();
	_mUserName		= Settings::value(Settings::OSF_USERNAME).toString();
	_mPassword		= decrypt(Settings::value(Settings::OSF_PASSWORD).toString());

	setShowfiledialog(false);
	setProcessing(false);

	_sortedMenuModel = new SortMenuModel(_osfFileSystem, {Sortable::None, Sortable::SortByNameAZ, Sortable::SortByNameZA, Sortable::SortByDate});
	_sortedMenuModel->setCurrentEntry(static_cast<Sortable::SortType>(Settings::value(Settings::OSF_SORTORDER).toInt()));
}

void OSF::setLoggedin(const bool loggedin)
{
	if(_mLoggedin == loggedin)
		return;

	_mLoggedin =  loggedin;
	emit loggedinChanged();

}

void OSF::setProcessing(const bool processing)
{
	if(_mProcessing == processing)
		return;

	_mProcessing = processing;
	emit processingChanged();
}

void OSF::setSavefilename(const QString &savefilename)
{
	if(_mSaveFileName == savefilename)
		return;

	_mSaveFileName = savefilename;
	emit savefilenameChanged();
}

void OSF::setSavefoldername(const QString &savefoldername)
{
	if(_mSaveFolderName == savefoldername)
		return;

	_mSaveFolderName = savefoldername;
	emit savefoldernameChanged();
}

void OSF::setShowfiledialog(const bool showdialog)
{
	if(_mShowFileDialog == showdialog)
		return;

	_mShowFileDialog = showdialog;
	emit showfiledialogChanged();
}

void OSF::setRememberme(const bool rememberme)
{
	if (_mRememberMe ==  rememberme)
		return;

	_mRememberMe = rememberme;

	emit remembermeChanged();

	Settings::setValue(Settings::OSF_REMEMBER_ME, _mRememberMe);

	if (!_mRememberMe)
	{
		Settings::remove(Settings::OSF_USERNAME);
		Settings::remove(Settings::OSF_PASSWORD);
	}
	else
	{
		Settings::setValue(Settings::OSF_USERNAME, _mUserName);
		//Save password encrypted in settings
		Settings::setValue(Settings::OSF_PASSWORD, encrypt(_mPassword));
		Settings::setValue(Settings::OSF_ENCRYPTION, SimpleCryptEncryption);
	}

	Settings::sync();

}

void OSF::setUsername(const QString &username)
{
	if (_mUserName == username)
		return;

	_mUserName = username;
	emit usernameChanged();

	if (_mRememberMe)
		Settings::setValue(Settings::OSF_USERNAME, _mUserName);
	else
		Settings::remove(Settings::OSF_USERNAME);

	Settings::sync();
}

void OSF::setPassword(const QString &password)
{
	if (_mPassword == password)
		return;

	_mPassword = password;
	emit passwordChanged();

	if (_mRememberMe)
	{
		//Save password encrypted in settings
		Settings::setValue(Settings::OSF_PASSWORD, encrypt(password));
		Settings::setValue(Settings::OSF_ENCRYPTION, SimpleCryptEncryption);
	}
	else
		Settings::remove(Settings::OSF_PASSWORD);

	Settings::sync();

}

void OSF::checkErrorMessageOSF(QNetworkReply *reply)
{
	QNetworkReply::NetworkError error = reply->error();

	if (error != QNetworkReply::NoError)
	{
		QString err = reply->errorString();

		switch(error)
		{
		case QNetworkReply::AuthenticationRequiredError:	err = tr("Username and/or password are not correct. Please try again.");		break;
		case QNetworkReply::HostNotFoundError:				err = tr("OSF service not available. Please check your internet connection.");	break;
		case QNetworkReply::TimeoutError:					err = tr("Connection Timeout error. Please check your internet connection.");	break;
		}

		MessageForwarder::showWarning(tr("OSF Error"), err);
	}
}

void OSF::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_osfFileSystem->setOnlineDataManager(_odm);

	connect(_odm, SIGNAL(startUploading()), this, SLOT(startProcessing()));
	connect(_odm, SIGNAL(finishedUploading()), this, SLOT(stopProcessing()));
}

void OSF::attemptToConnect()
{
	if (!processing())
	{
		setProcessing(true);
		_osfFileSystem->attemptToConnect();
		setLoggedin(_osfFileSystem->isAuthenticated());
	}
}

void OSF::setCurrentFileName(QString currentFileName)
{
	_currentFileName = currentFileName;
	setSavefilename(currentFileName);
}

void OSF::setMode(FileEvent::FileMode mode)
{
	FileMenuObject::setMode(mode);
	bool showfiledialog = (mode == FileEvent::FileExportResults || mode == FileEvent::FileGenerateData || mode == FileEvent::FileExportData || mode == FileEvent::FileSave );
	setShowfiledialog(showfiledialog);
}

SortMenuModel *OSF::sortedMenuModel() const
{
	return _sortedMenuModel;
}

//private slots

void OSF::notifyDataSetSelected(QString path)
{
	setSavefilename(QFileInfo(path).fileName());
}


void OSF::notifyDataSetOpened(QString path)
{
	OSFFileSystem::OnlineNodeData nodeData = _osfFileSystem->getNodeData(path);
	openSaveFile(nodeData.nodePath, nodeData.name, path);
}

void OSF::authenticationeFailed(QString message)
{
	//MessageForwarder::showWarning("Login", message);
}

void OSF::saveClicked()
{
	OSFFileSystem::OnlineNodeData currentNodeData = _osfFileSystem->currentNodeData();

	if (currentNodeData.canCreateFiles == false)
	{
		if (currentNodeData.level == 0)			MessageForwarder::showWarning(tr("Projects"),			tr("Files cannot be added to the projects list.\n\nTo add a new project please use the online OSF services."));
		else if (currentNodeData.level == 1)	MessageForwarder::showWarning(tr("Data Providers"),		tr("Files cannot be added to a projects data providers list.\n\nTo add a new data provider (eg. google drive) please use the online OSF services."));
		else									MessageForwarder::showWarning(currentNodeData.name,		tr("Files cannot be added to '%1' for an unknown reason.").arg(currentNodeData.name));
		return;
	}

	QString filename = _mSaveFileName;

	if (checkEntryName(filename, "File", true) == false)
		return;

	QString path;

	if (_osfFileSystem->hasFileEntry(filename.toLower(), path))
		notifyDataSetOpened(path);
	else
		openSaveFile(currentNodeData.nodePath, filename, _osfFileSystem->path());
}

void OSF::openSaveFile(const QString & nodePath, const QString & filename, const QString & osfPath)
{
	bool storedata = (mode() == FileEvent::FileSave || mode() == FileEvent::FileExportResults || mode() == FileEvent::FileExportData);

	FileEvent *event = new FileEvent(this, mode());

	event->setOsfPath(osfPath);

	setProcessing(true);

	if (event->setPath(nodePath + "#file://" + filename))
	{
		if (storedata)
		{
			setSavefilename(filename);

			connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(openSaveCompleted(FileEvent*)));
		}
	}
	else
	{
		MessageForwarder::showWarning(tr("File Types"), event->getLastError());
		event->setComplete(false, tr("Failed to open file from OSF"));
		return;
	}

	emit dataSetIORequest(event);
}

void OSF::userDetailsReceived()
{
	OnlineUserNode *userNode = qobject_cast<OnlineUserNode*>(sender());

	userNode->deleteLater();
}

void OSF::openSaveCompleted(FileEvent* event)
{

	if (event->isSuccessful())
	{
		_osfFileSystem->refresh();
	}

	setProcessing(false);
}


void OSF::updateUserDetails()
{
	if (_osfFileSystem->isAuthenticated())
	{
		OnlineUserNode *userNode = _odm->getOnlineUserData("https://staging2-api.osf.io/v2/users/me/", "fsbmosf");

		userNode->initialise();

		connect(userNode, &OnlineDataNode::finished , this, &OSF::userDetailsReceived);

		setLoggedin(true);
	}
	else
	{
		setLoggedin(false);
	}

}

void OSF::newFolderCreated()
{
	OnlineDataNode *node = qobject_cast<OnlineDataNode *>(sender());

	if (node->error())
		MessageForwarder::showWarning("", tr("An error occured and the folder could not be created."));
	else
		_osfFileSystem->refresh();

	setSavefoldername(QString());
	setProcessing(false);
}

void OSF::newFolderClicked()
{
	OSFFileSystem::OnlineNodeData currentNodeData = _osfFileSystem->currentNodeData();

	if (currentNodeData.canCreateFolders == false)
	{
		if (currentNodeData.level == 0)			MessageForwarder::showWarning(tr("Projects"),			tr("A new folder cannot be added to the projects list.<br>To add a new project please use the online OSF services."));
		else if (currentNodeData.level == 1)	MessageForwarder::showWarning(tr("Data Providers"),		tr("A new folder cannot be added to a projects data providers list.<br>To add a new data provider (eg. google drive) please use the online OSF services."));
		else									MessageForwarder::showWarning(currentNodeData.name,		tr("A new folder cannot be added to '%1' for an unknown reason.").arg(currentNodeData.name));

		return;
	}

	QString name = savefoldername();
	bool ok = checkEntryName(name, tr("Folder"), false);

	if (ok)
	{
		setProcessing(true);

		emit newFolderRequested(name);

		if (_osfFileSystem->hasFolderEntry(name.toLower()) == false)
		{
			OnlineDataNode *node = _odm->createNewFolderAsync(_osfFileSystem->currentNodeData().nodePath + "#folder://" + name, name, "createNewFolder");
			connect(node, &OnlineDataNode::finished , this, &OSF::newFolderCreated);
		}
	}
}

void OSF::closeFileDialog()
{
	setShowfiledialog(false);
}

void OSF::newLoginRequired()
{
	_osfFileSystem->clearAuthentication();
	_osfFileSystem->refresh();
	_osfBreadCrumbsListModel->indexChanged(0);
	setLoggedin(false);
	setProcessing(false);
}

void OSF::handleAuthenticationResult(bool success)
{
	_osfFileSystem->updateAuthentication(success);

	if(!success)
	{
		_odm->removePassword(OnlineDataManager::OSF);
		Settings::sync();
	}

	setProcessing(false);
}

void OSF::resetOSFListModel()
{
	_osfListModel->reload();
	setProcessing(false);
}

// public slots
void OSF::logoutClicked()
{
	_osfFileSystem->clearAuthentication();
	_osfFileSystem->refresh();
	_osfBreadCrumbsListModel->indexChanged(0);

	setLoggedin(false);
	setProcessing(false);
}

void OSF::loginRequested(const QString &username, const QString &password)
{

	if  (password == "" || username =="" )
	{
		MessageForwarder::showWarning(tr("Login"), tr("User or password cannot be empty."));
		return;
	}

	setProcessing(true);

	_odm->saveUsername(OnlineDataManager::OSF, username);
	_odm->savePassword(OnlineDataManager::OSF, password);
	_odm->setAuthentication(OnlineDataManager::OSF, username, password);

	OnlineUserNodeOSF *userNode = (OnlineUserNodeOSF *)_odm->getOnlineUserData("https://staging2-api.osf.io/v2/users/me/", "fsbmosf");
	userNode->login();

	connect(userNode, &OnlineUserNodeOSF::authenticationResult, this, &OSF::handleAuthenticationResult);

}

void OSF::openFile(const QString &path)
{
	emit openFileRequest(path);
}

void OSF::saveFile(const QString &name)
{
	_mSaveFileName = name;
	saveClicked();

}

void OSF::saveFolder(const QString &name)
{
	_mSaveFolderName = name;
	newFolderClicked();
}

void OSF::startProcessing()
{
	setProcessing(true);
}

void OSF::stopProcessing()
{
	setProcessing(false);
}

//private
bool OSF::checkEntryName(QString name, QString entryTitle, bool allowFullStop)
{
	if (name.trimmed() == "")
	{
		MessageForwarder::showWarning(tr("Entry name cannot be empty."));
		return false;
	}
	else
	{
		QRegularExpression r("[^\\w\\s" + (QString)(allowFullStop ? "\\.-" : "-") + "]");
		if (r.match(name).hasMatch())
		{
			MessageForwarder::showWarning(tr("%1 name can only contain the following characters A-Z a-z 0-9 _ %2").arg(entryTitle).arg((allowFullStop ? ". -" : "-")));
			return false;
		}
	}

	return true;
}


void OSF::setListModel(OSFListModel * listModel)
{
	if (_osfListModel == listModel)
		return;

	_osfListModel = listModel;

	connect(_osfListModel, &OSFListModel::openFileRequest, this, &OSF::openFileRequest);

	emit listModelChanged(_osfListModel);
}

void OSF::setBreadCrumbs(OSFBreadCrumbsListModel * breadCrumbs)
{
	if (_osfBreadCrumbsListModel == breadCrumbs)
		return;

	_osfBreadCrumbsListModel = breadCrumbs;
	emit breadCrumbsChanged(_osfBreadCrumbsListModel);
}
