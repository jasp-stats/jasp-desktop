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
#include "gui/messageforwarder.h"

OSF::OSF(QObject *parent): FileMenuObject(parent)
{				
	
	setBreadCrumbs(new OSFBreadCrumbsListModel(this, QChar('/')));
	
	_model = new OSFFileSystem(parent , OSFFileSystem::rootelementname);
	
	setListModel(new OSFListModel(this, _model, _osfBreadCrumbsListModel));
	
	connect(_model,						&OSFFileSystem::authenticationSuccess,				this,			&OSF::updateUserDetails);
	connect(_model,						&OSFFileSystem::authenticationClear,					this,			&OSF::updateUserDetails);
	connect(_model,						&OSFFileSystem::entriesChanged,						this,			&OSF::resetOSFListModel);
	connect(_model,						&OSFFileSystem::stopProcessing,						this,			&OSF::stopProcessing);
	connect(_osfListModel,				&OSFListModel::startProcessing,					this,			&OSF::startProcessing);
	connect(_osfBreadCrumbsListModel,	&OSFBreadCrumbsListModel::crumbIndexChanged,	_osfListModel,	&OSFListModel::changePathCrumbIndex);
	connect(this,						&OSF::openFileRequest,							this,			&OSF::notifyDataSetOpened);

	/*_fsBrowser = new FSBrowser(this);
	_fsBrowser->setViewType(FSBrowser::ListView);
	_fsBrowser->setFSModel(_model);
	_fsBrowser->hide();*/
	
	setShowfiledialog(false);
		
}

bool OSF::loggedin()
{
	return _mLoggedin;
}

bool OSF::rememberme()
{
	return _mRememberMe;
}

bool OSF::processing()
{
	return _mProcessing;
}

bool OSF::showfiledialog()
{
	return _mShowFileDialog;
}

QString OSF::savefilename()
{
	return _mSaveFileName;
}

QString OSF::username()
{
	return _mUserName;
}

QString OSF::password()
{
	return _mPassword;
}

void OSF::setLoggedin(const bool loggedin)
{
	_mLoggedin =  loggedin;
	emit loggedinChanged();

}

void OSF::setRememberme(const bool rememberme)
{	
	_mRememberMe =  rememberme;
	emit remembermeChanged();
	
	Settings::setValue(Settings::OSF_REMEMBER_ME, _mRememberMe);
	Settings::sync();
	
}

void OSF::setProcessing(const bool processing)
{
	_mProcessing = processing;
	emit processingChanged();
}

void OSF::setSavefilename(const QString &savefilename)
{
	_mSaveFileName = savefilename;
	emit savefilenameChanged();
}

void OSF::setShowfiledialog(const bool showdialog)
{
	_mShowFileDialog = showdialog;
	emit showfiledialogChanged();
}

void OSF::setUsername(const QString &username)
{

	_mUserName =  username;
	emit usernameChanged();
	
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
		Settings::setValue(Settings::OSF_USERNAME, _mUserName);
	else
		Settings::remove(Settings::OSF_USERNAME);
	
	Settings::sync();	
}

void OSF::setPassword(const QString &password)
{		
	_mPassword =  password;
	emit passwordChanged();
		
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
	{
		//Save password encrypted in settings
		Settings::setValue(Settings::OSF_PASSWORD, encrypt(password));
		Settings::setValue(Settings::OSF_ENCRYPTION, SimpleCryptEncryption);
	}
	else
		Settings::remove(Settings::OSF_USERNAME);

	Settings::sync();	
	
}

void OSF::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_model->setOnlineDataManager(_odm);

	connect(_odm, SIGNAL(startUploading()), this, SLOT(startProcessing()));
	connect(_odm, SIGNAL(finishedUploading()), this, SLOT(stopProcessing()));
}

void OSF::attemptToConnect()
{
	setProcessing(true);
	_model->attemptToConnect();
	setLoggedin(_model->isAuthenticated());	
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

//private slots

void OSF::notifyDataSetSelected(QString path)
{
	setSavefilename(QFileInfo(path).fileName());
}


void OSF::notifyDataSetOpened(QString path)
{
	OSFFileSystem::OnlineNodeData nodeData = _model->getNodeData(path);
	openSaveFile(nodeData.nodePath, nodeData.name);
}

void OSF::saveClicked()
{
	OSFFileSystem::OnlineNodeData currentNodeData = _model->currentNodeData();

	if (currentNodeData.canCreateFiles == false)
	{
		if (currentNodeData.level == 0)			MessageForwarder::showWarning("Projects",			"Files cannot be added to the projects list.\n\nTo add a new project please use the online OSF services.");
		else if (currentNodeData.level == 1)	MessageForwarder::showWarning("Data Providers",		"Files cannot be added to a projects data providers list.\n\nTo add a new data provider (eg. google drive) please use the online OSF services.");
		else									MessageForwarder::showWarning(currentNodeData.name, "Files cannot be added to '" + currentNodeData.name + "' for an unknown reason.");
		return;
	}

	QString filename = _mSaveFileName;

	if (checkEntryName(filename, "File", true) == false)
		return;

	QString path;

	if (_model->hasFileEntry(filename.toLower(), path))
		notifyDataSetOpened(path);
	else
		openSaveFile(currentNodeData.nodePath, filename);
}

void OSF::openSaveFile(const QString &nodePath, const QString &filename)
{
	bool storedata = (_mode == FileEvent::FileSave || _mode == FileEvent::FileExportResults || _mode == FileEvent::FileExportData);

	FileEvent *event = new FileEvent(this, _mode);
	
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
		MessageForwarder::showWarning("File Types", event->getLastError());
		event->setComplete(false, "Failed to open file from OSF");
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

	if (event->successful())
	{
		_model->refresh();
	}
	
	setProcessing(false);
}


void OSF::updateUserDetails()
{
	if (_model->isAuthenticated())
	{
	
		OnlineUserNode *userNode = _odm->getOnlineUserData("https://staging2-api.osf.io/v2/users/me/", "fsbmosf");

		userNode->initialise();

		connect(userNode, SIGNAL(finished()), this, SLOT(userDetailsReceived()));
		
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

	if (node->error())	MessageForwarder::showWarning("", "An error occured and the folder could not be created.");
	else
		_model->refresh();
	
	setProcessing(false);
}

void OSF::newFolderClicked()
{
	OSFFileSystem::OnlineNodeData currentNodeData = _model->currentNodeData();

	if (currentNodeData.canCreateFolders == false)
	{
		if (currentNodeData.level == 0)			MessageForwarder::showWarning("Projects",			"A new folder cannot be added to the projects list.\n\nTo add a new project please use the online OSF services.");
		else if (currentNodeData.level == 1)	MessageForwarder::showWarning("Data Providers",		"A new folder cannot be added to a projects data providers list.\n\nTo add a new data provider (eg. google drive) please use the online OSF services.");
		else									MessageForwarder::showWarning(currentNodeData.name, "A new folder cannot be added to '" + currentNodeData.name + "' for an unknown reason.");

		return;
	}

	bool ok;
	QString name = "New folder";


	do
	{
		std::cerr << "Qname = QInputDialog::getText(this, \"New folder\", \"New folder name\", QLineEdit::Normal, name, &ok);" << std::endl;
		throw std::runtime_error("No Inputdialog available cause were in QML");
	}
	while(ok && checkEntryName(name, "Folder", false) == false);


	if (ok)
	{
		setProcessing(true);
		
		emit newFolderRequested(name);

		if (_model->hasFolderEntry(name.toLower()) == false)
		{
			OnlineDataNode *node = _odm->createNewFolderAsync(_model->currentNodeData().nodePath + "#folder://" + name, name, "createNewFolder");
			connect(node, SIGNAL(finished()), this, SLOT(newFolderCreated()));
		}
	}
}

void OSF::closeFileDialog()
{
	setShowfiledialog(false);
}

void OSF::resetOSFListModel()
{
	_osfListModel->reload();
	setProcessing(false);
}

// public slots
void OSF::logoutClicked()
{
	_model->clearAuthentication();
	//_logoutButton->hide();	
	_model->refresh();
	_osfBreadCrumbsListModel->indexChanged(0);
	
	setLoggedin(false);
	setProcessing(false);
}

void OSF::remembermeCheckChanged(bool rememberme)
{
	_mRememberMe = rememberme;
	
	if (_mRememberMe)
		Settings::setValue(Settings::OSF_REMEMBER_ME, rememberme);
	else
		Settings::remove(Settings::OSF_REMEMBER_ME);
	
	setRememberme(rememberme);
}

void OSF::usernameTextChanged(const QString &username)
{
	_mUserName = username;
	
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
		Settings::setValue(Settings::OSF_USERNAME, _mUserName);
	else
		Settings::remove(Settings::OSF_USERNAME);
	
	setUsername(username);
}

void OSF::passwordTextChanged(const QString &password)
{
	_mPassword = password;
	
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
		Settings::setValue(Settings::OSF_PASSWORD, encrypt(_mPassword));
	else
		Settings::remove(Settings::OSF_PASSWORD);
	
	setPassword(password);
	
}

void OSF::updateLoginScreen()
{
	setRememberme(Settings::value(Settings::OSF_REMEMBER_ME).toBool());  
	setUsername(Settings::value(Settings::OSF_USERNAME).toString());
	setPassword(decrypt(Settings::value(Settings::OSF_PASSWORD).toString()));
}

void OSF::loginRequested(const QString &username, const QString &password)
{
	if  (password == "" || username =="" )
	{
		MessageForwarder::showWarning("Login", "User or password cannot be empty.");
		return;
	}
	
	setProcessing(true);
	_model->authenticate(username, password);
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
		MessageForwarder::showWarning("Entry name cannot be empty.");
		return false;
	}
	else
	{
		QRegularExpression r("[^\\w\\s" + (QString)(allowFullStop ? "\\.-" : "-") + "]");
		if (r.match(name).hasMatch())
		{
			MessageForwarder::showWarning(entryTitle + " name can only contain the following characters A-Z a-z 0-9 _ " + (allowFullStop ? ". -" : "-"));
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
