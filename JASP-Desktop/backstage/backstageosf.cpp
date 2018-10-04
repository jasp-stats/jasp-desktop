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

#include "backstageosf.h"
#include "ui_backstageform.h"
#include <QRegularExpression>
#include <QInputDialog>
#include <QMessageBox>
#include <QQmlEngine>
#include <QFileInfo>

#include "settings.h"
#include "qutils.h"

BackstageOSF::BackstageOSF(QWidget *parent): BackstagePage(parent),
	ui(new Ui::BackstageForm)
{
	ui->setupUi(this);
				
	_osfBreadCrumbsListModel = new OSFBreadCrumbsListModel(this);
	_osfBreadCrumbsListModel->setSeparator(/* QDir::separator() */ QChar('/'));		
	
	_model = new FSBMOSF(parent , FSBMOSF::rootelementname);
	
	_osfListModel = new OSFListModel(this);		
	_osfListModel ->setFSBModel(_model);
	_osfListModel->setBreadCrumbsListModel(_osfBreadCrumbsListModel);
	
	ui->QmlContent->engine()->addImportPath("qrc:///components");
		
	ui->QmlContent->rootContext()->setContextProperty("osfListModel", _osfListModel);
	ui->QmlContent->rootContext()->setContextProperty("osfBreadCrumbsListModel",_osfBreadCrumbsListModel);
	ui->QmlContent->rootContext()->setContextProperty("breadcrumbsmodel",_osfBreadCrumbsListModel);
	ui->QmlContent->rootContext()->setContextProperty("backstageosf", this);
	
	ui->QmlContent->setSource(QUrl(QStringLiteral("qrc:/backstage/BackstageOSF.qml")));
		
	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(updateUserDetails()));
	connect(_model, SIGNAL(authenticationClear()), this, SLOT(updateUserDetails()));
	connect(_model, SIGNAL(entriesChanged()), this, SLOT(resetOSFListModel()));
	connect(_model, SIGNAL(stopProcessing()), this, SLOT(stopProcessing()));
	connect(_osfListModel, SIGNAL(startProcessing()), this, SLOT(startProcessing()));
	connect(_osfBreadCrumbsListModel, SIGNAL(crumbIndexChanged(const int &)), _osfListModel, SLOT(changePath(const int &)));
	connect(this, SIGNAL(openFileRequest(const QString &)), this, SLOT(notifyDataSetOpened(const QString &)));

	_fsBrowser = new FSBrowser(this);
	_fsBrowser->setViewType(FSBrowser::ListView);
	_fsBrowser->setFSModel(_model);
	_fsBrowser->hide();
	
	setShowfiledialog(false);
		
}

bool BackstageOSF::loggedin()
{
	return _mLoggedin;
}

bool BackstageOSF::rememberme()
{
	return _mRememberMe;
}

bool BackstageOSF::processing()
{
	return _mProcessing;
}

bool BackstageOSF::showfiledialog()
{
	return _mShowFileDialog;
}

QString BackstageOSF::savefilename()
{
	return _mSaveFileName;
}

QString BackstageOSF::username()
{
	return _mUserName;
}

QString BackstageOSF::password()
{
	return _mPassword;
}

void BackstageOSF::setLoggedin(const bool loggedin)
{
	_mLoggedin =  loggedin;
	emit loggedinChanged();

}

void BackstageOSF::setRememberme(const bool rememberme)
{	
	_mRememberMe =  rememberme;
	emit remembermeChanged();
	
	Settings::setValue(Settings::OSF_REMEMBER_ME, _mRememberMe);
	Settings::sync();
	
}

void BackstageOSF::setProcessing(const bool processing)
{
	_mProcessing = processing;
	emit processingChanged();
}

void BackstageOSF::setSavefilename(const QString &savefilename)
{
	_mSaveFileName = savefilename;
	emit savefilenameChanged();
}

void BackstageOSF::setShowfiledialog(const bool showdialog)
{
	_mShowFileDialog = showdialog;
	emit showfiledialogChanged();
}

void BackstageOSF::setUsername(const QString &username)
{

	_mUserName =  username;
	emit usernameChanged();
	
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
		Settings::setValue(Settings::OSF_USERNAME, _mUserName);
	else
		Settings::remove(Settings::OSF_USERNAME);
	
	Settings::sync();	
}

void BackstageOSF::setPassword(const QString &password)
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

void BackstageOSF::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_model->setOnlineDataManager(_odm);

	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(authenticatedHandler()));
}

void BackstageOSF::attemptToConnect()
{
	setProcessing(true);
	_model->attemptToConnect();
	setLoggedin(_model->isAuthenticated());	
}

void BackstageOSF::setCurrentFileName(QString currentFileName)
{
	_currentFileName = currentFileName;
	setSavefilename(currentFileName);
}

void BackstageOSF::setMode(FileEvent::FileMode mode)
{
	BackstagePage::setMode(mode);
	bool showfiledialog = (mode == FileEvent::FileExportResults || mode == FileEvent::FileExportData || mode == FileEvent::FileSave );
	setShowfiledialog(showfiledialog);
}

//private slots

void BackstageOSF::notifyDataSetSelected(QString path)
{
	//_fileNameTextBox->setText(QFileInfo(path).fileName());
	setSavefilename(QFileInfo(path).fileName());
}


void BackstageOSF::notifyDataSetOpened(QString path)
{
	FSBMOSF::OnlineNodeData nodeData = _model->getNodeData(path);
	openSaveFile(nodeData.nodePath, nodeData.name);
}

void BackstageOSF::saveClicked()
{
	FSBMOSF::OnlineNodeData currentNodeData = _model->currentNodeData();

	if (currentNodeData.canCreateFiles == false)
	{
		if (currentNodeData.level == 0)
			QMessageBox::warning(this, "Projects", "Files cannot be added to the projects list.\n\nTo add a new project please use the online OSF services.");
		else if (currentNodeData.level == 1)
			QMessageBox::warning(this, "Data Providers", "Files cannot be added to a projects data providers list.\n\nTo add a new data provider (eg. google drive) please use the online OSF services.");
		else
			QMessageBox::warning(this, currentNodeData.name, "Files cannot be added to '" + currentNodeData.name + "' for an unknown reason.");
		return;
	}

	///QString filename = _fileNameTextBox->text();
	QString filename = _mSaveFileName;

	if (checkEntryName(filename, "File", true) == false)
		return;

	QString path;

	if (_model->hasFileEntry(filename.toLower(), path))
		notifyDataSetOpened(path);
	else
		openSaveFile(currentNodeData.nodePath, filename);
}

void BackstageOSF::openSaveFile(const QString &nodePath, const QString &filename)
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
		QMessageBox::warning(this, "File Types", event->getLastError());
		event->setComplete(false, "Failed to open file from OSF");
		return;
	}

	emit dataSetIORequest(event);
}

void BackstageOSF::userDetailsReceived()
{
	OnlineUserNode *userNode = qobject_cast<OnlineUserNode*>(sender());

	userNode->deleteLater();
}

void BackstageOSF::openSaveCompleted(FileEvent* event)
{

	if (event->successful())
	{
		_model->refresh();
	}
	
	setProcessing(false);
}


void BackstageOSF::updateUserDetails()
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

void BackstageOSF::newFolderCreated()
{
	OnlineDataNode *node = qobject_cast<OnlineDataNode *>(sender());

	if (node->error())
		QMessageBox::warning(this, "", "An error occured and the folder could not be created.");
	else
		_model->refresh();
	
	setProcessing(false);
}

void BackstageOSF::newFolderClicked()
{
	FSBMOSF::OnlineNodeData currentNodeData = _model->currentNodeData();

	if (currentNodeData.canCreateFolders == false)
	{
		if (currentNodeData.level == 0)
			QMessageBox::warning(this, "Projects", "A new folder cannot be added to the projects list.\n\nTo add a new project please use the online OSF services.");
		else if (currentNodeData.level == 1)
			QMessageBox::warning(this, "Data Providers", "A new folder cannot be added to a projects data providers list.\n\nTo add a new data provider (eg. google drive) please use the online OSF services.");
		else
			QMessageBox::warning(this, currentNodeData.name, "A new folder cannot be added to '" + currentNodeData.name + "' for an unknown reason.");
		return;
	}

	bool ok;
	QString name = "New folder";


	do
	{
		name = QInputDialog::getText(this, "New folder", "New folder name", QLineEdit::Normal, name, &ok);
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

void BackstageOSF::closeFileDialog()
{
	setShowfiledialog(false);
}

void BackstageOSF::authenticatedHandler()
{
	//_newFolderButton->setEnabled(true);
	//_logoutButton->show();
}

void BackstageOSF::resetOSFListModel()
{
	_osfListModel->reload();
	setProcessing(false);
}

// public slots
void BackstageOSF::logoutClicked()
{
	_model->clearAuthentication();
	//_logoutButton->hide();	
	_model->refresh();
	_osfBreadCrumbsListModel->indexChanged(0);
	
	setLoggedin(false);
	setProcessing(false);
}

void BackstageOSF::remembermeCheckChanged(bool rememberme)
{
	_mRememberMe = rememberme;
	
	if (_mRememberMe)
		Settings::setValue(Settings::OSF_REMEMBER_ME, rememberme);
	else
		Settings::remove(Settings::OSF_REMEMBER_ME);
	
	setRememberme(rememberme);
}

void BackstageOSF::usernameTextChanged(const QString &username)
{
	_mUserName = username;
	
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
		Settings::setValue(Settings::OSF_USERNAME, _mUserName);
	else
		Settings::remove(Settings::OSF_USERNAME);
	
	setUsername(username);
}

void BackstageOSF::passwordTextChanged(const QString &password)
{
	_mPassword = password;
	
	if (Settings::value(Settings::OSF_REMEMBER_ME).toBool() == true)
		Settings::setValue(Settings::OSF_PASSWORD, encrypt(_mPassword));
	else
		Settings::remove(Settings::OSF_PASSWORD);
	
	setPassword(password);
	
}

void BackstageOSF::updateLoginScreen()
{
	setRememberme(Settings::value(Settings::OSF_REMEMBER_ME).toBool());  
	setUsername(Settings::value(Settings::OSF_USERNAME).toString());
	setPassword(decrypt(Settings::value(Settings::OSF_PASSWORD).toString()));
}

void BackstageOSF::loginRequested(const QString &username, const QString &password)
{
	if  (password == "" || username =="" )
	{
		QMessageBox::warning(this, "Login", " User or password cannot be empty. ");
		return;
	}
	
	setProcessing(true);
	_model->authenticate(username, password);
}

void BackstageOSF::openFile(const QString &path)
{
	emit openFileRequest(path);
}

void BackstageOSF::saveFile(const QString &name)
{
	_mSaveFileName = name;
	saveClicked();
	
}

void BackstageOSF::startProcessing()
{
	setProcessing(true);
}

void BackstageOSF::stopProcessing()
{
	setProcessing(false);
}

//private
bool BackstageOSF::checkEntryName(QString name, QString entryTitle, bool allowFullStop)
{
	if (name.trimmed() == "")
	{
		QMessageBox::warning(this, "", "Entry name cannot be empty.");
		return false;
	}
	else
	{
		QRegularExpression r("[^\\w\\s" + (QString)(allowFullStop ? "\\.-" : "-") + "]");
		if (r.match(name).hasMatch())
		{
			QMessageBox::warning(this, "", entryTitle + " name can only contain the following characters A-Z a-z 0-9 _ " + (allowFullStop ? ". -" : "-"));
			return false;
		}
	}

	return true;
}
