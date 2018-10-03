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

#ifndef BACKSTAGEOSF_H
#define BACKSTAGEOSF_H

#include "backstagepage.h"
#include "osflistmodel.h"
#include "osfbreadcrumbslistmodel.h"
#include "fsbrowser.h"

#include <QQmlContext>

namespace Ui {
class BackstageForm;
}

class BackstageOSF: public BackstagePage
{
	Q_OBJECT
	
	Q_PROPERTY(	bool	loggedin		READ loggedin		WRITE setLoggedin		NOTIFY loggedinChanged)
	Q_PROPERTY(	bool	processing		READ processing		WRITE setProcessing		NOTIFY processingChanged)
	Q_PROPERTY(	bool	showfiledialog	READ showfiledialog WRITE setShowfiledialog NOTIFY showfiledialogChanged)
	Q_PROPERTY(	QString	savefilename	READ savefilename	WRITE setSavefilename	NOTIFY savefilenameChanged)	
	Q_PROPERTY(	bool	rememberme		READ rememberme		WRITE setRememberme		NOTIFY remembermeChanged)
	Q_PROPERTY(	QString	username		READ username		WRITE setUsername		NOTIFY usernameChanged)
	Q_PROPERTY(	QString	password		READ password		WRITE setPassword		NOTIFY passwordChanged)
	
	
public:
	explicit BackstageOSF(QWidget *parent = nullptr);
	
	bool loggedin();	
	bool rememberme();
	bool processing();
	bool showfiledialog();
	QString savefilename();
	QString username();
	QString password();
	
	void setLoggedin(const bool loggedin);	
	void setRememberme(const bool rememberme);
	void setProcessing(const bool processing);
	void setSavefilename(const QString &savefilename);	
	void setShowfiledialog(const bool showdialog);
	void setUsername(const QString &username);		
	void setPassword(const QString &password);	
		
	void setOnlineDataManager(OnlineDataManager *odm);
	void attemptToConnect();
	void setCurrentFileName(QString currentFileName);
	void setMode(FileEvent::FileMode mode) OVERRIDE;

signals:
	//void dataSetOpened(QString path); dead code
	void newFolderRequested(QString folderName);
	void loggedinChanged();
	void remembermeChanged();
	void processingChanged();
	void savefilenameChanged();
	void showfiledialogChanged();
	void usernameChanged();
	void passwordChanged();
	void openFileRequest(QString path);
		
private slots:
	void notifyDataSetSelected(QString path);
	void notifyDataSetOpened(QString path);
	void saveClicked();
	void openSaveFile(const QString &nodePath, const QString &filename);
	void userDetailsReceived();
	void openSaveCompleted(FileEvent* event);
	void updateUserDetails();
	void newFolderCreated();
//	void newFolderClicked();
	void authenticatedHandler();
	void resetOSFListModel();
	

public slots:
	void logoutClicked();
	void remembermeCheckChanged(bool check);
	void usernameTextChanged(const QString &username);
	void passwordTextChanged(const QString &password);
	void updateLoginScreen();
	void loginRequested(const QString &username, const QString &password);
	void openFile(const QString &name);
	void saveFile(const QString &name);
	void startProcessing();
	void stopProcessing();
	void newFolderClicked();
	
	
private:	
	bool checkEntryName(QString name, QString entryTitle, bool allowFullStop);	
	
	OnlineDataManager *_odm;	
	OSFListModel *_osfListModel;
	OSFBreadCrumbsListModel *_osfBreadCrumbsListModel;
	FSBMOSF *_model;
	FSBrowser *_fsBrowser;	
	QString _currentFileName;	
	Ui::BackstageForm *ui;
	
	bool _mLoggedin;
	bool _mRememberMe;
	bool _mProcessing;
	bool _mShowFileDialog;
	QString _mSaveFileName;
	QString _mUserName;
	QString _mPassword;
		
};


#endif // BACKSTAGEOSF_H
