//
// Copyright (C) 2017 University of Amsterdam
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

#include <QLabel>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QMessageBox>
#include <QRegularExpression>

#include "fsbmosf.h"

BackstageOSF::BackstageOSF(QWidget *parent) : BackstagePage(parent)
{
	QGridLayout *layout = new QGridLayout(this);
	layout->setSpacing(0);
	layout->setContentsMargins(0, 0, 0, 0);
	setLayout(layout);

	QWidget *topRow = new QWidget(this);
	layout->addWidget(topRow);

	QGridLayout *topRowLayout = new QGridLayout();
	topRowLayout->setContentsMargins(0, 6, 12, 0);
	topRow->setLayout(topRowLayout);

	QLabel *label = new QLabel("Open Science Framework", topRow);
	QSizePolicy sp = label->sizePolicy();
	sp.setHorizontalStretch(1);
	label->setSizePolicy(sp);
	label->setContentsMargins(12, 12, 12, 1);
	topRowLayout->addWidget(label, 0, 0);

	_logoutButton = new QToolButton(topRow);
	_logoutButton->hide();
	topRowLayout->addWidget(_logoutButton, 0, 1);

	connect(_logoutButton, SIGNAL(clicked(bool)), this, SLOT(logoutClicked()));

	QWidget *buttonsWidget = new QWidget(this);
	buttonsWidget->setContentsMargins(0, 0, 0, 0);
	layout->addWidget(buttonsWidget);

	QGridLayout *buttonsWidgetLayout = new QGridLayout(buttonsWidget);
	buttonsWidgetLayout->setContentsMargins(0, 0, 12, 0);
	buttonsWidget->setLayout(buttonsWidgetLayout);

	_breadCrumbs = new BreadCrumbs(buttonsWidget);
	buttonsWidgetLayout->addWidget(_breadCrumbs, 0, 0);

	_newFolderButton = new QToolButton(buttonsWidget);
	_newFolderButton->setText("New Folder");
	_newFolderButton->hide();
	buttonsWidgetLayout->addWidget(_newFolderButton, 0, 2);

	_fileNameContainer = new QWidget(this);
	_fileNameContainer->hide();
	_fileNameContainer->setObjectName("browseContainer");
	layout->addWidget(_fileNameContainer);

	QHBoxLayout *saveLayout = new QHBoxLayout(_fileNameContainer);
	_fileNameContainer->setLayout(saveLayout);

	_fileNameTextBox = new QLineEdit(_fileNameContainer);
	QSizePolicy policy = _fileNameTextBox->sizePolicy();
	policy.setHorizontalStretch(1);
	_fileNameTextBox->setSizePolicy(policy);
	_fileNameTextBox->setEnabled(false);

	saveLayout->addWidget(_fileNameTextBox);

	_saveButton = new QPushButton(_fileNameContainer);
	_saveButton->setText("Save");
	_saveButton->setEnabled(false);
	saveLayout->addWidget(_saveButton, 0, Qt::AlignRight);

	QWidget *line;

	line = new QWidget(this);
	line->setFixedHeight(1);
	line->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	line->setStyleSheet("QWidget { background-color: #A3A4A5 ; }");
	layout->addWidget(line);

	_model = new FSBMOSF();

	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(updateUserDetails()));
	connect(_model, SIGNAL(authenticationClear()), this, SLOT(updateUserDetails()));

	_fsBrowser = new FSBrowser(this);
	_fsBrowser->setViewType(FSBrowser::ListView);
	_fsBrowser->setFSModel(_model);
	layout->addWidget(_fsBrowser);

	_breadCrumbs->setModel(_model);
	_breadCrumbs->setEnabled(false);

	connect(_fsBrowser, SIGNAL(entryOpened(QString)), this, SLOT(notifyDataSetOpened(QString)));
	connect(_fsBrowser, SIGNAL(entrySelected(QString)), this, SLOT(notifyDataSetSelected(QString)));

	connect(_saveButton, SIGNAL(clicked()), this, SLOT(saveClicked()));
	connect(_newFolderButton, SIGNAL(clicked(bool)), this, SLOT(newFolderClicked()));

	line = new QWidget(this);
	line->setFixedWidth(1);
	line->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);
	line->setStyleSheet("QWidget { background-color: #A3A4A5 ; }");
	layout->addWidget(line, 0, 1, 6, 1);

	QWidget *about = new QWidget(this);
	about->setObjectName("aboutOSF");
	about->setStyleSheet("#aboutOSF { border-top: 1px solid #A3A4A5 ; }");
	layout->addWidget(about);

	QHBoxLayout *aboutLayout = new QHBoxLayout(about);
	aboutLayout->setSpacing(12);
	about->setLayout(aboutLayout);

	HyperlinkLabel *aboutOSF = new HyperlinkLabel(about);
	aboutOSF->setText("<a href='https://osf.io/getting-started/'>About the OSF</a>");

	HyperlinkLabel *registerOSF = new HyperlinkLabel(about);
	registerOSF->setText("<a href='https://osf.io/'>Register</a>");

	aboutLayout->addWidget(aboutOSF);
	aboutLayout->addWidget(registerOSF);
	aboutLayout->addStretch(1);
}

void BackstageOSF::attemptToConnect()
{
	_model->attemptToConnect();
}

void BackstageOSF::updateUserDetails()
{
	if (_model->isAuthenticated())
	{
		_breadCrumbs->setEnabled(true);
		_saveButton->setEnabled(true);
		_fileNameTextBox->setEnabled(true);
		_newFolderButton->setEnabled(true);

		OnlineUserNode *userNode = _odm->getOnlineUserData("https://staging2-api.osf.io/v2/users/me/", "fsbmosf");

		userNode->initialise();

		connect(userNode, SIGNAL(finished()), this, SLOT(userDetailsReceived()));
	}
	else
	{
		_breadCrumbs->setEnabled(false);
		_saveButton->setEnabled(false);
		_fileNameTextBox->setEnabled(false);
		_newFolderButton->setEnabled(false);
	}
}

void BackstageOSF::userDetailsReceived()
{
	OnlineUserNode *userNode = qobject_cast<OnlineUserNode*>(sender());

	_logoutButton->setText("Logout " + userNode->getFullname());

	userNode->deleteLater();
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
		emit newFolderRequested(name);

		if (_model->hasFolderEntry(name.toLower()) == false)
		{
			OnlineDataNode *node = _odm->createNewFolderAsync(_model->currentNodeData().nodePath + "#folder://" + name, name, "createNewFolder");
			connect(node, SIGNAL(finished()), this, SLOT(newFolderCreated()));
		}
	}
}

void BackstageOSF::newFolderCreated()
{
	OnlineDataNode *node = qobject_cast<OnlineDataNode *>(sender());

	if (node->error())
		QMessageBox::warning(this, "", "An error occured and the folder could not be created.");
	else
		_model->refresh();
}

void BackstageOSF::authenticatedHandler()
{
	_newFolderButton->setEnabled(true);
	_logoutButton->show();
}

void BackstageOSF::logoutClicked()
{
	_model->clearAuthentication();
	_logoutButton->hide();
	_model->refresh();
}

void BackstageOSF::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_model->setOnlineDataManager(_odm);

	_newFolderButton->setEnabled(_model->isAuthenticated());
	_logoutButton->setVisible(_model->isAuthenticated());

	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(authenticatedHandler()));
}

void BackstageOSF::setMode(FileEvent::FileMode mode)
{
	BackstagePage::setMode(mode);
	bool visible = (mode == FileEvent::FileSave || mode == FileEvent::FileExportResults || mode == FileEvent::FileExportData);
	_fileNameContainer->setVisible(visible);
	_newFolderButton->setVisible(visible);
}

void BackstageOSF::notifyDataSetOpened(QString path)
{
	FSBMOSF::OnlineNodeData nodeData = _model->getNodeData(path);
	openSaveFile(nodeData.nodePath, nodeData.name);
}

void BackstageOSF::notifyDataSetSelected(QString path)
{
	_fileNameTextBox->setText(QFileInfo(path).fileName());
}

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

	QString filename = _fileNameTextBox->text();

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

	if (event->setPath(nodePath + "#file://" + filename))
	{
		if (storedata)
		{
			_breadCrumbs->setEnabled(false);
			_saveButton->setEnabled(false);
			_fileNameTextBox->setEnabled(false);
			_newFolderButton->setEnabled(false);
			_fileNameTextBox->setText(filename);

			_fsBrowser->StartProcessing();

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

void BackstageOSF::openSaveCompleted(FileEvent* event)
{

	_breadCrumbs->setEnabled(true);
	_saveButton->setEnabled(true);
	_fileNameTextBox->setEnabled(true);
	_newFolderButton->setEnabled(true);

	_fsBrowser->StopProcessing();

	if (event->successful())
		_model->refresh();
}
