//
// Copyright (C) 2015 University of Amsterdam
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

#include "fsbmosf.h"

BackstageOSF::BackstageOSF(QWidget *parent) : BackstagePage(parent)
{
	QGridLayout *layout = new QGridLayout(this);
	layout->setSpacing(0);
	layout->setContentsMargins(0, 0, 0, 0);
	setLayout(layout);

	QLabel *label = new QLabel("Open Science Framework", this);
	label->setContentsMargins(12, 12, 0, 1);
	layout->addWidget(label, 0, 0, 1, 1);

	_nameLabel = new QLabel(this);
	_nameLabel->setContentsMargins(12, 12, 0, 1);
	layout->addWidget(_nameLabel, 1, 0, 1, 1);

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

	saveLayout->addWidget(_fileNameTextBox);

	_saveButton = new QPushButton(_fileNameContainer);
	_saveButton->setText("Save");
	saveLayout->addWidget(_saveButton, 0, Qt::AlignRight);

	QWidget *line;

	line = new QWidget(this);
	line->setFixedHeight(1);
	line->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	line->setStyleSheet("QWidget { background-color: #A3A4A5 ; }");
	layout->addWidget(line);

	_model = new FSBMOSF();

	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(updateUserDetails()));

	//_model->refresh();

	_fsBrowser = new FSBrowser(this);
	_fsBrowser->setViewType(FSBrowser::ListView);
	_fsBrowser->setFSModel(_model);
	layout->addWidget(_fsBrowser);

	_breadCrumbs->setModel(_model);

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

void BackstageOSF::updateUserDetails()
{
	OnlineUserNode *userNode = _odm->getOnlineUserData("https://staging2-api.osf.io/v2/users/me/", "fsbmosf");

	userNode->initialise();

	connect(userNode, SIGNAL(finished()), this, SLOT(userDetailsReceived()));
}

void BackstageOSF::userDetailsReceived()
{
	OnlineUserNode *userNode = qobject_cast<OnlineUserNode*>(sender());

	_nameLabel->setText(userNode->getFullname());

	userNode->deleteLater();
}

void BackstageOSF::newFolderClicked()
{
	bool ok;
	QString name = QInputDialog::getText(this, "New folder", "New folder name", QLineEdit::Normal, "New folder", &ok);

	if (ok)
		emit newFolderRequested(name);
}

void BackstageOSF::authenticatedHandler()
{
	_newFolderButton->setEnabled(true);
}

void BackstageOSF::setOnlineDataManager(OnlineDataManager *odm)
{
	_odm = odm;
	_model->setOnlineDataManager(_odm);

	_newFolderButton->setEnabled(_model->isAuthenticated());

	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(authenticatedHandler()));
}

void BackstageOSF::setMode(FileEvent::FileMode mode)
{
	BackstagePage::setMode(mode);
	_fileNameContainer->setVisible(mode == FileEvent::FileSave);
	_newFolderButton->setVisible(mode == FileEvent::FileSave);
}

void BackstageOSF::notifyDataSetOpened(QString path)
{
	FSBMOSF::OnlineNodeData nodeData = _model->getNodeData(path);
	openFile(nodeData.nodePath, nodeData.name);
}

void BackstageOSF::notifyDataSetSelected(QString path)
{
	_fileNameTextBox->setText(QFileInfo(path).fileName());
}

void BackstageOSF::saveClicked()
{
	QString filename = _fileNameTextBox->text();

	if (filename.endsWith(".jasp") == false)
	{
		filename = filename + ".jasp";
		_fileNameTextBox->setText(filename);
	}

	QString path;

	if (_model->hasFileEntry(filename, path))
		notifyDataSetOpened(path);
	else
		openFile(_model->currentNodeData().nodePath, filename);
}

void BackstageOSF::openFile(const QString &nodePath, const QString &filename)
{
	FileEvent *event = new FileEvent(this);
	event->setOperation(_mode);

	if (_mode == FileEvent::FileSave)
		connect(event, SIGNAL(completed(FileEvent*)), this, SLOT(saveCompleted(FileEvent*)));

	if (filename != "")
	{
		event->setPath(nodePath + "#file://" + filename);

		if ( ! filename.endsWith(".jasp", Qt::CaseInsensitive))
			event->setReadOnly();
	}
	else
		event->setComplete(false, "Failed to open file from OSF");

	emit dataSetIORequest(event);
}

void BackstageOSF::saveCompleted(FileEvent* event)
{
	if (event->successful())
		_model->refresh();
}
