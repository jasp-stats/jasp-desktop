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

#include "fsbrowser.h"

#include <QGridLayout>
#include <QScrollArea>
#include <QMessageBox>
#include <QMovie>

#include "fsentrywidget.h"
#include <iostream>


FSBrowser::FSBrowser(QWidget *parent, FSBrowser::BrowseMode mode) : QWidget(parent)
{
	_browseMode = mode;
	_viewType = FSBrowser::IconView;

	QGridLayout *layout = new QGridLayout(this);
	layout->setContentsMargins(10, 10, 0, 0);
	setLayout(layout);

	if (mode == FSBrowser::BrowseCurrent)
	{
#ifdef __APPLE__
		QString shortCutKey = "\u2318";
#else
		QString shortCutKey = "Ctrl";
#endif
		layout->addWidget(new QLabel(QString("Double-click on the file below to synchronize or use ") + shortCutKey + "-Y"));
	}

	_scrollArea = new VerticalScrollArea(this);
	_scrollArea->setFrameShape(QScrollArea::NoFrame);
	layout->addWidget(_scrollArea);

	_scrollPane = new QWidget;
	_scrollArea->setWidget(_scrollPane);

	_scrollPaneLayout = new QVBoxLayout(_scrollPane);
	_scrollPaneLayout->setSpacing(1);
	_scrollPaneLayout->setSizeConstraint(QLayout::SetMinAndMaxSize);
	_scrollPane->setLayout(_scrollPaneLayout);

	_buttonGroup = new QButtonGroup(this);

	_authWidget = new AuthWidget(this);
	_authWidget->hide();

	_processLabel = new QLabel(this);
	_processLabel->setAlignment(Qt::AlignCenter);
	_processLabel->setSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
	_processLabel->setMovie(new QMovie(":/icons/loading.gif", QByteArray(), _processLabel));
	_processLabel->setHidden(true);
	layout->addWidget(_processLabel);

	connect(_authWidget, SIGNAL(loginRequested(QString,QString)), this, SLOT(loginRequested(QString,QString)));
}

void FSBrowser::StartProcessing()
{
	_processLabel->movie()->start();
	_processLabel->setHidden(false);
	_scrollArea->setHidden(true);
}

void FSBrowser::StopProcessing()
{
	_processLabel->movie()->stop();
	_processLabel->setHidden(true);
	_scrollArea->setHidden(false);
}

void FSBrowser::setFSModel(FSBModel *model)
{
	_model = model;
	_model->refresh();
	refresh();

	connect(_model, SIGNAL(entriesChanged()), this, SLOT(refresh()));
	connect(_model, SIGNAL(processingEntries()), this, SLOT(processingEntries()));
	connect(_model, SIGNAL(authenticationSuccess()), this, SLOT(refresh()));
	connect(_model, SIGNAL(authenticationClear()), this, SLOT(refresh()));
	connect(_model, SIGNAL(authenticationFail(QString)), this, SLOT(authenticationFailed(QString)));
	connect(_model, SIGNAL(hideAuthentication()), this, SLOT(hideAuthentication()));
}

void FSBrowser::setBrowseMode(FSBrowser::BrowseMode mode)
{
	_browseMode = mode;
}

void FSBrowser::setViewType(FSBrowser::ViewType viewType)
{
	_viewType = viewType;
}

void FSBrowser::hideAuthentication()
{
	_authWidget->hide();
}

void FSBrowser::clearItems()
{
	foreach (QAbstractButton *button, _buttonGroup->buttons())
		delete button;
}

void FSBrowser::processingEntries()
{
	StartProcessing();
}

void FSBrowser::refresh()
{
	clearItems();

	StopProcessing();

	if (_model->requiresAuthentication() && !_model->isAuthenticated())
	{
		_authWidget->setUsernameclearPassword();
		_authWidget->show();
	}
	else
	{
		_authWidget->hide();

		bool compact = false;

		if (_viewType == ListView)
		{
			compact = true;
			_scrollPaneLayout->setContentsMargins(8, 8, 8, 8);
			_scrollPaneLayout->setSpacing(0);
		}
		else
		{
			_scrollPaneLayout->setContentsMargins(12, 12, 12, 12);
			_scrollPaneLayout->setSpacing(8);
		}

		int id = 0;

		foreach (const FSEntry &entry, _model->entries())
		{
			FSEntryWidget *button = new FSEntryWidget(entry, _scrollPane);
			button->setCompact(compact);

			_buttonGroup->addButton(button, id++);
			_scrollPaneLayout->addWidget(button);

			connect(button, SIGNAL(selected()), this, SLOT(entrySelectedHandler()));
			connect(button, SIGNAL(opened()), this, SLOT(entryOpenedHandler()));
		}


	}
}

void FSBrowser::loginRequested(QString username, QString password)
{
	_model->authenticate(username, password);
}

void FSBrowser::entrySelectedHandler()
{
	FSEntryWidget *entry = qobject_cast<FSEntryWidget*>(this->sender());
	if (entry->entryType() != FSEntry::Folder)
		emit entrySelected(entry->path());
}

void FSBrowser::entryOpenedHandler()
{
	FSEntryWidget *entry = qobject_cast<FSEntryWidget*>(this->sender());

	if (_browseMode == BrowseOpenFolder)
	{
		emit entryOpened(entry->path());
	}
	else
	{
		if (entry->entryType() == FSEntry::Folder)
			_model->setPath(entry->path());
		else
			emit entryOpened(entry->path());
	}
}

void FSBrowser::authenticationFailed(QString message)
{
	QMessageBox::warning(this, "", message);

}

