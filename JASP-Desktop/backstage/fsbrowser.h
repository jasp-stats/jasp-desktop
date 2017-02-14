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

#ifndef FSBROWSER_H
#define FSBROWSER_H

#include <QWidget>
#include <QGridLayout>
#include <QButtonGroup>
#include <QLabel>
#include <QSettings>
#include <QCheckBox>

#include "fsbmodel.h"
#include "breadcrumbs.h"
#include "authwidget.h"
#include "verticalscrollarea.h"

class FSBrowser : public QWidget
{
	Q_OBJECT
public:
	enum BrowseMode { BrowseOpenFile, BrowseOpenFolder, BrowseSaveFile, BrowseExportFile, BrowseCurrent};
	enum ViewType   { IconView, ListView };

	explicit FSBrowser(QWidget *parent = 0, BrowseMode mode = BrowseOpenFile);

	void setFSModel(FSBModel *model);


	void setBrowseMode(BrowseMode mode);
	void setViewType(ViewType viewType);
	void StartProcessing();
	void StopProcessing();

signals:

	void entryOpened(QString path);
	void entrySelected(QString path);

public slots:

private slots:

	void processingEntries();
	void refresh();
	void loginRequested(QString username, QString password);
	void entrySelectedHandler();
	void entryOpenedHandler();
	void authenticationFailed(QString message);
	void hideAuthentication();

private:

	void clearItems();

	BrowseMode _browseMode;
	ViewType _viewType;

	QWidget *_scrollPane;
	QVBoxLayout *_scrollPaneLayout;
	QButtonGroup *_buttonGroup;
	QLabel *_processLabel;
	VerticalScrollArea *_scrollArea;

	FSBModel *_model;

	AuthWidget *_authWidget;
	QSettings _settings;

};

#endif // FSBROWSER_H
