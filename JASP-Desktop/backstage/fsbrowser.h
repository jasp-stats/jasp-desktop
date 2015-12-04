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

#ifndef FSBROWSER_H
#define FSBROWSER_H

#include <QWidget>
#include <QGridLayout>
#include <QButtonGroup>

#include "fsbmodel.h"
#include "breadcrumbs.h"

class FSBrowser : public QWidget
{
	Q_OBJECT
public:
	explicit FSBrowser(QWidget *parent = 0);

	void setFSModel(FSBModel *model);

	enum BrowseMode { BrowseOpenFile, BrowseOpenFolder, BrowseSaveFile };
	enum ViewType   { IconView, ListView };

	void setBrowseMode(BrowseMode mode);
	void setViewType(ViewType viewType);

signals:

	void entryOpened(QString path);
	void entrySelected(QString path);

public slots:

private slots:

	void refresh();
	void entrySelectedHandler();
	void entryOpenedHandler();

private:

	BrowseMode _browseMode;
	ViewType _viewType;

	QWidget *_scrollPane;
	QVBoxLayout *_scrollPaneLayout;
	QButtonGroup *_buttonGroup;

	FSBModel *_model;


};

#endif // FSBROWSER_H
