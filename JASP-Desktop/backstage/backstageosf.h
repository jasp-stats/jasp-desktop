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

#ifndef BACKSTAGEOSF_H
#define BACKSTAGEOSF_H

#include "backstagepage.h"

#include <QPushButton>

#include "fsbmcomputer.h"
#include "fsbrowser.h"
#include "breadcrumbs.h"

class BackstageOSF : public BackstagePage
{
	Q_OBJECT
public:
	explicit BackstageOSF(QWidget *parent = 0);

signals:
	void dataSetOpened(QString path);

private slots:
	void notifyDataSetOpened(QString path);

private:
	BreadCrumbs *_breadCrumbs;
	FSBMComputer *_model;
	FSBrowser *_fsBrowser;
	QPushButton *_browseButton;
};

#endif // BACKSTAGEOSF_H
