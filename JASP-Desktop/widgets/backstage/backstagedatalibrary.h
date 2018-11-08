//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef BACKSTAGEDATALIBRARY_H
#define BACKSTAGEDATALIBRARY_H

#include "backstagepage.h"
#include "fsbmdatalibrary.h"
#include "datalibrarylistmodel.h"
#include "datalibrarybreadcrumbsmodel.h"


#include <QQuickWidget>

class BackstageDataLibrary : public BackstagePage
{
	
	Q_OBJECT
public:
	explicit BackstageDataLibrary(QWidget *parent = nullptr, QQuickWidget *qquickfilemenu = nullptr);
	~BackstageDataLibrary();

public slots:
	void openFile(FileEvent *event);
	
private:	
	DataLibraryListModel *_dataLibraryListModel;
	DataLibraryBreadCrumbsListModel *_dataLibraryBreadCrumbsListModel;
				
};

#endif // BACKSTAGEDATALIBRARY_H
