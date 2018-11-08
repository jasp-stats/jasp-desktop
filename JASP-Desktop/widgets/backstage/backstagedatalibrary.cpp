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

#include "backstagedatalibrary.h"
#include <QDir>
#include <QQuickWidget>
#include <QQmlContext>

BackstageDataLibrary::BackstageDataLibrary(QWidget *parent, QQuickWidget *qquickfilemenu) : BackstagePage(parent)
{
			
	_dataLibraryBreadCrumbsListModel = new DataLibraryBreadCrumbsListModel(this);
	_dataLibraryBreadCrumbsListModel->setSeparator(QDir::separator());
	
	_dataLibraryListModel = new DataLibraryListModel(this);
	_dataLibraryListModel->setBreadCrumbsListModel(_dataLibraryBreadCrumbsListModel);
	
	connect(_dataLibraryBreadCrumbsListModel, SIGNAL(crumbIndexChanged(const int &)), _dataLibraryListModel, SLOT(changePath(const int &)));

	qquickfilemenu->rootContext()->setContextProperty("dataLibraryListModel",_dataLibraryListModel);
	qquickfilemenu->rootContext()->setContextProperty("breadcrumbsmodel",_dataLibraryBreadCrumbsListModel); // Calling changePath(index)
	qquickfilemenu->rootContext()->setContextProperty("dataLibraryBreadCrumbsListModel",_dataLibraryBreadCrumbsListModel);
	qquickfilemenu->rootContext()->setContextProperty("backstagedatalibrary",this);	
			
}

BackstageDataLibrary::~BackstageDataLibrary()
{
	
}

void BackstageDataLibrary::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}


