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

#include "datalibrary.h"
#include <QDir>
#include <QQuickWidget>
#include <QQmlContext>
#include <QQmlEngine>

DataLibrary::DataLibrary(FileMenu *parent) : FileMenuObject(parent)
{
	setBreadcrumbsmodel(new DataLibraryBreadCrumbsListModel(this, QDir::separator()));
	setListModel(new DataLibraryListModel(this, breadcrumbsmodel()));
	
	connect(_dataLibraryBreadCrumbsListModel,	&DataLibraryBreadCrumbsListModel::crumbIndexChanged,	_dataLibraryListModel,	&DataLibraryListModel::changePathCrumbIndex	);
	connect(this,								&DataLibrary::resetPath,								_dataLibraryListModel,	&DataLibraryListModel::resetPath			);
}

void DataLibrary::refres()
{
	_dataLibraryListModel->refresh();
}

void DataLibrary::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}

void DataLibrary::setListModel(DataLibraryListModel * listModel)
{
	if (_dataLibraryListModel == listModel)
		return;

	_dataLibraryListModel = listModel;
	emit listModelChanged();
}

void DataLibrary::setBreadcrumbsmodel(DataLibraryBreadCrumbsListModel * breadcrumbsmodel)
{
	if (_dataLibraryBreadCrumbsListModel == breadcrumbsmodel)
		return;

	_dataLibraryBreadCrumbsListModel = breadcrumbsmodel;
	emit breadcrumbsmodelChanged();
}


