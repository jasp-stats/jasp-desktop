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

#ifndef DATALIBRARY_H
#define DATALIBRARY_H

#include "filemenuobject.h"
#include "datalibraryfilesystem.h"
#include "datalibrarylistmodel.h"
#include "datalibrarybreadcrumbsmodel.h"

class DataLibrary : public FileMenuObject
{
	
	Q_OBJECT
	Q_PROPERTY(DataLibraryListModel * listModel						READ listModel			WRITE setListModel			NOTIFY listModelChanged)
	Q_PROPERTY(DataLibraryBreadCrumbsListModel * breadcrumbsmodel	READ breadcrumbsmodel	WRITE setBreadcrumbsmodel	NOTIFY breadcrumbsmodelChanged)

public:
	explicit DataLibrary(FileMenu *parent = nullptr);
	~DataLibrary() {}

	DataLibraryListModel *				listModel()			const {	return _dataLibraryListModel; }
	DataLibraryBreadCrumbsListModel *	breadcrumbsmodel()	const {	return _dataLibraryBreadCrumbsListModel; }
	void refres();

signals:
	void breadcrumbsmodelChanged();
	void listModelChanged();
	void resetPath();

public slots:
	void openFile(FileEvent *event);
	
	void setListModel(DataLibraryListModel * listModel);
	void setBreadcrumbsmodel(DataLibraryBreadCrumbsListModel * breadcrumbsmodel);

private:
	DataLibraryListModel			*_dataLibraryListModel				= nullptr;
	DataLibraryBreadCrumbsListModel	*_dataLibraryBreadCrumbsListModel	= nullptr;

};

#endif // DATALIBRARY_H
