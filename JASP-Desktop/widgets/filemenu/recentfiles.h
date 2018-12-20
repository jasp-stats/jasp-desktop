//
// Copyright (C) 2018 University of Amsterdam
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

#ifndef RECENTFILES_H
#define RECENTFILES_H

#include "filemenuobject.h"
#include "recentfileslistmodel.h"

class RecentFiles : public FileMenuObject		
{
	Q_OBJECT	
	Q_PROPERTY(RecentFilesListModel * listModel READ listModel WRITE setListModel NOTIFY listModelChanged)
	
public:
	explicit RecentFiles(QObject *parent = nullptr);

	void pushRecentFilePath(const QString & newrecent);

	RecentFilesListModel * listModel() const { return _recentFilesListModel; }

public slots:
	void openFile(FileEvent *event);
	
	void setListModel(RecentFilesListModel * listModel);

signals:
	void listModelChanged(RecentFilesListModel * listModel);

private:
	RecentFilesListModel *_recentFilesListModel;
};
				
#endif // RECENTFILES_H
