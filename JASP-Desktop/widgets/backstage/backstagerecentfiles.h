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

#ifndef BACKSTAGERECENTFILES_H
#define BACKSTAGERECENTFILES_H

#include "backstagepage.h"
#include "recentfileslistmodel.h"

class BackstageRecentFiles : public BackstagePage		
{
	Q_OBJECT	
	Q_PROPERTY(RecentFilesListModel * recentFiles READ recentFiles WRITE setRecentFiles NOTIFY recentFilesChanged)
	
public:
	explicit BackstageRecentFiles(QObject *parent = nullptr);

	void pushRecentFilePath(const QString & newrecent);

	RecentFilesListModel * recentFiles() const { return _recentFilesListModel; }

public slots:
	void openFile(FileEvent *event);
	
	void setRecentFiles(RecentFilesListModel * recentFiles);

signals:
	void recentFilesChanged(RecentFilesListModel * recentFiles);

private:
	RecentFilesListModel *_recentFilesListModel;
};
				
#endif // BACKSTAGERECENTFILES_H
