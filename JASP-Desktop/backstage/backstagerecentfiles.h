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
#include <QQmlContext>

namespace Ui {
class BackstageForm;
}

class BackstageRecentFiles : public BackstagePage		
{
	
	Q_OBJECT	
	
public:
	explicit BackstageRecentFiles(QWidget *parent = 0);
	~BackstageRecentFiles();
	void pushRecentFilePath(const QString & newrecent);

public slots:
	void openFile(FileEvent *event);
	
private:
	RecentFilesListModel *_recentFilesListModel;
	Ui::BackstageForm *ui;
	
};
				
#endif // BACKSTAGERECENTFILES_H
