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

#ifndef BACKSTAGECOMPUTER_H
#define BACKSTAGECOMPUTER_H

#include "backstagepage.h"
#include "computerlistmodel.h"
#include <QQuickWidget>

class BackstageComputer : public BackstagePage
{
	Q_OBJECT
	
public:
	explicit BackstageComputer(QWidget *parent = nullptr,  QQuickWidget *qquickfilemenu = nullptr);
	~BackstageComputer();
	
	FileEvent *browseOpen(const QString &path = "");
	FileEvent *browseSave(const QString &path = "", FileEvent::FileMode mode = FileEvent::FileSave);
	
	void setFileName(const QString &filename);
	void clearFileName();
	void addRecentFolder(const QString &path);
		
protected:
	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;

public slots:
	void browsePath(QString path);
	void browseMostRecent();
	
private:
	// these two variables are a hack!
	bool _hasFileName;
	QString _fileName;	
	
	ComputerListModel *_computerListModel;	

};

#endif // BACKSTAGECOMPUTER_H
