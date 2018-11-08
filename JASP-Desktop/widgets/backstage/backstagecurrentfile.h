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


#ifndef BACKSTAGECURRENTFILE_H
#define BACKSTAGECURRENTFILE_H

#include "backstagepage.h"
#include "currentfilelistmodel.h"
#include <QQuickWidget>

class BackstageCurrentFile : public BackstagePage
{
	Q_OBJECT
	
public:
	explicit BackstageCurrentFile(QWidget *parent = nullptr,  QQuickWidget *qquickfilemenu = nullptr);
	~BackstageCurrentFile();
	
	void setCurrentFilePath(const QString &path);
	void setCurrentDataFilePath(const QString &path);
    void setCurrentFileInfo(const QString &path, const Utils::FileType &type, const bool & readonly);
	void setCurrentFileType(const Utils::FileType &type);
	void setCurrentFileReadOnly(const bool & readonly);
	
	Utils::FileType getCurrentFileType();	
	bool isCurrentFileReadOnly();
	bool isOnlineFile(const QString &path);
		
	//Special
	CurrentFileListModel * getCurrentFileListModel();

public slots:
	QString getCurrentFilePath();
	QString getCurrentDataFilePath();
	QString getCurrentDataFileName();
	QString getCurrentDataFolder();
	QString getHeaderText();		
	void syncFile(FileEvent *event);
	
private:
	CurrentFileListModel *_currentFileListModel;
	
	QString _currentFilePath;
	QString _currentDataFilePath;
	Utils::FileType _currentFileType;
	bool _currentFileReadOnly;
		
};

#endif // BACKSTAGECURRENTFILE_H
