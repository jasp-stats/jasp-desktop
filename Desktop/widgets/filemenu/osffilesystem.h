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

#ifndef OSFFILESYSTEM_H
#define OSFFILESYSTEM_H

#include <QPushButton>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QMap>

#include "filesystem.h"
#include "common.h"
#include "osf/onlinedatamanager.h"
#include "sortable.h"

class OSFFileSystem : public FileSystem, public Sortable
{
	Q_OBJECT

public:
	OSFFileSystem(QObject *parent = nullptr, QString root = "");
	~OSFFileSystem() override;
	void refresh() override;

	struct OnlineNodeData
	{
		QString name;
		bool	isFolder,
				isComponent,
				isProvider;
		QString	contentsPath,
				childrenPath,
				uploadPath,
				downloadPath,
				nodePath;
		bool	canCreateFolders,
				canCreateFiles;
		int		level = 0;
	};
	
	static const QString rootelementname; //Root element in the OSF

	OnlineNodeData getNodeData(QString key);
	void setOnlineDataManager(OnlineDataManager *odm);
	void attemptToConnect();
	void updateAuthentication(bool authenticated);

	bool requiresAuthentication()			const override;
	bool isAuthenticated()					const override;
	void clearAuthentication()				override;

	OnlineNodeData currentNodeData();
	void sortItems(SortType sortType)	override;

signals:
	void userDataChanged();
	void stopProcessing();
	void newLoginRequired();
	
private slots:
	void gotProjects();
	void gotFilesAndFolders();

public slots:
	void handleAuthenticationResult(bool);

private:
	void setAuthenticated(bool value);

	QString getRelationshipUrl(QJsonObject nodeObject, QString name);

	QMap<QString, OnlineNodeData> _pathUrls;

	OnlineDataManager *_dataManager = nullptr;
	QNetworkAccessManager *_manager = nullptr;

	QString _userId;
	QString _filesPath;
	QString _fullname;

	bool _isAuthenticated;
	bool _isPaginationCall = false;
	bool _isProjectPaginationCall = false;

	void loadProjects();
	void loadFilesAndFolders(QUrl url, int level);
	void parseFilesAndFolders(QUrl url, int level, bool recursive = false);
	void parseProjects(QUrl url, bool recursive = false);
	void handleNetworkReplyError(QNetworkReply* reply);

	QDateTime osfJsonToDateTime(const QString &input);

	FileSystemEntryList _unsortedEntries;

	int _level = 0;

};

#endif // OSFFILESYSTEM_H
