//
// Copyright (C) 2017 University of Amsterdam
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

#ifndef FSBMOSF_H
#define FSBMOSF_H

#include <QPushButton>
#include <QNetworkAccessManager>
#include <QMap>
#include <QSettings>

#include "fsbmodel.h"
#include "common.h"
#include "onlinedatamanager.h"

class FSBMOSF : public FSBModel
{
	Q_OBJECT

public:
	FSBMOSF();
	~FSBMOSF();
	void refresh() OVERRIDE;

	typedef struct {
		QString name;
		bool isFolder;
		bool isComponent;
		bool isProvider;
		QString contentsPath;
		QString childrenPath;
		QString uploadPath;
		QString downloadPath;
		QString nodePath;
		bool canCreateFolders;
		bool canCreateFiles;
		int level = 0;
	} OnlineNodeData;

	OnlineNodeData getNodeData(QString key);
	void setOnlineDataManager(OnlineDataManager *odm);
	void attemptToConnect();

	bool requiresAuthentication() const OVERRIDE;
	void authenticate(const QString &username, const QString &password) OVERRIDE;
	bool isAuthenticated() const OVERRIDE;
	void clearAuthentication() OVERRIDE;

	OnlineNodeData currentNodeData();

signals:
	void userDataChanged();
	void hideAuthentication();

private slots:
	void gotProjects();
	void gotFilesAndFolders();

private:

	QSettings _settings;

	void setAuthenticated(bool value);

	QString getRelationshipUrl(QJsonObject nodeObject, QString name);

	QMap<QString, OnlineNodeData> _pathUrls;

	OnlineDataManager *_dataManager = NULL;
	QNetworkAccessManager *_manager = NULL;

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

	int _level = 0;

};

#endif // FSBMOSF_H
