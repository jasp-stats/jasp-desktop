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

	bool requiresAuthentication() const OVERRIDE;
	void authenticate(const QString &username, const QString &password) OVERRIDE;
	bool isAuthenticated() const OVERRIDE;
	void clearAuthentication() OVERRIDE;

	OnlineNodeData currentNodeData();

signals:
	void userDataChanged();

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

	void loadProjects();
	void loadFilesAndFolders(QUrl url, int level);
	void parseFilesAndFolders(QUrl url, int level);

	int _level = 0;

};

#endif // FSBMOSF_H
