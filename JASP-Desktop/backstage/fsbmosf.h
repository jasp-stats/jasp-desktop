#ifndef FSBMOSF_H
#define FSBMOSF_H

#include <QPushButton>
#include <QNetworkAccessManager>
#include <QMap>

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
		QString contentsPath;
		QString uploadPath;
		QString downloadPath;
		QString nodePath;
	} OnlineNodeData;

	OnlineNodeData getNodeData(QString key);
	void setOnlineDataManager(OnlineDataManager *odm);

	bool requiresAuthentication() const OVERRIDE;
	void authenticate(const QString &username, const QString &password) OVERRIDE;
	bool isAuthenticated() const OVERRIDE;

	OnlineNodeData currentNodeData();

signals:
	void userDataChanged();

private slots:
	void gotProjects();
	void gotFilesAndFolders();

private:

	QMap<QString, OnlineNodeData> _pathUrls;

	OnlineDataManager *_dataManager;
	QNetworkAccessManager *_manager;

	QString _userId;
	QString _filesPath;
	QString _fullname;

	bool _isAuthenticated;

	void loadProjects();
	void loadFilesAndFolders(QUrl url);

};

#endif // FSBMOSF_H
