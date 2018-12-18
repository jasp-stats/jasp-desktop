#include "recentfiles.h"
#include "ui_backstageform.h"
#include <QQmlEngine>


BackstageRecentFiles::BackstageRecentFiles(QObject *parent): BackstagePage(parent)
{	
	setListModel(new RecentFilesListModel(this));
}

void BackstageRecentFiles::pushRecentFilePath(const QString &newrecent)
{
	_recentFilesListModel->addRecentFilePath(newrecent);
}

void BackstageRecentFiles::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}

void BackstageRecentFiles::setListModel(RecentFilesListModel * listModel)
{
	if (_recentFilesListModel == listModel)
		return;

	_recentFilesListModel = listModel;
	emit listModelChanged(_recentFilesListModel);
}
