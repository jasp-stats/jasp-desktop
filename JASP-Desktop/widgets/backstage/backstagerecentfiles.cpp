#include "backstagerecentfiles.h"
#include "ui_backstageform.h"
#include <QQmlEngine>


BackstageRecentFiles::BackstageRecentFiles(QObject *parent): BackstagePage(parent)
{	
	setRecentFiles(new RecentFilesListModel(this));
}

void BackstageRecentFiles::pushRecentFilePath(const QString &newrecent)
{
	_recentFilesListModel->addRecentFilePath(newrecent);
}

void BackstageRecentFiles::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}

void BackstageRecentFiles::setRecentFiles(RecentFilesListModel * recentFiles)
{
	if (_recentFilesListModel == recentFiles)
		return;

	_recentFilesListModel = recentFiles;
	emit recentFilesChanged(_recentFilesListModel);
}
