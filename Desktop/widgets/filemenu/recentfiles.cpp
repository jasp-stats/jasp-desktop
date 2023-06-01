#include "recentfiles.h"
#include <QQmlEngine>


RecentFiles::RecentFiles(FileMenu *parent): FileMenuObject(parent)
{	
	setListModel(new RecentFilesListModel(this));
}

void RecentFiles::pushRecentFilePath(const QString &newrecent)
{
	_recentFilesListModel->addRecentFilePath(newrecent);
}

void RecentFiles::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}

void RecentFiles::setListModel(RecentFilesListModel * listModel)
{
	if (_recentFilesListModel == listModel)
		return;

	_recentFilesListModel = listModel;
	emit listModelChanged(_recentFilesListModel);
}
