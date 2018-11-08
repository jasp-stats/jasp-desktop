#include "backstagerecentfiles.h"
#include "ui_backstageform.h"


BackstageRecentFiles::BackstageRecentFiles(QWidget *parent, QQuickWidget *qquickfilemenu): BackstagePage(parent)
{	
	
	_recentFilesListModel = new RecentFilesListModel(this);
	
	qquickfilemenu->rootContext()->setContextProperty("recentFilesListModel", _recentFilesListModel);
	
}

BackstageRecentFiles::~BackstageRecentFiles()
{
}

void BackstageRecentFiles::pushRecentFilePath(const QString &newrecent)
{
	_recentFilesListModel->addRecentFilePath(newrecent);
}

void BackstageRecentFiles::openFile(FileEvent *event)
{
	emit dataSetIORequest(event);
}

