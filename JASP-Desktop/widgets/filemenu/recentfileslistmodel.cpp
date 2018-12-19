#include "recentfileslistmodel.h"
#include "fsentry.h"
#include <QFileInfo>
#include <QDir>

RecentFilesListModel::RecentFilesListModel(QObject *parent)	: FileMenuBasicListModel(parent, new FSBMRecentFiles(parent))
{
	_fsbmRecentFiles = static_cast<FSBMRecentFiles*>(_model);
	_fsbmRecentFiles->refresh();

	connect(this, SIGNAL(openFile(FileEvent *)), parent, SLOT(openFile(FileEvent *)));

}

void RecentFilesListModel::addRecentFilePath(const QString &newpath)
{
	
	beginResetModel();
	
	_fsbmRecentFiles->addRecent(newpath);
	_fsbmRecentFiles->refresh();
	
	endResetModel();
	
}

//Slots
void RecentFilesListModel::openFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileOpen);
	event->setPath(path);

	emit openFile(event);
}
