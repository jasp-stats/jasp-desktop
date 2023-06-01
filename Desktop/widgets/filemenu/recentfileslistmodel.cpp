#include "recentfileslistmodel.h"
#include "filesystementry.h"
#include <QFileInfo>
#include <QDir>
#include "recentfiles.h"

RecentFilesListModel::RecentFilesListModel(QObject *parent)	: FileMenuBasicListModel(parent, new RecentFilesFileSystem(parent))
{
	_fsbmRecentFiles = static_cast<RecentFilesFileSystem*>(_model);
	_fsbmRecentFiles->refresh();

	connect(this, &RecentFilesListModel::openFileEvent, dynamic_cast<RecentFiles*> (parent), &RecentFiles::openFile);
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

	emit openFileEvent(event);
}
