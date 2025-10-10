#include "recentfileslistmodel.h"
#include "utilities/appdirs.h"
#include "recentfiles.h"
#include <QFileInfo>
#include <QDir>

RecentFilesListModel::RecentFilesListModel(QObject *parent)	: FileMenuBasicListModel(parent, new RecentFilesFileSystem(parent))
{
	_fsbmRecentFiles = static_cast<RecentFilesFileSystem*>(_model);
	_fsbmRecentFiles->refresh();

	connect(this, &RecentFilesListModel::openFileEvent, dynamic_cast<RecentFiles*> (parent), &RecentFiles::openFile);
}

void RecentFilesListModel::addRecentFilePath(const QString &newpath)
{
	QFileInfo	path		( newpath );
	QDir		autoSaveDir = AppDirs::autoSaveDir();
	

	if(path.dir() == autoSaveDir)
		return;

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
