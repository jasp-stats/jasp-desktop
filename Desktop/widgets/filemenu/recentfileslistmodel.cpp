#include "recentfileslistmodel.h"
#include "utilities/appdirs.h"
#include "recentfiles.h"
#include <QFileInfo>
#include <QDir>

RecentFilesListModel::RecentFilesListModel(FileMenuObject *parent)	: FileMenuBasicListModel(parent, new RecentFilesFileSystem(parent))
{
	_fsbmRecentFiles = static_cast<RecentFilesFileSystem*>(_model);
	_fsbmRecentFiles->refresh();
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
