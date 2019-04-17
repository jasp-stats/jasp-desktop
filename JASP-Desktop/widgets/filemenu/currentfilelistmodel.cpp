#include "currentfilelistmodel.h"
#include "filesystementry.h"
#include <QFileInfo>
#include <QDir>

CurrentFileListModel::CurrentFileListModel(QObject *parent)
	: FileMenuBasicListModel(parent, new CurrentFileFileSystem(parent))
{
	_openFileWhenClicked = false;
	_fsbmCurrentFile = static_cast<CurrentFileFileSystem*>(_model);
	_fsbmCurrentFile->refresh();	
}

CurrentFileFileSystem *CurrentFileListModel::getCurrentFileFSBModel()
{
	return _fsbmCurrentFile;
}

void CurrentFileListModel::setCurrentFilePath(const QString &newcurrent)
{
	beginResetModel();
	
	_fsbmCurrentFile->setCurrent(newcurrent);
	_fsbmCurrentFile->refresh();
	
	endResetModel();	
}

void CurrentFileListModel::openFile(const QString &path)
{
	if (path.isEmpty())
		return;

	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileSyncData);
	event->setPath(path);

	emit syncCurrentFile(event);
}
