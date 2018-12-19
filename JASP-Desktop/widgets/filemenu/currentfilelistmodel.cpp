#include "currentfilelistmodel.h"
#include "filesystementry.h"
#include <QFileInfo>
#include <QDir>

CurrentFileListModel::CurrentFileListModel(QObject *parent)
	: FileMenuBasicListModel(parent, new CurrentFileFileSystem(parent))
{
	_fsbmCurrentFile = static_cast<CurrentFileFileSystem*>(_model);
	_fsbmCurrentFile->refresh();
	
	connect(this, SIGNAL(syncFile(FileEvent *)), parent, SLOT(syncFile(FileEvent *)));
	std::cout << "CurrentFileListModel is SIGNAL SLOTTING WITH Macros with it's parent... that isnt safe you know." << std::endl;
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

void CurrentFileListModel::syncFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileSyncData);
	event->setPath(path);

	emit syncFile(event);
}
