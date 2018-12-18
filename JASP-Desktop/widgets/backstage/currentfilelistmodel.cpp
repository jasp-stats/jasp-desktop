#include "currentfilelistmodel.h"
#include "fsentry.h"
#include <QFileInfo>
#include <QDir>

CurrentFileListModel::CurrentFileListModel(QObject *parent)
	: FileMenuBasicListModel(parent, new FSBMCurrentFile(parent))
{
	_fsbmCurrentFile = static_cast<FSBMCurrentFile*>(_model);
	_fsbmCurrentFile->refresh();
	
	connect(this, SIGNAL(syncFile(FileEvent *)), parent, SLOT(syncFile(FileEvent *)));
	std::cout << "CurrentFileListModel is SIGNAL SLOTTING WITH Macros with it's parent... that isnt safe you know." << std::endl;
}

FSBMCurrentFile *CurrentFileListModel::getCurrentFileFSBModel()
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
