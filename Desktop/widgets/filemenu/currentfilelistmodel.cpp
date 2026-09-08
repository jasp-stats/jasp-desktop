#include "currentfilelistmodel.h"
#include <QFileInfo>
#include <QDir>

CurrentFileListModel::CurrentFileListModel(FileMenuObject *parent)
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
