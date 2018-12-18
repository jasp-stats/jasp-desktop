#include "computerlistmodel.h"
#include "fsentry.h"
#include <QFileInfo>
#include <QDir>

ComputerListModel::ComputerListModel(QObject *parent)
	: FileMenuBasicListModel(parent, new FSBMRecentFolders())
{
	_fsbmRecentFolders = static_cast<FSBMRecentFolders*>(_model);
	_fsbmRecentFolders->refresh();
}


QString ComputerListModel::getMostRecent()
{
	return _fsbmRecentFolders->mostRecent();
}

void ComputerListModel::addRecentFolder(const QString &newpath)
{
	beginResetModel();
	
	_fsbmRecentFolders->addRecent(newpath);
	_fsbmRecentFolders->refresh();
	
	endResetModel();
}

void ComputerListModel::refresh()
{
	beginResetModel();
	
	_fsbmRecentFolders->refresh();
	
	endResetModel();
}

//Slots

