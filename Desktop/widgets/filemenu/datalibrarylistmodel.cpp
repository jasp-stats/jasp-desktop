#include "datalibrarylistmodel.h"
#include "filesystementry.h"
#include <QFileInfo>
#include <QDir>
#include "log.h"
#include "datalibrary.h"

DataLibraryListModel::DataLibraryListModel(QObject *parent, DataLibraryBreadCrumbsListModel* crumbs) : FileMenuBasicListModel(parent, new DataLibraryFileSystem(parent,  DataLibraryFileSystem::rootelementname )), _dataLibraryBreadCrumbsListModel(crumbs)
{
	_fsbmDataLibrary = static_cast<DataLibraryFileSystem*>(_model);;
	_fsbmDataLibrary->refresh();

	connect(this, &DataLibraryListModel::openFileEvent, dynamic_cast<DataLibrary *>(parent), &DataLibrary::openFile);
}

void DataLibraryListModel::refresh()
{
	_fsbmDataLibrary->refresh();
	_dataLibraryBreadCrumbsListModel->refresh();

}

void DataLibraryListModel::changePath(const QString &name, const QString &path)
{
	// Called from datalibrarylist
	beginResetModel();
	_dataLibraryBreadCrumbsListModel->appendCrumb(name , path);

	_fsbmDataLibrary->setPath(path);
	_fsbmDataLibrary->refresh();

	endResetModel();
}

void DataLibraryListModel::changePathCrumbIndex(const int &index)
{
	// Called from breadcrumbs
	beginResetModel();

	QString path;

	path = _dataLibraryBreadCrumbsListModel->switchCrumb(index);

	_fsbmDataLibrary->setPath(path);
	_fsbmDataLibrary->refresh();

	endResetModel();

}

void DataLibraryListModel::openFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileOpen);
	event->setPath(path);
	event->setReadOnly();

	emit openFileEvent(event);
	
	changePathCrumbIndex(0);  //Reset begin screen datalibrary (the same as with key navigation).
}
