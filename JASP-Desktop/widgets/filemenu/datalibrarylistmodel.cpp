#include "datalibrarylistmodel.h"
#include "filesystementry.h"
#include <QFileInfo>
#include <QDir>

DataLibraryListModel::DataLibraryListModel(QObject *parent, DataLibraryBreadCrumbsListModel* crumbs) : FileMenuBasicListModel(parent, new DataLibraryFileSystem(parent,  DataLibraryFileSystem::rootelementname )), _dataLibraryBreadCrumbsListModel(crumbs)
{
	_fsbmDataLibrary = static_cast<DataLibraryFileSystem*>(_model);;
	_fsbmDataLibrary->refresh();

	connect(this, SIGNAL(openFile(FileEvent *)), parent, SLOT(openFile(FileEvent *)));	//connect(_dataLibraryBreadCrumbsListModel, SIGNAL(indexChanged(const int &)), this, SLOT(changePath(const int &)));
	std::cout << "DataLibraryListModel is connecting to it's parent through the SIGNAL and SLOT Macros! DANGEROUS ;)" << std::endl;
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

	emit openFile(event);
}
