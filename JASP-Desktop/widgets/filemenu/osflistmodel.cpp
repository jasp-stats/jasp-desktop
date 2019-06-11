#include "osflistmodel.h"
#include "filesystementry.h"
#include <QFileInfo>
#include <QDir>

OSFListModel::OSFListModel(QObject *parent, OSFFileSystem * fsbMod, OSFBreadCrumbsListModel * crummyList)
	: FileMenuBasicListModel(parent, nullptr)
{

	setFSBModel(fsbMod);
	setBreadCrumbsListModel(crummyList);
}

void OSFListModel::setBreadCrumbsListModel(OSFBreadCrumbsListModel *osfBreadCrumbsListModel)
{
	_osfBreadCrumbsListModel = osfBreadCrumbsListModel;
}

void OSFListModel::setFSBModel(OSFFileSystem *model)
{
	beginResetModel();
	
	_model		= model;
	_fsbmOSF	= model;
	//_fsbmOSF->refresh();
	
	endResetModel();
}

void OSFListModel::reload()
{
	beginResetModel();
	
	//_fsbmOSF->refresh();
	
	endResetModel();
}

void OSFListModel::changePath(const QString &name, const QString &path)
{
	//Called from osflist qml
	emit startProcessing();
	beginResetModel();	
	_osfBreadCrumbsListModel->appendCrumb(name , path);
	
	_fsbmOSF->setPath(path);
	
	endResetModel();
}

void OSFListModel::changePathCrumbIndex(const int &index)
{
	if (_osfBreadCrumbsListModel->rowCount() == index+1)
		return;
	
	//Called from breadcrumbs qml
	emit startProcessing();
	beginResetModel();
	
	QString path;
	
	path = _osfBreadCrumbsListModel->switchCrumb(index);
		
	_fsbmOSF->setPath(path);
	
	endResetModel();
}

