#ifndef DATALIBRARYLISTMODEL_H
#define DATALIBRARYLISTMODEL_H

#include <QAbstractListModel>
#include "datalibraryfilesystem.h"
#include "datalibrarybreadcrumbsmodel.h"
#include "data/fileevent.h"
#include "filemenulistitem.h"
#include "filemenubasiclistmodel.h"

class DataLibraryListModel : public FileMenuBasicListModel
{
	Q_OBJECT

public:
	explicit DataLibraryListModel(QObject *parent, DataLibraryBreadCrumbsListModel* crumbs);	
	void setBreadCrumbsListModel (DataLibraryBreadCrumbsListModel *dataLibraryBreadCrumbsModel);
	void refresh();

signals:
	void openFileEvent(FileEvent *event);

public slots:
	void changePath(const QString& name, const QString& path)	override;
	void changePathCrumbIndex(const int& index)					override;
	void openFile(const QString& path)							override;

private:
	DataLibraryFileSystem			*	_fsbmDataLibrary					= nullptr;
	DataLibraryBreadCrumbsListModel *	_dataLibraryBreadCrumbsListModel	= nullptr;

};

#endif // DATALIBRARYLISTMODEL_H
