#ifndef OSFLISTMODEL_H
#define OSFLISTMODEL_H

#include "filemenubasiclistmodel.h"
#include "osffilesystem.h"
#include "osfbreadcrumbslistmodel.h"
#include "filemenulistitem.h"

class OSFListModel : public FileMenuBasicListModel
{
	Q_OBJECT
	
public:
	explicit OSFListModel(QObject *parent, OSFFileSystem * fsbMod, OSFBreadCrumbsListModel * crummyList);
		

	void setFSBModel(OSFFileSystem *model);
	void setBreadCrumbsListModel (OSFBreadCrumbsListModel *osfBreadCrumbsModel);
	void reload();
	
public slots:
	void changePath(const QString& name, const QString& path)	override;
	void changePathCrumbIndex(const int& index)					override;
	void openFile(const QString& path)							override	{ emit openFileRequest(path); }
	
signals:
	void startProcessing();
	void openFileRequest(QString path);
		
private:
	OSFFileSystem		*_fsbmOSF;
	OSFBreadCrumbsListModel	*_osfBreadCrumbsListModel;
};

#endif // OSFLISTMODEL_H
