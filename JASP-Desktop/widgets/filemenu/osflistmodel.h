#ifndef OSFLISTMODEL_H
#define OSFLISTMODEL_H

#include "basiclistmodel.h"
#include "fsbmosf.h"
#include "osfbreadcrumbslistmodel.h"
#include "filemenulistitem.h"

class OSFListModel : public FileMenuBasicListModel
{
	Q_OBJECT
	
public:
	explicit OSFListModel(QObject *parent, FSBMOSF * fsbMod, OSFBreadCrumbsListModel * crummyList);
		

	void setFSBModel(FSBMOSF *model);
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
	FSBMOSF					*_fsbmOSF;
	OSFBreadCrumbsListModel	*_osfBreadCrumbsListModel;
};

#endif // OSFLISTMODEL_H
