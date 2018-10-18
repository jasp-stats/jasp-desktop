#ifndef OSFLISTMODEL_H
#define OSFLISTMODEL_H

#include <QAbstractListModel>
#include "fsbmosf.h"
#include "osfbreadcrumbslistmodel.h"

class OSFListModel : public QAbstractListModel
{
	Q_OBJECT
	
public:
	explicit OSFListModel(QObject *parent = nullptr);
		
	enum
	{
		NameRole = Qt::UserRole,
		PathRole,
		DescriptionRole,
		TypeRole,
		IconSourceRole,
		DirRole
	};
		
	// Basic functionality:
	int rowCount(const QModelIndex &parent = QModelIndex()) const override;	
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;	
	virtual QHash<int, QByteArray> roleNames() const override;
			
	void setFSBModel(FSBMOSF *model);
	void setBreadCrumbsListModel (OSFBreadCrumbsListModel *osfBreadCrumbsModel);
	void reload();
	
public slots:
	void changePath(const QString& name, const QString& path);
	void changePath(const int& index);
	
signals:
	void startProcessing();
		
private:
	FSBMOSF *_fsbmOSF;
	OSFBreadCrumbsListModel *_osfBreadCrumbsListModel;
	QHash<int, QString> _iconsources;
};

#endif // OSFLISTMODEL_H
