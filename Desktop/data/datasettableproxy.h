#ifndef DATASETTABLEPROXY_H
#define DATASETTABLEPROXY_H

#include <QSortFilterProxyModel>
#include "datasetpackage.h"
#include "datasetpackagesubnodemodel.h"

///
/// Makes sure that only a desired subnode of DataSetPackage is passed through
/// It can also be used to filter out rows or columns and even though rearrange them (in potential that is)
class DataSetTableProxy : public QSortFilterProxyModel
{
	Q_OBJECT
	Q_PROPERTY(int proxyParentColumn READ proxyParentColumn WRITE setProxyParentColumn NOTIFY proxyParentColumnChanged)

public:
	explicit	DataSetTableProxy(DataSetPackageSubNodeModel * subNodeModel);
	
	int			proxyParentColumn()							const	{ return	subNodeModel()->proxyParentColumn();						}
	void		setProxyParentColumn(int proxyParentColumn)			{			subNodeModel()->setProxyParentColumn(proxyParentColumn);	}
	
protected:
	DataSetPackageSubNodeModel	*	subNodeModel() const { return qobject_cast<DataSetPackageSubNodeModel*>(sourceModel()); }

signals:
	void		proxyParentColumnChanged();
};

#endif // DATASETTABLEPROXY_H
