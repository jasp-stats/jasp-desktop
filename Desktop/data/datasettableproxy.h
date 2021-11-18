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
	explicit				DataSetTableProxy(DataSetPackageSubNodeModel * subNodeModel);

	int						proxyParentColumn()	const { return _subNodeModel ? _subNodeModel->proxyParentColumn() : 0; } //might not be set yet at some annoying points during init



signals:
	void setProxyParentColumn(int proxyParentColumn);
	void proxyParentColumnChanged();

private:
	DataSetPackageSubNodeModel * _subNodeModel = nullptr;
};

#endif // DATASETTABLEPROXY_H
