#include "datasettableproxy.h"

DataSetTableProxy::DataSetTableProxy(DataSetPackageSubNodeModel * subNodeModel) : QSortFilterProxyModel(subNodeModel)
{
	setSourceModel(subNodeModel);

	connect(subNodeModel,			&DataSetPackageSubNodeModel::proxyParentColumnChanged,	this,			&DataSetTableProxy::proxyParentColumnChanged		);
}
