#include "datasettableproxy.h"

DataSetTableProxy::DataSetTableProxy(DataSetPackageSubNodeModel * subNodeModel) : QSortFilterProxyModel(subNodeModel)
{
	setSourceModel(subNodeModel);

	connect(subNodeModel,			&DataSetPackageSubNodeModel::nodeChanged,	this,			&DataSetTableProxy::nodeChanged		);
}
