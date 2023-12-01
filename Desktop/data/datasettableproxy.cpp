#include "datasettableproxy.h"
#include "log.h"

DataSetTableProxy::DataSetTableProxy(DataSetPackageSubNodeModel * subNodeModel) : QSortFilterProxyModel(subNodeModel)
{
	setSourceModel(subNodeModel);

	connect(subNodeModel,			&DataSetPackageSubNodeModel::nodeChanged,	this,			&DataSetTableProxy::nodeChanged		);
}


QModelIndex DataSetTableProxy::mapToSource(const QModelIndex & proxyIndex) const
{
	if(proxyIndex.isValid() && proxyIndex.model() != this)
		Log::log() << "Wrong index!" << std::endl;

	return QSortFilterProxyModel::mapToSource(proxyIndex);
}

QModelIndex DataSetTableProxy::mapFromSource(const QModelIndex & sourceIndex) const
{
	if(sourceIndex.isValid() && sourceIndex.model() != sourceModel())
		Log::log() << "Wrong index!" << std::endl;

	return QSortFilterProxyModel::mapFromSource(sourceIndex);
}
