#include "datasettableproxy.h"

DataSetTableProxy::DataSetTableProxy(DataSetPackage * package, parIdxType proxyType)
	: QSortFilterProxyModel(package),
	  _package(package),
	  _proxyType(proxyType)
{
	setSourceModel(_package);

	connect(_package, &DataSetPackage::columnsRemoved,	this, &DataSetTableProxy::columnsWereRemoved);
	connect(_package, &DataSetPackage::modelReset,		this, &DataSetTableProxy::modelWasReset		);
}

QModelIndex	DataSetTableProxy::mapToSource(const QModelIndex &proxyIndex)	const
{
	QModelIndex semiSource = QSortFilterProxyModel::mapToSource(proxyIndex);

	if(!semiSource.isValid())
		return _package->parentModelForType(_proxyType, _proxyParentColumn);

	return _package->index(semiSource.row(), semiSource.column(), _package->parentModelForType(_proxyType, _proxyParentColumn));
}

QModelIndex	DataSetTableProxy::mapFromSource(const QModelIndex &sourceIndex) const
{
	QModelIndex semiProxy = QSortFilterProxyModel::mapFromSource(sourceIndex);

	QModelIndex sourceParent = sourceIndex.parent();

	if(_package->parentIndexTypeIs(sourceIndex) == _proxyType || _package->parentIndexTypeIs(sourceParent) != _proxyType || semiProxy.column() != _proxyParentColumn)
		return QModelIndex();

	return index(semiProxy.row(), semiProxy.column(), QModelIndex());
}

void DataSetTableProxy::setProxyParentColumn(int proxyParentColumn)
{
	if (_proxyParentColumn == proxyParentColumn)
		return;

	beginResetModel();
	_proxyParentColumn = proxyParentColumn;
	emit proxyParentColumnChanged();
	endResetModel();
}


void DataSetTableProxy::modelWasReset()
{
	if(_proxyParentColumn >= _package->columnCount())
		setProxyParentColumn(std::max(0, _package->columnCount() - 1));
}
