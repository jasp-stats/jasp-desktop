#include "datasettableproxy.h"

DataSetTableProxy::DataSetTableProxy(parIdxType proxyType)
	: QSortFilterProxyModel(DataSetPackage::pkg()),
	  _proxyType(proxyType)
{
	setSourceModel(DataSetPackage::pkg());

	connect(DataSetPackage::pkg(), &DataSetPackage::columnsRemoved,	this, &DataSetTableProxy::columnsWereRemoved);
	connect(DataSetPackage::pkg(), &DataSetPackage::modelReset,		this, &DataSetTableProxy::modelWasReset		);
}

QModelIndex	DataSetTableProxy::mapToSource(const QModelIndex &proxyIndex)	const
{
	QModelIndex semiSource = QSortFilterProxyModel::mapToSource(proxyIndex);

	//if(!semiSource.isValid())
	//	return DataSetPackage::pkg()->parentModelForType(_proxyType, _proxyParentColumn);

	return DataSetPackage::pkg()->index(semiSource.row(), semiSource.column(), DataSetPackage::pkg()->parentModelForType(_proxyType, _proxyParentColumn));
}

QModelIndex	DataSetTableProxy::mapFromSource(const QModelIndex &sourceIndex) const
{
	QModelIndex semiProxy = QSortFilterProxyModel::mapFromSource(sourceIndex);

	QModelIndex sourceParent = sourceIndex.parent();

	/*if(	DataSetPackage::pkg()->parentIndexTypeIs(sourceIndex) == _proxyType		||
		DataSetPackage::pkg()->parentIndexTypeIs(sourceParent) != _proxyType	||
		semiProxy.column() != _proxyParentColumn
	)
		return QModelIndex();*/

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
	if(_proxyParentColumn >= DataSetPackage::pkg()->columnCount())
		setProxyParentColumn(std::max(0, DataSetPackage::pkg()->columnCount() - 1));
}
