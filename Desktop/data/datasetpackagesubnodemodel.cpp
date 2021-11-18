#include "datasetpackagesubnodemodel.h"
#include "datasetpackage.h"

DataSetPackageSubNodeModel::DataSetPackageSubNodeModel(parIdxType proxyType, int proxyParentColumn)
	: QIdentityProxyModel(DataSetPackage::pkg()), _proxyType(proxyType), _proxyParentColumn(proxyParentColumn)
{
	beginResetModel();
	setSourceModel(DataSetPackage::pkg());
	endResetModel();

	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,	this,	&DataSetPackageSubNodeModel::modelWasReset);
}


QModelIndex	DataSetPackageSubNodeModel::mapToSource(const QModelIndex & proxyIndex)	const
{
	QModelIndex possibleParent = DataSetPackage::pkg()->rootModelIndexForType(_proxyType, _proxyParentColumn);

	if(!proxyIndex.isValid())
		return possibleParent;

	return DataSetPackage::pkg()->index(proxyIndex.row(), proxyIndex.column(), possibleParent);
}

QModelIndex	DataSetPackageSubNodeModel::mapFromSource(const QModelIndex &sourceIndex) const
{
	QModelIndex sourceParent = sourceIndex.parent();

	if(	DataSetPackage::pkg()->parIdxTypeIs(sourceIndex)	== DataSetPackage::parIdxTypeChildForParent(_proxyType)	||
		DataSetPackage::pkg()->parIdxTypeIs(sourceParent)	!= _proxyType											||
		sourceIndex.column()								!= _proxyParentColumn
	)
		return QModelIndex();

	return index(sourceIndex.row(), sourceIndex.column(), QModelIndex());
}

/*
int DataSetPackageSubNodeModel::rowCount(const QModelIndex & parent) const
{
	return sourceModel()->rowCount(mapToSource(parent));
}

int DataSetPackageSubNodeModel::columnCount(const QModelIndex &parent) const
{
	return sourceModel()->columnCount(mapToSource(parent));
}

QModelIndex DataSetPackageSubNodeModel::parent(const QModelIndex &index) const
{
	return mapFromSource(sourceModel()->parent(mapToSource(index)));
}

QModelIndex DataSetPackageSubNodeModel::index(int row, int column, const QModelIndex & parent) const
{
	return mapFromSource(sourceModel()->index(row, column, mapToSource(parent)));
}*/

int DataSetPackageSubNodeModel::proxyParentColumn() const
{
	return _proxyParentColumn;
}

void DataSetPackageSubNodeModel::setProxyParentColumn(int newProxyParentColumn)
{
	if (_proxyParentColumn == newProxyParentColumn)
		return;

	beginResetModel();
	_proxyParentColumn = newProxyParentColumn;
	emit proxyParentColumnChanged();
	endResetModel();
}

void DataSetPackageSubNodeModel::modelWasReset()
{
	if(_proxyParentColumn >= DataSetPackage::pkg()->columnCount())
		setProxyParentColumn(std::max(0, DataSetPackage::pkg()->columnCount() - 1));
}
