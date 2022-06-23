#include "datasetpackagesubnodemodel.h"
#include "datasetpackage.h"
#include "log.h"

DataSetPackageSubNodeModel::DataSetPackageSubNodeModel(DataSetBaseNode * node)
	: QIdentityProxyModel(DataSetPackage::pkg()), _node(node)
{
	beginResetModel();
	setSourceModel(DataSetPackage::pkg());
	endResetModel();

	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,			this,	&DataSetPackageSubNodeModel::modelWasReset);
}


QModelIndex	DataSetPackageSubNodeModel::mapToSource(const QModelIndex & proxyIndex)	const
{
	if(!_node)
		return QModelIndex();
	
	QModelIndex sourceParent = DataSetPackage::pkg()->indexForSubNode(_node);

	if(!proxyIndex.isValid())
		return sourceParent;

	return DataSetPackage::pkg()->index(proxyIndex.row(), proxyIndex.column(), sourceParent);
}

QModelIndex	DataSetPackageSubNodeModel::mapFromSource(const QModelIndex &sourceIndex) const
{
	if(!_node)
		return QModelIndex();
	
	QModelIndex sourceParentGiven = sourceIndex.parent(),
				sourceParentKnown = DataSetPackage::pkg()->indexForSubNode(_node);

	if(	sourceParentGiven != sourceParentKnown)
		return QModelIndex();

	return createIndex(sourceIndex.row(), sourceIndex.column(), nullptr);
}


void DataSetPackageSubNodeModel::modelWasReset()
{
	//The model was reset, which means _node might no longer exist!
	if(!DataSetPackage::pkg()->dataSetBaseNodeStillExists(_node))
		selectNode(nullptr);
	
}

void DataSetPackageSubNodeModel::selectNode(DataSetBaseNode * node)
{
	if (_node == node)
		return;

	beginResetModel();
	_node = node;
	emit nodeChanged();
	endResetModel();
}
