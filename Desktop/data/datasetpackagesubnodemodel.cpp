#include "datasetpackagesubnodemodel.h"
#include "datasetpackage.h"
#include "log.h"

DataSetPackageSubNodeModel::DataSetPackageSubNodeModel(const QString & whatAmI, DataSetBaseNode * node)
	: QIdentityProxyModel(DataSetPackage::pkg()), _node(node), _whatAmI(whatAmI)
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

int DataSetPackageSubNodeModel::rowCount(const QModelIndex & parent) const
{
	int row = !_node ? 0 :DataSetPackage::pkg()->rowCount(mapToSource(parent));
	//Log::log() << "DataSetPackageSubNodeModel("<< _whatAmI.toStdString() << ")::rowCount(" << ( _node ? dataSetBaseNodeTypeToString(_node->nodeType()) : "no node") << ") = " << row << std::endl;
	return row;
}

int DataSetPackageSubNodeModel::columnCount(const QModelIndex & parent) const
{
	int col = !_node ? 0 : DataSetPackage::pkg()->columnCount(mapToSource(parent));
	//Log::log() << "DataSetPackageSubNodeModel("<< _whatAmI.toStdString() << ")::columnCount(" << ( _node ? dataSetBaseNodeTypeToString(_node->nodeType()) : "no node")  << ") = " << col << std::endl;
	return col;
}

QString DataSetPackageSubNodeModel::insertColumnSpecial(int column, bool computed, bool R)
{
	int sourceColumn = column > columnCount() ? columnCount() : column;
	sourceColumn = mapToSource(index(0, sourceColumn)).column();
	return DataSetPackage::pkg()->insertColumnSpecial(sourceColumn == -1 ? sourceModel()->columnCount() : sourceColumn, computed, R);
}

QString DataSetPackageSubNodeModel::appendColumnSpecial(bool computed, bool R)
{
	return DataSetPackage::pkg()->appendColumnSpecial(computed, R);
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
	if(!_node)	setSourceModel(nullptr);
	else		setSourceModel(DataSetPackage::pkg());
	emit nodeChanged();
	endResetModel();
}
