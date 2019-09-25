#include "labelmodel.h"

LabelModel::LabelModel(DataSetPackage * package) : DataSetTableProxy(package, parIdxType::label)
{
	connect(_package,	&DataSetPackage::filteredOutChanged,			this, &LabelModel::filteredOutChangedHandler);
	connect(this,		&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::filteredOutChanged		);
	connect(this,		&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::columnNameChanged		);
	connect(_package,	&DataSetPackage::modelReset,					this, &LabelModel::columnNameChanged		);
	connect(_package,	&DataSetPackage::allFiltersReset,				this, &LabelModel::allFiltersReset			);
	connect(_package,	&DataSetPackage::labelFilterChanged,			this, &LabelModel::labelFilterChanged		);
}

bool LabelModel::labelNeedsFilter(size_t col)
{
	QVariant result = _package->headerData(col, Qt::Orientation::Horizontal, int(DataSetPackage::specialRoles::labelsHasFilter));

	if(result.type() == QMetaType::Bool)	return result.toBool();
	return false;
}

std::vector<bool> LabelModel::filterAllows(size_t col)
{
	QModelIndex p = _package->parentModelForType(parIdxType::label, col);
	std::vector<bool> allows(_package->rowCount(p));

	for(size_t row=0; row<_package->rowCount(p); row++)
		allows[row] = _package->data(_package->index(row, 0, p), int(DataSetPackage::specialRoles::filter)).toBool();

	return allows;
}

std::vector<std::string> LabelModel::labels(size_t col)
{
	QModelIndex p = _package->parentModelForType(parIdxType::label, col);
	std::vector<std::string> labels(_package->rowCount(p));

	for(size_t row=0; row<_package->rowCount(p); row++)
		labels[row] = _package->data(_package->index(row, 0, p), Qt::DisplayRole).toString().toStdString();

	return labels;
}


void LabelModel::moveUp(std::vector<size_t> selection)
{
	_package->labelMoveRows(proxyParentColumn(), selection, true);
}

void LabelModel::moveDown(std::vector<size_t> selection)
{
	_package->labelMoveRows(proxyParentColumn(), selection, false);
}

void LabelModel::reverse()
{
	_package->labelReverse(proxyParentColumn());
}

std::vector<size_t> LabelModel::convertQVariantList_to_RowVec(QVariantList selection)
{
	std::vector<size_t> vec;
	bool Converted;

	for(QVariant variant : selection)
	{
		int Row = variant.toInt(&Converted);
		if(Converted)
			vec.push_back(size_t(Row));
	}

	return vec;

}

bool LabelModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
	int roleToSet = index.column() == 0 ? int(DataSetPackage::specialRoles::filter) : index.column() == 1 ? int(DataSetPackage::specialRoles::value) : Qt::DisplayRole;
	return _package->setData(mapToSource(index), value, roleToSet);
}

void LabelModel::filteredOutChangedHandler(int c)
{
	if(c == proxyParentColumn()) emit filteredOutChanged();
}

int LabelModel::filteredOut() const
{
	return _package->filteredOut(proxyParentColumn());
}

void LabelModel::resetFilterAllows()
{
	_package->resetFilterAllows(proxyParentColumn());
}

void LabelModel::setVisible(bool visible)
{
	visible = visible && rowCount() > 0; //cannot show labels when there are no labels

	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}

int LabelModel::dataColumnCount() const
{
	return _package->dataColumnCount();
}
