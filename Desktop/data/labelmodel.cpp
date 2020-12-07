#include "labelmodel.h"

LabelModel::LabelModel() : DataSetTableProxy(parIdxType::label)
{
	connect(DataSetPackage::pkg(),	&DataSetPackage::filteredOutChanged,			this, &LabelModel::filteredOutChangedHandler);
	connect(this,					&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::filteredOutChanged		);
	connect(this,					&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::columnNameChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,					this, &LabelModel::columnNameChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::allFiltersReset,				this, &LabelModel::allFiltersReset			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelFilterChanged,			this, &LabelModel::labelFilterChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnAboutToBeRemoved,		this, &LabelModel::columnAboutToBeRemoved	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnDataTypeChanged,			this, &LabelModel::columnDataTypeChanged	);
}

bool LabelModel::labelNeedsFilter(size_t col)
{
	QVariant result = DataSetPackage::pkg()->headerData(col, Qt::Orientation::Horizontal, int(DataSetPackage::specialRoles::labelsHasFilter));

	if(result.type() == QMetaType::Bool)	return result.toBool();
	return false;
}

std::string LabelModel::columnName(size_t col)
{
	if(DataSetPackage::pkg()->columnCount() <= int(col))
		return "";

	return DataSetPackage::pkg()->getColumnName(col);
}

std::vector<bool> LabelModel::filterAllows(size_t col)
{
	DataSetPackage *	pkg = DataSetPackage::pkg();
	QModelIndex			p	= pkg->parentModelForType(parIdxType::label, col);
	std::vector<bool>	allows(pkg->rowCount(p));

	for(size_t row=0; row<pkg->rowCount(p); row++)
		allows[row] = pkg->data(pkg->index(row, 0, p), int(DataSetPackage::specialRoles::filter)).toBool();

	return allows;
}

std::vector<std::string> LabelModel::labels(size_t col)
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->parentModelForType(parIdxType::label, col);
	std::vector<std::string>	labels(pkg->rowCount(p));

	for(size_t row=0; row<pkg->rowCount(p); row++)
		labels[row] = pkg->data(pkg->index(row, 0, p), Qt::DisplayRole).toString().toStdString();

	return labels;
}


void LabelModel::moveUp(std::vector<size_t> selection)
{
	DataSetPackage::pkg()->labelMoveRows(proxyParentColumn(), selection, true);
}

void LabelModel::moveDown(std::vector<size_t> selection)
{
	DataSetPackage::pkg()->labelMoveRows(proxyParentColumn(), selection, false);
}

void LabelModel::reverse()
{
	DataSetPackage::pkg()->labelReverse(proxyParentColumn());
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
	return DataSetPackage::pkg()->setData(mapToSource(index), value, roleToSet);
}

void LabelModel::filteredOutChangedHandler(int c)
{
	if(c == proxyParentColumn()) emit filteredOutChanged();
}

int LabelModel::filteredOut() const
{
	return DataSetPackage::pkg()->filteredOut(proxyParentColumn());
}

void LabelModel::resetFilterAllows()
{
	DataSetPackage::pkg()->resetFilterAllows(proxyParentColumn());
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
	return DataSetPackage::pkg()->dataColumnCount();
}

void LabelModel::columnAboutToBeRemoved(int column)
{
	if(proxyParentColumn() == column)
		setVisible(false);
}

void LabelModel::columnDataTypeChanged(QString colName)
{
	int colIndex = DataSetPackage::pkg()->getColumnIndex(colName);

	if(colIndex == proxyParentColumn())
		invalidate();
}
