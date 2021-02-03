#include "labelmodel.h"
#include "log.h"


LabelModel::LabelModel() : DataSetTableProxy(parIdxType::label)
{
	connect(DataSetPackage::pkg(),	&DataSetPackage::filteredOutChanged,			this, &LabelModel::filteredOutChangedHandler);
	connect(this,					&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::filteredOutChanged		);
	connect(this,					&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::columnNameChanged		);
	connect(this,					&DataSetTableProxy::proxyParentColumnChanged,	this, &LabelModel::onChosenColumnChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,					this, &LabelModel::columnNameChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::allFiltersReset,				this, &LabelModel::allFiltersReset			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelFilterChanged,			this, &LabelModel::labelFilterChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnAboutToBeRemoved,		this, &LabelModel::columnAboutToBeRemoved	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnDataTypeChanged,			this, &LabelModel::columnDataTypeChanged	);
}

bool LabelModel::labelNeedsFilter(size_t col)
{
	QVariant result = DataSetPackage::pkg()->headerData(col, Qt::Orientation::Horizontal, int(DataSetPackage::specialRoles::labelsHasFilter));

	if(result.type() == QVariant::Bool)	return result.toBool();
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

	for(int row=0; row<pkg->rowCount(p); row++)
		allows[row] = pkg->data(pkg->index(row, int(Column::Filter), p), int(DataSetPackage::specialRoles::filter)).toBool();

	return allows;
}

std::vector<std::string> LabelModel::labels(size_t col)
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->parentModelForType(parIdxType::label, col);
	std::vector<std::string>	labels(pkg->rowCount(p));

	for(int row=0; row<pkg->rowCount(p); row++)
		labels[row] = pkg->data(pkg->index(row, int(Column::Label), p), Qt::DisplayRole).toString().toStdString();

	return labels;
}

std::vector<size_t> LabelModel::getSortedSelection() const
{
	std::map<QString, size_t> mapValueToRow;

	for(size_t r=0; r<size_t(rowCount()); r++)
		mapValueToRow[data(index(r, int(Column::Value)), int(DataSetPackage::specialRoles::value)).toString()] = r;

	std::vector<size_t> out;

	for(const QString & v : _selected)
		out.push_back(mapValueToRow[v]);

	std::sort(out.begin(), out.end());

	return out;
}

void LabelModel::moveSelectionUp()
{
	DataSetPackage::pkg()->labelMoveRows(proxyParentColumn(), getSortedSelection(), true);
}

void LabelModel::moveSelectionDown()
{
	DataSetPackage::pkg()->labelMoveRows(proxyParentColumn(), getSortedSelection(), false);
}

void LabelModel::reverse()
{
	DataSetPackage::pkg()->labelReverse(proxyParentColumn());
}

int	LabelModel::roleFromColumn(Column col) const
{
	switch(col)
	{
	case Column::Filter:	return int(DataSetPackage::specialRoles::filter);
	case Column::Value:		return int(DataSetPackage::specialRoles::value);
	case Column::Label:		return int(DataSetPackage::specialRoles::label);
	default:				return Qt::DisplayRole;
	}
}

bool LabelModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
	if(role == int(DataSetPackage::specialRoles::selected))
		return false;

	return DataSetPackage::pkg()->setData(mapToSource(index), value, role != -1 ? role : roleFromColumn(Column(index.column())));
}

QVariant LabelModel::data(	const QModelIndex & index, int role) const
{
	if(role == int(DataSetPackage::specialRoles::selected))
		return _selected.count(data(index, int(DataSetPackage::specialRoles::value)).toString()) > 0;

	return DataSetPackage::pkg()->data(mapToSource(index), role > 0 ? role : roleFromColumn(Column(index.column())));
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

void LabelModel::columnDataTypeChanged(const QString & colName)
{
	int colIndex = DataSetPackage::pkg()->getColumnIndex(colName);

	if(colIndex == proxyParentColumn())
		invalidate();
}

bool LabelModel::setColumnWidth(int col, float width)
{
	float old = _colWidths[col];

	_colWidths[col] = width;

	bool changed = abs(old - width) > 0.001;

	if(changed)
		emit headerDataChanged(Qt::Horizontal, col, col);

	return changed;
}


///Override of headerData because it doesnt get QModelIndex and thus cannot know whether it is proxied by labelmodel or something else...
QVariant LabelModel::headerData(int section, Qt::Orientation orientation, int role)	const
{
	if (section < 0 || section >= (orientation == Qt::Horizontal ? columnCount() : rowCount()))
		return QVariant();

	QStringList headers = { tr("Filter"), tr("Value"), tr("Label"), tr("Select") }; //We look it up every time to allow for translations. It adds overhead but probably negligible.

	switch(role)
	{
	case int(DataSetPackage::specialRoles::columnWidthFallback):	return _colWidths[section];
	case int(DataSetPackage::specialRoles::maxRowHeaderString):		return "";
	case Qt::DisplayRole:											return orientation == Qt::Horizontal ? headers[section] : QVariant(section);
	case Qt::TextAlignmentRole:										return QVariant(Qt::AlignCenter);
	}

	return QVariant();
}

void LabelModel::onChosenColumnChanged()
{
	_selected.clear();
	//dataChanged probably not needed 'cause we are in a reset
}

void LabelModel::toggleSelected(int row, bool unselectRest)
{
	QString rowValue = data(index(row, 0), int(DataSetPackage::specialRoles::value)).toString();

	bool disableCurrent = _selected.count(rowValue) > 0;

	if(unselectRest)
	{
		beginResetModel();
		_selected.clear();
	}

	if(!disableCurrent)	_selected.insert(rowValue);
	else				_selected.erase(rowValue);

	if(unselectRest)	endResetModel();
	else				emit dataChanged(LabelModel::index(row, 0), LabelModel::index(row, int(Column::Label)), {int(DataSetPackage::specialRoles::selected)});
}

void LabelModel::setSelectedOnRow(int row, bool selected)
{
	QString rowValue = data(index(row, 0), int(DataSetPackage::specialRoles::value)).toString();

	if(selected == (_selected.count(rowValue) > 0))
		return;

	if(selected)	_selected.insert(rowValue);
	else			_selected.erase(rowValue);

	emit dataChanged(LabelModel::index(row, 0), LabelModel::index(row, int(Column::Label)), {int(DataSetPackage::specialRoles::selected)});
}


void LabelModel::unselectAll()
{
	_selected.clear();
	emit dataChanged(LabelModel::index(0, 0), LabelModel::index(rowCount(), int(Column::Label)), {int(DataSetPackage::specialRoles::selected)});
}
