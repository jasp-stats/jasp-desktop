#include "labelmodel.h"
#include "log.h"
#include "qquick/jasptheme.h"

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
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelsReordered,				this, &LabelModel::refresh					);
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
		allows[row] = pkg->data(pkg->index(row, 0, p), int(DataSetPackage::specialRoles::filter)).toBool();

	return allows;
}

std::vector<std::string> LabelModel::labels(size_t col)
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->parentModelForType(parIdxType::label, col);
	std::vector<std::string>	labels(pkg->rowCount(p));

	for(int row=0; row<pkg->rowCount(p); row++)
		labels[row] = pkg->data(pkg->index(row, 0, p), Qt::DisplayRole).toString().toStdString();

	return labels;
}

std::vector<size_t> LabelModel::getSortedSelection() const
{
	std::map<QString, size_t> mapValueToRow;

	for(size_t r=0; r<size_t(rowCount()); r++)
		mapValueToRow[data(index(r, 0), int(DataSetPackage::specialRoles::value)).toString()] = r;

	std::vector<size_t> out;

	for(const QString & v : _selected)
		out.push_back(mapValueToRow[v]);

	std::sort(out.begin(), out.end());

	return out;
}

void LabelModel::setValueMaxWidth()
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->parentModelForType(parIdxType::label, proxyParentColumn());

	double max = JaspTheme::fontMetrics().size(Qt::TextSingleLine, tr("Value")).width();

	for (int row=0; row<pkg->rowCount(p); row++)
	{
		QString value = pkg->data(pkg->index(row, 0, p), int(DataSetPackage::specialRoles::value)).toString();
		max = std::max(max, JaspTheme::fontMetrics().size(Qt::TextSingleLine, value).width());
	}

	_valueMaxWidth = max;

	emit valueMaxWidthChanged();
}

void LabelModel::setLabelMaxWidth()
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->parentModelForType(parIdxType::label, proxyParentColumn());

	double max = JaspTheme::fontMetrics().size(Qt::TextSingleLine, tr("Label")).width();

	for (int row=0; row<pkg->rowCount(p); row++)
	{
		QString label = pkg->data(pkg->index(row, 0, p)).toString();
		max = std::max(max, JaspTheme::fontMetrics().size(Qt::TextSingleLine, label).width());
	}

	_labelMaxWidth = max;

	emit labelMaxWidthChanged();
}

void LabelModel::moveSelectionUp()
{
	_lastSelected = -1;
	DataSetPackage::pkg()->labelMoveRows(proxyParentColumn(), getSortedSelection(), true);
}

void LabelModel::moveSelectionDown()
{
	_lastSelected = -1;
	DataSetPackage::pkg()->labelMoveRows(proxyParentColumn(), getSortedSelection(), false);
}

void LabelModel::reverse()
{
	_lastSelected = -1;
	DataSetPackage::pkg()->labelReverse(proxyParentColumn());
}

bool LabelModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
	if(role == int(DataSetPackage::specialRoles::selected))
		return false;

	return DataSetPackage::pkg()->setData(mapToSource(index), value, role != -1 ? role : int(DataSetPackage::specialRoles::label));
}

QVariant LabelModel::data(	const QModelIndex & index, int role) const
{
	if(role == int(DataSetPackage::specialRoles::selected))
	{
		bool s = _selected.count(data(index, int(DataSetPackage::specialRoles::value)).toString()) > 0;
		return s;
	}

	return DataSetPackage::pkg()->data(mapToSource(index), role > 0 ? role : int(DataSetPackage::specialRoles::label));
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

void LabelModel::setRowWidth(double len)
{
	if (abs(len - _rowWidth) > 0.001)
	{
		_rowWidth = len;

		emit headerDataChanged(Qt::Horizontal, 0, 0);
	}
}

///Override of headerData because it doesnt get QModelIndex and thus cannot know whether it is proxied by labelmodel or something else...
QVariant LabelModel::headerData(int section, Qt::Orientation orientation, int role)	const
{
	if (section < 0 || section >= (orientation == Qt::Horizontal ? columnCount() : rowCount()))
		return QVariant();

	switch(role)
	{
	case int(DataSetPackage::specialRoles::columnWidthFallback):	return _rowWidth;
	case int(DataSetPackage::specialRoles::maxRowHeaderString):		return "";
	case Qt::DisplayRole:											return QVariant(section);
	case Qt::TextAlignmentRole:										return QVariant(Qt::AlignCenter);
	}

	return QVariant();
}

void LabelModel::onChosenColumnChanged()
{
	_selected.clear();
	_lastSelected = -1;
	setValueMaxWidth();
	setLabelMaxWidth();
	//dataChanged probably not needed 'cause we are in a reset
}

void LabelModel::refresh()
{
	beginResetModel();
	endResetModel();
}

void LabelModel::removeAllSelected()
{
	QMap<QString, size_t> mapValueToRow;

	for(size_t r=0; r<size_t(rowCount()); r++)
		mapValueToRow[data(index(r, 0), int(DataSetPackage::specialRoles::value)).toString()] = r;

	QVector<QString> selectedValues;
	for (const QString& s : _selected)
		selectedValues.append(s);

	_selected.clear();
	_lastSelected = -1;
	for (const QString& selectedValue : selectedValues)
	{
		if (mapValueToRow.contains(selectedValue))
		{
			int selectedRow = int(mapValueToRow[selectedValue]);
			emit dataChanged(LabelModel::index(selectedRow, 0), LabelModel::index(selectedRow, 0), {int(DataSetPackage::specialRoles::selected)});
		}
	}
}

void LabelModel::setSelected(int row, int modifier)
{
	if (modifier & Qt::ShiftModifier && _lastSelected >= 0)
	{
		int start = _lastSelected >= row ? row : _lastSelected;
		int end = start == _lastSelected ? row : _lastSelected;
		for (int i = start; i <= end; i++)
		{
			QString rowValue = data(index(i, 0), int(DataSetPackage::specialRoles::value)).toString();
			_selected.insert(rowValue);
			emit dataChanged(LabelModel::index(i, 0), LabelModel::index(i, 0), {int(DataSetPackage::specialRoles::selected)});
		}
	}
	else if (modifier & Qt::ControlModifier)
	{
		QString rowValue = data(index(row, 0), int(DataSetPackage::specialRoles::value)).toString();
		_selected.insert(rowValue);
		emit dataChanged(LabelModel::index(row, 0), LabelModel::index(row, 0), {int(DataSetPackage::specialRoles::selected)});
	}
	else
	{
		QString rowValue = data(index(row, 0), int(DataSetPackage::specialRoles::value)).toString();
		bool disableCurrent = _selected.count(rowValue) > 0;
		removeAllSelected();
		if (!disableCurrent)	_selected.insert(rowValue);
		else					_selected.erase(rowValue);
		emit dataChanged(LabelModel::index(row, 0), LabelModel::index(row, 0), {int(DataSetPackage::specialRoles::selected)});
	}
	_lastSelected = row;

}


void LabelModel::unselectAll()
{
	_selected.clear();
	_lastSelected = -1;
	emit dataChanged(LabelModel::index(0, 0), LabelModel::index(rowCount(), 0), {int(DataSetPackage::specialRoles::selected)});
}

bool LabelModel::setChecked(int rowIndex, bool checked)
{
	return setData(LabelModel::index(rowIndex, 0), checked, int(DataSetPackage::specialRoles::filter));
}

void LabelModel::setLabel(int rowIndex, QString label)
{
	setData(LabelModel::index(rowIndex, 0), label);
	setLabelMaxWidth();
}
