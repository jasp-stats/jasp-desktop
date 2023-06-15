#include "labelmodel.h"
#include "log.h"
#include "jasptheme.h"
#include "utilities/qutils.h"

LabelModel::LabelModel() : DataSetTableProxy(DataSetPackage::pkg()->labelsSubModel())
{
	connect(DataSetPackage::pkg(),	&DataSetPackage::filteredOutChanged,			this, &LabelModel::filteredOutChangedHandler);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &LabelModel::filteredOutChanged		);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &LabelModel::columnNameChanged		);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &LabelModel::columnTitleChanged		);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &LabelModel::columnDescriptionChanged	);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &LabelModel::chosenColumnChanged		);
	connect(this,					&LabelModel::chosenColumnChanged,				this, &LabelModel::onChosenColumnChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,					this, &LabelModel::columnNameChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::allFiltersReset,				this, &LabelModel::allFiltersReset			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelFilterChanged,			this, &LabelModel::labelFilterChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnDataTypeChanged,			this, &LabelModel::columnDataTypeChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelsReordered,				this, &LabelModel::refresh					);
}

bool LabelModel::labelNeedsFilter(size_t col)
{
	QVariant result = DataSetPackage::pkg()->headerData(col, Qt::Orientation::Horizontal, int(DataSetPackage::specialRoles::labelsHasFilter));

	return result.type() == QVariant::Bool && result.toBool();
}

std::string LabelModel::columnName(size_t col)
{
	if(	!	node() 
		||	DataSetPackage::pkg()->columnCount(DataSetPackage::pkg()->indexForSubNode(node()->parent())) <= int(col))
		return "";

	return DataSetPackage::pkg()->getColumnName(col);
}

QString LabelModel::columnNameQ()
{
	return QString::fromStdString(column() ? column()->name() : "");
}


void LabelModel::setColumnNameQ(QString newColumnName)
{
	if(column())
		return DataSetPackage::pkg()->setColumnName(DataSetPackage::pkg()->getColumnIndex(column()->name()), fq(newColumnName)); //use DataSetPackage to make sure signals are sent!
}


QString LabelModel::columnTitle() const
{
	return QString::fromStdString(column() ? column()->title() : "");
}

void LabelModel::setColumnTitle(const QString & newColumnTitle)
{
	if(column())
		return DataSetPackage::pkg()->setColumnTitle(DataSetPackage::pkg()->getColumnIndex(column()->name()), fq(newColumnTitle)); //use DataSetPackage to make sure signals are sent!
}

QString LabelModel::columnDescription() const
{
	return QString::fromStdString(column() ? column()->description() : "");
}

void LabelModel::setColumnDescription(const QString & newColumnDescription)
{
	if(column())
		return DataSetPackage::pkg()->setColumnDescription(chosenColumn(), fq(newColumnDescription)); //use DataSetPackage to make sure signals are sent!
}


std::vector<bool> LabelModel::filterAllows(size_t col)
{
	DataSetPackage *	pkg = DataSetPackage::pkg();
	QModelIndex			p	= pkg->indexForSubNode(node());
	boolvec				allows(pkg->rowCount(p));

	for(int row=0; row<pkg->rowCount(p); row++)
		allows[row] = pkg->data(pkg->index(row, 0, p), int(DataSetPackage::specialRoles::filter)).toBool();

	return allows;
}

std::vector<std::string> LabelModel::labels(size_t col)
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->indexForSubNode(node());
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
	QModelIndex					p	= pkg->indexForSubNode(node());

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
	QModelIndex					p	= pkg->indexForSubNode(node());

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
	DataSetPackage::pkg()->labelMoveRows(chosenColumn(), getSortedSelection(), true); //through DataSetPackage to make sure signals get sent
}

void LabelModel::moveSelectionDown()
{
	_lastSelected = -1;
	DataSetPackage::pkg()->labelMoveRows(chosenColumn(), getSortedSelection(), false); //through DataSetPackage to make sure signals get sent
}

void LabelModel::reverse()
{
	_lastSelected = -1;
	DataSetPackage::pkg()->labelReverse(chosenColumn()); //through DataSetPackage to make sure signals get sent
}

bool LabelModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
	if(role == int(DataSetPackage::specialRoles::selected))
		return false;

	return DataSetTableProxy::setData(index, value, role != -1 ? role : int(DataSetPackage::specialRoles::label));
}

QVariant LabelModel::data(	const QModelIndex & index, int role) const
{
	if(role == int(DataSetPackage::specialRoles::selected))
	{
		bool s = _selected.count(data(index, int(DataSetPackage::specialRoles::value)).toString()) > 0;
		return s;
	}

	return DataSetTableProxy::data(index, role > 0 ? role : int(DataSetPackage::specialRoles::label));
}

void LabelModel::filteredOutChangedHandler(int c)
{
	if(c == chosenColumn())
		emit filteredOutChanged();
}

int LabelModel::filteredOut() const
{
	return DataSetPackage::pkg()->filteredOut(chosenColumn());
}

void LabelModel::resetFilterAllows()
{
	DataSetPackage::pkg()->resetFilterAllows(chosenColumn());
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

Column * LabelModel::column() const 
{
	return static_cast<Column *>(node());
}

int LabelModel::chosenColumn() const
{
	Column * c = column();
	
	if(!c)
		return -1;
	
	return c->data()->columnIndex(c);
}

void LabelModel::setChosenColumn(int chosenColumn)
{
	//This only works as long as we have a single dataSet but lets not go overboard with rewriting stuff atm
	DataSet * data = DataSetPackage::pkg()->dataSet();
	
	subNodeModel()->selectNode(data ? data->column(chosenColumn) : nullptr);
}


void LabelModel::columnDataTypeChanged(const QString & colName)
{
	int colIndex = DataSetPackage::pkg()->getColumnIndex(colName);

	if(colIndex == chosenColumn())
	{
		if(DataSetPackage::pkg()->dataSet()->column(colIndex)->type() == columnType::scale)
			setChosenColumn(-1);
		
		invalidate();
	}
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
	if(column() && column()->type() == columnType::scale)
		setChosenColumn(-1);
	
	beginResetModel();
	endResetModel();

	setValueMaxWidth();
	setLabelMaxWidth();
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
