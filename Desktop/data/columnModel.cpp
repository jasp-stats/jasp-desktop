#include "columnModel.h"
#include "log.h"
#include "jasptheme.h"
#include "utilities/qutils.h"

ColumnModel::ColumnModel() : DataSetTableProxy(DataSetPackage::pkg()->labelsSubModel())
{
	connect(DataSetPackage::pkg(),	&DataSetPackage::filteredOutChanged,			this, &ColumnModel::filteredOutChangedHandler);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &ColumnModel::filteredOutChanged		);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &ColumnModel::columnNameChanged		);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &ColumnModel::columnTitleChanged		);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &ColumnModel::columnDescriptionChanged	);
	connect(this,					&DataSetTableProxy::nodeChanged,				this, &ColumnModel::chosenColumnChanged		);
	connect(this,					&ColumnModel::chosenColumnChanged,				this, &ColumnModel::onChosenColumnChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,					this, &ColumnModel::columnNameChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,					this, &ColumnModel::columnTitleChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,					this, &ColumnModel::columnDescriptionChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::allFiltersReset,				this, &ColumnModel::allFiltersReset			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelFilterChanged,			this, &ColumnModel::labelFilterChanged		);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnDataTypeChanged,			this, &ColumnModel::columnDataTypeChanged	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelsReordered,				this, &ColumnModel::refresh					);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnsBeingRemoved,			this, &ColumnModel::checkRemovedColumns		);

	_undoStack = DataSetPackage::pkg()->undoStack();
}

bool ColumnModel::labelNeedsFilter(size_t col)
{
	QVariant result = DataSetPackage::pkg()->headerData(col, Qt::Orientation::Horizontal, int(DataSetPackage::specialRoles::labelsHasFilter));

	return result.type() == QVariant::Bool && result.toBool();
}

std::string ColumnModel::columnName(size_t col)
{
	if(	!	node() 
		||	DataSetPackage::pkg()->columnCount(DataSetPackage::pkg()->indexForSubNode(node()->parent())) <= int(col))
		return "";

	return DataSetPackage::pkg()->getColumnName(col);
}

QString ColumnModel::columnNameQ()
{
	return QString::fromStdString(column() ? column()->name() : "");
}


void ColumnModel::setColumnNameQ(QString newColumnName)
{
	if(column())
		_undoStack->pushCommand(new SetColumnPropertyCommand(this, newColumnName, SetColumnPropertyCommand::ColumnProperty::Name));
}

QString ColumnModel::columnTitle() const
{
	return QString::fromStdString(column() ? column()->title() : "");
}

void ColumnModel::setColumnTitle(const QString & newColumnTitle)
{
	if(column())
		_undoStack->pushCommand(new SetColumnPropertyCommand(this, newColumnTitle, SetColumnPropertyCommand::ColumnProperty::Title));
}

QString ColumnModel::columnDescription() const
{
	return QString::fromStdString(column() ? column()->description() : "");
}

void ColumnModel::setColumnDescription(const QString & newColumnDescription)
{
	if(column())
		_undoStack->pushCommand(new SetColumnPropertyCommand(this, newColumnDescription, SetColumnPropertyCommand::ColumnProperty::Description));
}


std::vector<bool> ColumnModel::filterAllows(size_t col)
{
	DataSetPackage *	pkg = DataSetPackage::pkg();
	QModelIndex			p	= pkg->indexForSubNode(node());
	boolvec				allows(pkg->rowCount(p));

	for(int row=0; row<pkg->rowCount(p); row++)
		allows[row] = pkg->data(pkg->index(row, 0, p), int(DataSetPackage::specialRoles::filter)).toBool();

	return allows;
}

std::vector<std::string> ColumnModel::labels(size_t col)
{
	DataSetPackage *			pkg = DataSetPackage::pkg();
	QModelIndex					p	= pkg->indexForSubNode(node());
	std::vector<std::string>	labels(pkg->rowCount(p));

	for(int row=0; row<pkg->rowCount(p); row++)
		labels[row] = pkg->data(pkg->index(row, 0, p), Qt::DisplayRole).toString().toStdString();

	return labels;
}

std::vector<size_t> ColumnModel::getSortedSelection() const
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

void ColumnModel::setValueMaxWidth()
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

void ColumnModel::setLabelMaxWidth()
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

void ColumnModel::moveSelectionUp()
{
	std::vector<size_t> indexes = getSortedSelection();
	if (indexes.size() < 1)
		return;

	_lastSelected = -1;
	_undoStack->pushCommand(new MoveLabelCommand(this, indexes, true));
}

void ColumnModel::moveSelectionDown()
{
	std::vector<size_t> indexes = getSortedSelection();
	if (indexes.size() < 1)
		return;

	_lastSelected = -1;
	_undoStack->pushCommand(new MoveLabelCommand(this, indexes, false));
}

void ColumnModel::reverse()
{
	_lastSelected = -1;
	_undoStack->pushCommand(new ReverseLabelCommand(this));
}

bool ColumnModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
	if(role == int(DataSetPackage::specialRoles::selected))
		return false;

	bool result = DataSetTableProxy::setData(index, value, role);

	if (!_editing && (role == Qt::EditRole || role == int(DataSetPackage::specialRoles::filter)))
		setSelected(index.row(), 0);

	return result;
}

QVariant ColumnModel::data(	const QModelIndex & index, int role) const
{
	if(role == int(DataSetPackage::specialRoles::selected))
	{
		bool s = _selected.count(data(index, int(DataSetPackage::specialRoles::value)).toString()) > 0;
		return s;
	}

	return DataSetTableProxy::data(index, role > 0 ? role : int(DataSetPackage::specialRoles::label));
}

void ColumnModel::filteredOutChangedHandler(int c)
{
	JASPTIMER_SCOPE(ColumnModel::filteredOutChangedHandler);

	if(c == chosenColumn())
		emit filteredOutChanged();
}

int ColumnModel::filteredOut() const
{
	return DataSetPackage::pkg()->filteredOut(chosenColumn());
}

void ColumnModel::resetFilterAllows()
{
	DataSetPackage::pkg()->resetFilterAllows(chosenColumn());
}

void ColumnModel::setVisible(bool visible)
{
	//visible = visible && rowCount() > 0; //cannot show labels when there are no labels

	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}

int ColumnModel::dataColumnCount() const
{
	return DataSetPackage::pkg()->dataColumnCount();
}

Column * ColumnModel::column() const 
{
	return static_cast<Column *>(node());
}

int ColumnModel::chosenColumn() const
{
	Column * c = column();
	
	if(!c)
		return -1;
	
	return c->data()->columnIndex(c);
}

void ColumnModel::setChosenColumn(int chosenColumn)
{
	emit beforeChangingColumn(chosenColumn);
	//This only works as long as we have a single dataSet but lets not go overboard with rewriting stuff atm
	DataSet * data = DataSetPackage::pkg()->dataSet();
	
	subNodeModel()->selectNode(data ? data->column(chosenColumn) : nullptr);
	emit showLabelEditorChanged();
	emit showComputedColumnChanged();
}


void ColumnModel::columnDataTypeChanged(const QString & colName)
{
	int colIndex = DataSetPackage::pkg()->getColumnIndex(colName);

	if(colIndex == chosenColumn())
	{
//		if(DataSetPackage::pkg()->dataSet()->column(colIndex)->type() == columnType::scale)
//			setChosenColumn(-1);
		emit showLabelEditorChanged();
		emit showComputedColumnChanged();
		
		invalidate();
	}
}

void ColumnModel::setRowWidth(double len)
{
	if (abs(len - _rowWidth) > 0.001)
	{
		_rowWidth = len;

		emit headerDataChanged(Qt::Horizontal, 0, 0);
	}
}

///Override of headerData because it doesnt get QModelIndex and thus cannot know whether it is proxied by columnModel or something else...
QVariant ColumnModel::headerData(int section, Qt::Orientation orientation, int role)	const
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

void ColumnModel::onChosenColumnChanged()
{
	_selected.clear();
	_lastSelected = -1;
	setValueMaxWidth();
	setLabelMaxWidth();
	//dataChanged probably not needed 'cause we are in a reset
}

void ColumnModel::refresh()
{
	//if(column() && column()->type() == columnType::scale)
	//	setChosenColumn(-1);
	
	beginResetModel();
	endResetModel();
	emit showLabelEditorChanged();
	emit showComputedColumnChanged();

	setValueMaxWidth();
	setLabelMaxWidth();
}

void ColumnModel::changeSelectedColumn(QPoint selectionStart)
{
	if (selectionStart.x() != chosenColumn() && visible())
		setChosenColumn(selectionStart.x());
}

void ColumnModel::checkRemovedColumns(int columnIndex, int count)
{
	int currentCol = chosenColumn();
	if ((columnIndex <= currentCol) && (currentCol < columnIndex + count))
	{
		setVisible(false);
		setChosenColumn(-1);
	}
}

void ColumnModel::removeAllSelected()
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
			emit dataChanged(ColumnModel::index(selectedRow, 0), ColumnModel::index(selectedRow, 0), {int(DataSetPackage::specialRoles::selected)});
		}
	}
}

void ColumnModel::setSelected(int row, int modifier)
{
	if (modifier & Qt::ShiftModifier && _lastSelected >= 0)
	{
		int start = _lastSelected >= row ? row : _lastSelected;
		int end = start == _lastSelected ? row : _lastSelected;
		for (int i = start; i <= end; i++)
		{
			QString rowValue = data(index(i, 0), int(DataSetPackage::specialRoles::value)).toString();
			_selected.insert(rowValue);
			emit dataChanged(ColumnModel::index(i, 0), ColumnModel::index(i, 0), {int(DataSetPackage::specialRoles::selected)});
		}
	}
	else if (modifier & Qt::ControlModifier)
	{
		QString rowValue = data(index(row, 0), int(DataSetPackage::specialRoles::value)).toString();
		_selected.insert(rowValue);
		emit dataChanged(ColumnModel::index(row, 0), ColumnModel::index(row, 0), {int(DataSetPackage::specialRoles::selected)});
	}
	else
	{
		QString rowValue = data(index(row, 0), int(DataSetPackage::specialRoles::value)).toString();
		bool disableCurrent = _selected.count(rowValue) > 0;
		removeAllSelected();
		if (!disableCurrent)	_selected.insert(rowValue);
		else					_selected.erase(rowValue);
		emit dataChanged(ColumnModel::index(row, 0), ColumnModel::index(row, 0), {int(DataSetPackage::specialRoles::selected)});
	}
	_lastSelected = row;

}

void ColumnModel::unselectAll()
{
	_selected.clear();
	_lastSelected = -1;
	emit dataChanged(ColumnModel::index(0, 0), ColumnModel::index(rowCount(), 0), {int(DataSetPackage::specialRoles::selected)});
}

bool ColumnModel::setChecked(int rowIndex, bool checked)
{
	JASPTIMER_SCOPE(ColumnModel::setChecked);

	_editing = true;
	_undoStack->pushCommand(new FilterLabelCommand(this, rowIndex, checked));
	_editing = false;

	return data(index(rowIndex, 0), int(DataSetPackage::specialRoles::filter)).toBool();
}

void ColumnModel::setLabel(int rowIndex, QString label)
{
	_editing = true;
	_undoStack->pushCommand(new SetLabelCommand(this, rowIndex, label));
	_editing = false;
}

bool ColumnModel::showLabelEditor() const
{
	if(column())
			return column()->type() != columnType::scale && rowCount() > 0;
	return false;
}

bool ColumnModel::showComputedColumn() const
{
	if(column())
		return column()->isComputed();
	return false;
}

bool ColumnModel::columnIsFiltered() const
{
	if(column())
		return column()->hasFilter();
	return false;
}
