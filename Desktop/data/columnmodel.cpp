#include "timers.h"
#include "qutils.h"
#include "column.h"
#include "dataenums.h"
#include "dataset.h"
#include "jasptheme.h"
#include "columnmodel.h"
#include "columnutils.h"
#include "datasetpackage.h"
#include "gui/preferencesmodel.h"

ColumnModel::ColumnModel() : QIdentityProxyModel(DataSetPackage::pkg())
{
	connect(DataSetPackage::pkg(),	&DataSetPackage::shownFilterChanged,			this, &ColumnModel::refreshFilteredOut				);

	connect(DataSetPackage::pkg(),	&DataSetPackage::allFiltersReset,				this, &ColumnModel::allFiltersReset				);
	
	connect(DataSetPackage::pkg(),	&DataSetPackage::datasetChanged,				this, &ColumnModel::checkCurrentColumn			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::workspaceEmptyValuesChanged,	this, &ColumnModel::emptyValuesChanged			);
	connect(DataSetPackage::pkg(),	&DataSetPackage::chooseColumn,					this, &ColumnModel::setChosenColumn				);
	connect(DataSetPackage::pkg(),	&DataSetPackage::shownDataSetChanged,			this, &ColumnModel::shownDataSetChangedHandler	);
}

QVariant ColumnModel::columnTypeFriendlyMapping(computedColumnType compColT)
{
	typedef QMap<QString, QVariant> localMap;
	
	return	localMap(
			{	
				std::make_pair("value", computedColumnTypeToQString(compColT)),		
				std::make_pair("label", Column::columnTypeFriendlyName(compColT))	
			});
}

QVariantList ColumnModel::computedTypeValues() const
{
	switch(column() ? column()->codeType() : computedColumnType::notComputed)
	{
	case computedColumnType::notComputed:
	case computedColumnType::rCode:
	case computedColumnType::constructorCode:
		return { columnTypeFriendlyMapping(computedColumnType::notComputed), columnTypeFriendlyMapping(computedColumnType::rCode), columnTypeFriendlyMapping(computedColumnType::constructorCode) };

	case computedColumnType::analysis:
		return  { columnTypeFriendlyMapping(computedColumnType::analysis) };

	case computedColumnType::analysisNotComputed:
		return { columnTypeFriendlyMapping(computedColumnType::analysisNotComputed), columnTypeFriendlyMapping(computedColumnType::notComputed), columnTypeFriendlyMapping(computedColumnType::rCode), columnTypeFriendlyMapping(computedColumnType::constructorCode) };;
	}
	
	return {};
}

QVariantList ColumnModel::columnTypeValues() const
{
	typedef QMap<QString, QVariant> localMap;
	
	return {
		localMap({ std::make_pair("value", columnTypeToQString(columnType::scale)),				std::make_pair("label", QObject::tr("Scale")),		std::make_pair("columnTypeIcon", JaspTheme::currentIconPath() + "variable-scale.svg")	}),
		localMap({ std::make_pair("value", columnTypeToQString(columnType::ordinal)),			std::make_pair("label", QObject::tr("Ordinal")),	std::make_pair("columnTypeIcon", JaspTheme::currentIconPath() + "variable-ordinal.svg")	}),
		localMap({ std::make_pair("value", columnTypeToQString(columnType::nominal)),			std::make_pair("label", QObject::tr("Nominal")),	std::make_pair("columnTypeIcon", JaspTheme::currentIconPath() + "variable-nominal.svg")	})
	};
}

QString ColumnModel::columnNameQ()
{
	if (_virtual) return _dummyColumn.name;

	return QString::fromStdString(column() ? column()->name() : "");
}


void ColumnModel::setColumnNameQ(QString newColumnName)
{
	if (_beingRefreshed || newColumnName == columnNameQ()) return;

	if (_virtual)
	{
		undoStack()->startMacro();

		for (int colNr = DataSetPackage::pkg()->dataSet()->columnCount(); colNr < _columnIndex; colNr++)
			undoStack()->pushCommand(new InsertColumnCommand(DataSetPackage::pkg()->dataSet(), colNr));

		QMap<QString, QVariant> props;
		props["name"]			= newColumnName;
		props["type"]			= int(_dummyColumn.type);
		props["computed"]		= int(_dummyColumn.computedType);
		props["computeFilter"]	= _dummyColumn.computeFilter;
		undoStack()->endMacro(new InsertColumnCommand(DataSetPackage::pkg()->dataSet(), _columnIndex, props));
	}
	else if(column())
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), newColumnName, SetColumnPropertyCommand::ColumnProperty::Name));
}

QString ColumnModel::columnTitle() const
{
	if (_virtual) return _dummyColumn.title;

	return QString::fromStdString(column() ? column()->title() : "");
}

void ColumnModel::setColumnTitle(const QString & newColumnTitle)
{
	if (_beingRefreshed)
		return;

	if (_virtual)
		_dummyColumn.title = newColumnTitle;

	if(column() && column()->title() != fq(newColumnTitle))
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), newColumnTitle, SetColumnPropertyCommand::ColumnProperty::Title));
}

void ColumnModel::setDropLevels(QString dropLevels)
{
	if (_beingRefreshed)
		return;

	dropLevelsType dropEm = dropLevelsType::drop;
	
	try { dropEm = dropLevelsTypeFromQString(dropLevels); } catch(...){} 

	if(column())
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), dropLevelsTypeToQString(dropEm), SetColumnPropertyCommand::ColumnProperty::DropLevels));
}

QString ColumnModel::columnDescription() const
{
	if (_virtual) return _dummyColumn.description;

	return tq(column() ? column()->description() : "");
}

QString ColumnModel::computeFilter() const
{
	if (_virtual) 
		return _dummyColumn.computeFilter;
	
	if(column())
		return tq(column()->computeFilter());
	
	return "";
}


bool ColumnModel::autoSort() const
{
	if (_virtual) 
		return PreferencesModel::prefs()->orderByValueByDefault();
	
	return column() && column()->autoSortByValue();
}

void ColumnModel::setAutoSort(bool newAutoSort)
{
	if (!column() || column()->autoSortByValue() == newAutoSort)
		return;
	
	column()->setAutoSortByValue(newAutoSort);
	
	emit autoSortChanged();
}

bool ColumnModel::useCustomEmptyValues() const
{
	if (_virtual || !column()) return false;

	return column()->hasCustomEmptyValues();
}

void ColumnModel::setUseCustomEmptyValues(bool useCustom)
{
	if (_beingRefreshed || _virtual || !column() || column()->hasCustomEmptyValues() == useCustom) return;

	undoStack()->pushCommand(new SetUseCustomEmptyValuesCommand(column(), useCustom));
}

QStringList ColumnModel::emptyValues() const
{
	return (_virtual || !column()) ? QStringList() : tql(column()->emptyValues()->emptyStringsColumnModel());
}

int ColumnModel::rowsTotal() const
{
	return rowCount();	
}

int ColumnModel::rowCount(const QModelIndex &parent) const
{
	//The label-editor (and rowsTotal) present the column's *labels*, not its data rows. A (scale)
	//column's data lives in _dbls but only its non-empty labels are editable here, so the number of
	//rows a view should show is the number of labels, not the underlying row count.
	return parent.isValid() ? 0 : (column() ? int(column()->labelsNonEmptyCount()) : 0);
}

QString ColumnModel::dropLevels() const
{
	return dropLevelsTypeToQString(_virtual || !column() || column()->dropLevels() == dropLevelsType::noChoice ? dropLevelsType::drop : column()->dropLevels());
}

bool ColumnModel::hasSeveralNumericValues() const
{
	if(!column())
		return false;
	
	int numberOfNumericalValues = 0;
	for(Label * label : column()->labels())	
		if(!label->isEmptyValue())
		{
			static double dummy;
			
			if(label->originalValue().isDouble() && ColumnUtils::getDoubleValue(label->originalValueAsString(), dummy))
				numberOfNumericalValues++;

			if (numberOfNumericalValues > 1)
				return true;
		}
	
	return false;
}

void ColumnModel::setCustomEmptyValues(const QStringList& customEmptyValues)
{
	if (_beingRefreshed || _virtual || !column() || column()->emptyValues()->emptyStrings() == fql(customEmptyValues)) return;

	undoStack()->pushCommand(new SetCustomEmptyValuesCommand(column(), customEmptyValues));
}


void ColumnModel::addEmptyValue(const QString & value)
{
	QStringList values = emptyValues();
	values.push_back(value);
	setCustomEmptyValues(values);
}

void ColumnModel::removeEmptyValue(const QString & value)
{
	QStringList values = emptyValues();
	values.removeAll(value);
	setCustomEmptyValues(values);
}

void ColumnModel::resetEmptyValues()
{
	if(column())
		setCustomEmptyValues(tql(column()->data()->emptyValuesAsStrings()));
}

UndoStack *ColumnModel::undoStack()
{
	return UndoStack::singleton();
}


QVariantList ColumnModel::tabs() const
{
	QVariantList tabs;
	Column* col = column();
	
	if(_compactMode)
		tabs.push_back(QMap<QString, QVariant>({  std::make_pair("name", "basicInfo"), std::make_pair("title", tr("Column definition"))}));
	
	if(col)
	{
		if (col->isComputed() && (col->codeType() == computedColumnType::rCode || col->codeType() == computedColumnType::constructorCode))
			tabs.push_back(QMap<QString, QVariant>({  std::make_pair("name", "computed"), std::make_pair("title", tr("Computed column definition"))}));

		tabs.push_back(QMap<QString, QVariant>({  std::make_pair("name", "label"), std::make_pair("title", tr("Label editor"))}));
	}

	QMap<QString, QVariant> misingValues =	{  std::make_pair("name", "missingValues"), std::make_pair("title", tr("Missing values"))};
	tabs.push_back(misingValues);

	return tabs;
}


QString ColumnModel::currentColumnType() const
{
	if (_virtual) return columnTypeToQString(_dummyColumn.type);

	columnType type = column() ? column()->type() : columnType::scale;

	return columnTypeToQString(type);
}

QString ColumnModel::computedType() const
{
	if (_virtual) return computedColumnTypeToQString(_dummyColumn.computedType);

	return column() ? computedColumnTypeToQString(column()->codeType()) : computedColumnTypeToQString(computedColumnType::notComputed);
}

bool ColumnModel::computedTypeEditable() const
{
	if(_virtual)
		return true;

	if (!column())
		return false;

	switch (column()->codeType())
	{
	case computedColumnType::notComputed:
	case computedColumnType::analysisNotComputed:
	case computedColumnType::constructorCode:
	case computedColumnType::rCode:
		return true;

	default:
		return false;
	}
}

bool ColumnModel::isComputed() const
{
	if(_virtual)
		return false;

	if (!column())
		return false;

	return column()->isComputed();
}

void ColumnModel::setColumnDescription(const QString & newColumnDescription)
{
	if (_beingRefreshed)
		return;

	if (_virtual)
		_dummyColumn.description = newColumnDescription;

	if(column() && column()->description() != fq(newColumnDescription))
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), newColumnDescription, SetColumnPropertyCommand::ColumnProperty::Description));
}

void ColumnModel::setComputedType(QString type)
{
	if (_beingRefreshed || type.isEmpty() || type == computedType() || !computedColumnTypeValidName(fq(type)))
		return;

	computedColumnType cType = computedColumnTypeFromString(type.toStdString());

	if (_virtual)
		_dummyColumn.computedType = cType;
	else if(column())
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), int(cType), SetColumnPropertyCommand::ColumnProperty::ComputedColumnType));

	emit tabsChanged();
}

void ColumnModel::setComputeFilter(const QString &newComputeFilter)
{
	if(_beingRefreshed || !column() || column()->computeFilter() == fq(newComputeFilter))
		return;

	if (_virtual)
		_dummyColumn.computeFilter = newComputeFilter;
	
	else if(column())
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), newComputeFilter, SetColumnPropertyCommand::ColumnProperty::ComputeFilter));

	emit tabsChanged();
}

void ColumnModel::setColumnType(QString type)
{
	if (_beingRefreshed || type.isEmpty() || type == currentColumnType() || !columnTypeValidName(fq(type))) 
		return;

	columnType cType = columnTypeFromString(type.toStdString());

	if (_virtual)
		_dummyColumn.type = cType;
	else if(column())
		undoStack()->pushCommand(new SetColumnTypeCommand(column()->data(), {fq(columnNameQ())}, int(cType)));
}

std::vector<size_t> ColumnModel::getSortedSelection() const
{
	if (_virtual) return {};

	std::map<QString, size_t> mapValueToRow;

	for(size_t r=0; r<size_t(rowCount()); r++)
		mapValueToRow[data(index(r, 0), int(dataPkgRoles::value)).toString()] = r;

	std::vector<size_t> out;

	for(const QString & v : _selected)
		out.push_back(mapValueToRow[v]);

	std::sort(out.begin(), out.end());

	return out;
}

void ColumnModel::setValueMaxWidth()
{
	size_t maxWidthChars = std::max(size_t(tr("Value").size()), !column() ? 0 : column()->getMaximumWidthInCharacters(false, true));
	
	double prevMaxWidth = _valueMaxWidth;
	_valueMaxWidth = JaspTheme::fontMetrics().size(Qt::TextSingleLine, QString(maxWidthChars, 'X')).width();

	if(_valueMaxWidth != prevMaxWidth)
		emit valueMaxWidthChanged();
}

void ColumnModel::setLabelMaxWidth()
{
	size_t maxWidthChars = std::max(size_t(tr("Label").size()), !column() ? 0 : column()->getMaximumWidthInCharacters(false, false));
	
	double prevMaxWidth = _labelMaxWidth;
	_labelMaxWidth = JaspTheme::fontMetrics().size(Qt::TextSingleLine, QString(maxWidthChars, 'X')).width();

	if(_labelMaxWidth != prevMaxWidth)
		emit labelMaxWidthChanged();
}

void ColumnModel::moveSelectionUp()
{
	std::vector<size_t> indexes = getSortedSelection();
	if (_beingRefreshed || indexes.size() < 1)
		return;

	_lastSelected = -1;
	undoStack()->pushCommand(new MoveLabelCommand(column(), indexes, true));
}

void ColumnModel::moveSelectionDown()
{
	std::vector<size_t> indexes = getSortedSelection();
	if (_beingRefreshed || indexes.size() < 1)
		return;

	_lastSelected = -1;
	undoStack()->pushCommand(new MoveLabelCommand(column(), indexes, false));
}

void ColumnModel::reverse()
{
	if (_beingRefreshed)
		return;

	_lastSelected = -1;
	undoStack()->pushCommand(new ReverseLabelCommand(column()));
}

void ColumnModel::reverseValues()
{
	if (_beingRefreshed)
		return;

	_lastSelected = -1;
	undoStack()->pushCommand(new ColumnReverseValuesCommand(column()->data(), {fq(columnNameQ())}));
}

void ColumnModel::toggleAutoSortByValues()
{
	_lastSelected = -1;
	undoStack()->pushCommand(new ColumnToggleAutoSortByValuesCommand(column()->data(), {fq(columnNameQ())}));
}

bool ColumnModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
	if(role == int(dataPkgRoles::selected))
		return false;

	bool result = QIdentityProxyModel::setData(index, value, role);

	if (!_editing && (role == Qt::EditRole || role == int(dataPkgRoles::filter)))
		setSelected(index.row(), 0);

	return result;
}

QVariant ColumnModel::data(	const QModelIndex & index, int role) const
{
	if(role == int(dataPkgRoles::selected))
	{
		bool s = _selected.count(data(index, int(dataPkgRoles::value)).toString()) > 0;
		return s;
	}

	return QIdentityProxyModel::data(index, role > 0 ? role : int(dataPkgRoles::label));
}

QVariant ColumnModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if(role == int(dataPkgRoles::columnWidthFallback))
		return rowWidth();
	
	return !sourceModel() ? false : sourceModel()->headerData(section, orientation, role);
}

void ColumnModel::refreshFilteredOut()
{
	JASPTIMER_SCOPE(ColumnModel::refreshFilteredOut);

	//Re-query the chosen column's label-filter state so QML's `filteredOut` reacts to label-filter
	//changes while the chosen column itself stays unchanged (previously only re-emitted on selection).
	emit filteredOutChanged();
	emit columnIsFilteredChanged();
}

int ColumnModel::filteredOut() const
{
	return !column() ? 0 :column()->filteredOut();
}

void ColumnModel::resetFilterAllows()
{
	column()->resetFilterAllows();
}

void ColumnModel::setVisible(bool visible)
{
	//visible = visible && rowCount() > 0; //cannot show labels when there are no labels

	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}

Column * ColumnModel::column() const
{
	return _column;
}

int ColumnModel::chosenColumn() const
{
	Column * c = column();
	
	if(!c)
		return -1;
	
	if(!c->data())
		return -1;
	
	return c->data()->columnIndex(c);
}

void ColumnModel::setChosenColumnByName(const QString chosenNameQ, int colIndex)
{
	std::string chosenName = fq(chosenNameQ);
	// Always set the chosen column even if it is the same one: the ColumnModel might be not reset correctly when the dataset is closed.

	//If the user deletes the name the column ought to be removed because we cannot have columns without a name!
	Column * deleteMe = column() && column()->name() == "" ? column() : nullptr;

	emit beforeChangingColumn(chosenNameQ);

	DataSet * data = DataSetPackage::pkg()->dataSet();
	clearVirtual();
	
	Column * chosenColumn = data->column(chosenName);

	//Drop any per-column connections to the *previous* column before switching to the new one,
	//otherwise the old column (still alive in the dataset) keeps firing into this model.
	if(_column && _column != chosenColumn)
		disconnect(_column, nullptr, this, nullptr);
	
	_virtual = !chosenColumn;
	emit isVirtualChanged();

	setSourceModel(chosenColumn);
	_column = chosenColumn;
	
	
	if(_column)
	{
		connect(_column,	&Column::modelReset,					this, &ColumnModel::rowsTotalChanged,			Qt::UniqueConnection);
		connect(_column,	&Column::columnChanged,					this, &ColumnModel::setLabelMaxWidth,			Qt::UniqueConnection);
	}

	
	_columnIndex = colIndex != -1 || _virtual || !chosenColumn || !chosenColumn->data() ? colIndex : chosenColumn->data()->columnIndex(chosenColumn);

	refresh();
	notifyColumnChanged();

	if(deleteMe && data)
	{
		const int doomedIdx = data->columnIndex(deleteMe);
		if(doomedIdx >= 0)
			data->removeColumn(doomedIdx);
	}
}

void ColumnModel::setChosenColumn(int columnIndex)
{
	QString name = emit columnNameForIndex(columnIndex);
	
	if(name != "")
	{
		setChosenColumnByName(name);
		return;
	}
	
	_columnIndex = columnIndex;
	
	_virtual = true;
	emit isVirtualChanged();
	
	setSourceModel(nullptr);
	
	_column = nullptr;
	refresh();
	notifyColumnChanged();
	
}

void ColumnModel::checkInsertedColumns(const QModelIndex &, int first, int)
{
	if (_columnIndex >= first)
	{
		_columnIndex = -1; // Force the setting of new column.
		setChosenColumn(first);
	}
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

void ColumnModel::openComputedColumn(const QString name)
{
	setChosenColumnByName(name);
	setVisible(true);
}

void ColumnModel::checkCurrentColumn(int dataSetId, QStringList, QStringList missingColumns, QMap<QString, QString> changeNameColumns, bool, bool hasNewColumns)
{
	DataSet * current = DataSetPackage::pkg()->dataSet();
	if(!current || dataSetId != current->id())
		return;
	
	QString colName = columnNameQ();

	if (missingColumns.contains(colName))
	{
		setVisible(false);
		setChosenColumn(-1);
	}
	else
	{
		if (!_virtual && changeNameColumns.contains(colName))
			setColumnNameQ(changeNameColumns[colName]);
		if (hasNewColumns && _virtual && DataSetPackage::pkg()->dataSet()->columnCount() >= _columnIndex)
		{
			// The current column is not virtual anymore: reset it
			_columnIndex = -1;
			setChosenColumnByName(colName);
		}
	}
}

void ColumnModel::shownDataSetChangedHandler(DataSet * newDataSet)
{
	_shownDataSet = newDataSet;

	if(!newDataSet)
	{
		//Teardown (deleteWorkspace/connectWorkspace between datasets) left _column pointing at a
		//just-destroyed dataset's column: clear it so the Variables/label editor doesn't dereference
		//freed memory, and drop any per-column connections to the old column.
		if(_column)
		{
			disconnect(_column, nullptr, this, nullptr);
			setSourceModel(nullptr);
			_column = nullptr;
			_virtual = true;
			emit isVirtualChanged();
			emit filteredOutChanged();
			emit columnIsFilteredChanged();
		}
		return;
	}

	//Label-level filtering toggles don't fire datasetChanged, so hook the shown dataset's dedicated
	//signal to keep `filteredOut`/`columnIsFiltered` reactive while the chosen column is unchanged.
	connect(newDataSet, &DataSet::labelFilterChanged, this, &ColumnModel::refreshFilteredOut, Qt::UniqueConnection);

	if(!column())
		return;

	QString currentName = columnNameQ();
	if(currentName.isEmpty())
		return;

	//Another dataset became the shown one: re-resolve the currently chosen column (by name)
	//against the new shown dataset so the Variables/label editor follows the tab switch instead of
	//silently editing a non-shown dataset's column.
	setChosenColumnByName(currentName);
}

void ColumnModel::removeAllSelected()
{
	QMap<QString, size_t> mapValueToRow;

	for(size_t r=0; r<size_t(rowCount()); r++)
		mapValueToRow[data(index(r, 0), int(dataPkgRoles::value)).toString()] = r;

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
            emit dataChanged(ColumnModel::index(selectedRow, 0), ColumnModel::index(selectedRow, 0), {int(dataPkgRoles::selected)});
		}
	}
}

void ColumnModel::setRowWidth(double len)
{
	if(std::abs(_rowWidth - len) < 0.001)
		return;
	
	_rowWidth = len;
	emit rowWidthChanged();
	refresh();
}

void ColumnModel::refresh()
{
	beginResetModel();
	endResetModel();
}

void ColumnModel::notifyColumnChanged()
{
	setValueMaxWidth();
	setLabelMaxWidth();

	emit chosenColumnChanged();
	emit filteredOutChanged();
	emit nameEditableChanged();
	emit computedTypeChanged();
	emit computedTypeEditableChanged();
	emit computedTypeValuesChanged();
	emit columnTypeChanged();
	emit columnTypeValuesChanged();
	emit hasSeveralNumericValuesChanged();
	emit rowsTotalChanged();
	emit tabsChanged();
	emit useCustomEmptyValuesChanged();
	emit emptyValuesChanged();
	emit dropLevelsChanged();
	emit columnIsFilteredChanged();
}

void ColumnModel::setSelected(int row, int modifier)
{
	if (modifier & Qt::ShiftModifier && _lastSelected >= 0)
	{
		int start = _lastSelected >= row ? row : _lastSelected;
		int end = start == _lastSelected ? row : _lastSelected;
		for (int i = start; i <= end; i++)
		{
			QString rowValue = data(index(i, 0), int(dataPkgRoles::value)).toString();
			_selected.insert(rowValue);
            emit dataChanged(ColumnModel::index(i, 0), ColumnModel::index(i, 0), {int(dataPkgRoles::selected)});
		}
	}
	else if (modifier & Qt::ControlModifier)
	{
		QString rowValue = data(index(row, 0), int(dataPkgRoles::value)).toString();
		_selected.insert(rowValue);
        emit dataChanged(ColumnModel::index(row, 0), ColumnModel::index(row, 0), {int(dataPkgRoles::selected)});
	}
	else
	{
		QString rowValue = data(index(row, 0), int(dataPkgRoles::value)).toString();
		bool disableCurrent = _selected.count(rowValue) > 0;
		removeAllSelected();
		
		if (!disableCurrent)	_selected.insert(rowValue);
		else					_selected.erase(rowValue);
        emit dataChanged(ColumnModel::index(row, 0), ColumnModel::index(row, 0), {int(dataPkgRoles::selected)});
	}
	
	_lastSelected = row;

}

void ColumnModel::unselectAll()
{
	_selected.clear();
	_lastSelected = -1;
	refresh(); //emit dataChanged(ColumnModel::index(0, 0), ColumnModel::index(rowCount(), 0), {int(dataPkgRoles::selected)});
}

bool ColumnModel::setChecked(int rowIndex, bool checked)
{
	JASPTIMER_SCOPE(ColumnModel::setChecked);
	
	if(_beingRefreshed || checked == data(index(rowIndex,0), int(dataPkgRoles::filter)).toBool())
		return true; //Its already that value
	
	setSelected(rowIndex, true);

	_editing = true;
	undoStack()->pushCommand(new FilterLabelCommand(column(), rowIndex, checked));
	_editing = false;
	
	return data(index(rowIndex, 0), int(dataPkgRoles::filter)).toBool() == checked;
}

void ColumnModel::setValue(int rowIndex, const QString &value)
{
	JASPTIMER_SCOPE(ColumnModel::setValue);
	
	QString originalValue = data(index(rowIndex,0), int(dataPkgRoles::value)).toString();
	
	if(_beingRefreshed || value == originalValue)
		return; //Its already that value
	
	_editing = true;
	undoStack()->pushCommand(new SetLabelOriginalValueCommand(column(), rowIndex, value));
	_editing = false;
}

void ColumnModel::setLabel(int rowIndex, QString label)
{
	JASPTIMER_SCOPE(ColumnModel::setLabel);
	
	QString originalLabel = data(index(rowIndex,0), int(dataPkgRoles::label)).toString();
	
	if(_beingRefreshed || label == originalLabel)
		return; //Its already that value
	
	_editing = true;
	undoStack()->pushCommand(new SetLabelCommand(column(), rowIndex, label));
	_editing = false;
}

void ColumnModel::deleteLabel(int rowIndex)
{
	undoStack()->pushCommand(new DeleteLabelCommand(column(), rowIndex));
}

void ColumnModel::addLabel(QString value, QString label)
{
	undoStack()->pushCommand(new AddLabelCommand(column(), value, label));
}

bool ColumnModel::columnIsFiltered() const
{
	return column() && column()->hasLabelFilter();
}

bool ColumnModel::nameEditable() const
{
	if(column())
		return !(column()->isComputed() && (column()->codeType() == computedColumnType::analysisNotComputed || column()->codeType() == computedColumnType::analysis));

	return true;
}

void ColumnModel::clearVirtual()
{
	_dummyColumn.description.clear();
	_dummyColumn.name.clear();
	_dummyColumn.title.clear();
	_dummyColumn.computeFilter.clear();

	_dummyColumn.type			= columnType::scale;
	_dummyColumn.computedType	= computedColumnType::notComputed;
}

bool ColumnModel::compactMode() const
{
	return _compactMode;
}

void ColumnModel::setCompactMode(bool newCompactMode)
{
	if (_compactMode == newCompactMode)
		return;
	_compactMode = newCompactMode;
	emit compactModeChanged();
	emit tabsChanged();
}

void ColumnModel::languageChangedHandler()
{
	emit columnTypeValuesChanged();
	emit computedTypeValuesChanged();
	emit tabsChanged();
}

bool ColumnModel::hasLabels() const
{
	return column() ? column()->hasLabels() : false;
}

void ColumnModel::setHasLabels(bool newHasLabels)
{
	if (_beingRefreshed)
		return;


	if(column())
		undoStack()->pushCommand(new SetColumnPropertyCommand(column(), newHasLabels, SetColumnPropertyCommand::ColumnProperty::HasLabels));
}

bool ColumnModel::isColumnNameFree(const QString & name)
{
	DataSet * dataSet = DataSetPackage::pkg()->dataSet();

	return dataSet && !dataSet->column(fq(name));
}

void ColumnModel::createComputedColumn(const QString & name, int colType, bool useJsonConstructor)
{
	DataSet * dataSet = DataSetPackage::pkg()->dataSet();

	if(!dataSet || !isColumnNameFree(name))
		return;

	// Undoable: the command's redo() creates the column, its undo() removes it.
	undoStack()->pushCommand(new CreateComputedColumnCommand(
		dataSet,
		name,
		columnType(colType),
		useJsonConstructor ? computedColumnType::constructorCode : computedColumnType::rCode));

	openComputedColumn(name);
}

void ColumnModel::setComputedColumnCode(const QString & rCode, const QString & json)
{
	if(!column() || _beingRefreshed)
		return;

	if(column()->rCodeQ() == rCode && column()->constructorJsonQ() == json)
		return;

	undoStack()->pushCommand(new SetComputedColumnCodeCommand(DataSetPackage::filter(), column(), rCode, json));
}
