#include "log.h"
#include "undostack.h"
#include "column.h"
#include "filter.h"
#include "qutils.h"
#include "timers.h"
//#include "datasettablemodel.h"
#include "dataenums.h"
#include "workspace.h"

UndoStack* UndoStack::_currentUndoStack = nullptr;

UndoStack::UndoStack(QObject* parent) : QUndoStack(parent)
{
	connect(this, &QUndoStack::indexChanged, []() { if(Workspace::singleton()) Workspace::singleton()->somethingModified(); });
}

void UndoStack::pushCommand(UndoModelCommand *command)
{
	if (!_parentCommand) // Push to the stack only when no macro is started: in this case the command is autmatically added to the _parentCommand
		push(command);
}

void UndoStack::startMacro(const QString &text)
{
	if (_parentCommand)
	{
		Log::log() << "Macro started though last one is not finished!" << std::endl; //I think this should be an assert...
		delete _parentCommand; //Which it never was, so instead of leaking the unfinished macro we drop it here.
	}
	
	_parentCommand = new UndoModelCommand();
	
	if (!text.isEmpty())
		_parentCommand->setText(text);
}

void UndoStack::endMacro(UndoModelCommand *command)
{
	if(!_parentCommand)
	{
		push(command);
		return;
	}
	
	if (command && _parentCommand->text().isEmpty())
		_parentCommand->setText(command->text());
	
	
	if (_parentCommand)
		push(_parentCommand);

	_parentCommand = nullptr;
}

SetDataCommand::SetDataCommand(DataSet * data, int row, int col, const QVariant &value, int role)
	: UndoModelCommand(data), _newData{value}, _row{row}, _col{col}, _role{role}
{
	setText(QObject::tr("Set value to '%1' at row %2 column '%3'").arg(_newData.toString()).arg(rowName(_row)).arg(columnName(_col)));
	
	_oldValue = dataSet()->data(dataSet()->index(_row, _col), int(dataPkgRoles::value));
	_oldLabel = dataSet()->data(dataSet()->index(_row, _col), int(dataPkgRoles::label));	
}

void SetDataCommand::undo()
{
	dataSet()->setData(dataSet()->index(_row, _col), QVariantList({_oldValue, _oldLabel}), int(dataPkgRoles::valueLabelPair));	
}

void SetDataCommand::redo()
{

	dataSet()->setData(dataSet()->index(_row, _col), _newData, _role);

}

InsertColumnCommand::InsertColumnCommand(DataSet * data, int column, const QMap<QString, QVariant>& props)
	: UndoModelCommand(data), _col{column}, _props{props}
{
	QString colName = props.contains("name") ? props["name"].toString() : columnName(_col);
	setText(props.contains("computed") ? QObject::tr("Insert computed column '%1'").arg(_col) : QObject::tr("Insert column '%1'").arg(colName));
}

void InsertColumnCommand::undo()
{
	dataSet()->removeColumn(_col);
}

void InsertColumnCommand::redo()
{
	dataSet()->insertColumnSpecial(_col, _props);
}

InsertColumnsCommand::InsertColumnsCommand(DataSet * data, int col, int count)
	: UndoModelCommand(data), _col{col}, _count{count}
{
	setText(QObject::tr("Insert %2 cols at %1").arg(columnName(_col)).arg(_count));
}

void InsertColumnsCommand::undo()
{
	dataSet()->removeColumns(_col, _count);
}

void InsertColumnsCommand::redo()
{
	dataSet()->insertColumns(_col, _count, QModelIndex());
}

InsertRowsCommand::InsertRowsCommand(DataSet * data, int row, int count)
	: UndoModelCommand(data), _row{row}, _count{count}
{
	setText(QObject::tr("Insert %2 rows at %1").arg(rowName(_row)).arg(_count));
}

void InsertRowsCommand::undo()
{
	dataSet()->removeRows(_row, _count);
}

void InsertRowsCommand::redo()
{
	dataSet()->insertRows(_row, _count);
}

RemoveColumnsCommand::RemoveColumnsCommand(DataSet * data, int start, int count)
	: UndoModelCommand(data), _start{start}, _count{count}
{
	if (count == 1)
		setText(QObject::tr("Remove column '%1'").arg(columnName(_start)));
	else
		setText(QObject::tr("Remove %1 columns from '%2'").arg(_count).arg(columnName(_start)));
}

void RemoveColumnsCommand::undo()
{
	dataSet()->insertColumns(_start, _count);
	
	for (int col = _start; col < _start + _count; col++)
		column(col)->deserialize(_serializedColumns[col - _start]);

}

void RemoveColumnsCommand::redo()
{
	_serializedColumns.clear();
	
	assert(dataSetStillExists());

	_count = std::min(dataSet()->columnCount() - _start, _count);
	
	for (int col = 0; col < _count; col++)
		_serializedColumns.push_back(column(_start+col)->serialize());
	
	dataSet()->removeColumns(_start, _count);
}

RemoveRowsCommand::RemoveRowsCommand(DataSet * data, int start, int count)
	: UndoModelCommand(data), _start{start}, _count{count}
{
	if (count == 1)
		setText(QObject::tr("Remove row %1").arg(rowName(_start)));
	else
		setText(QObject::tr("Remove rows %1 to %2").arg(rowName(_start), rowName(_start + count)));
}

void RemoveRowsCommand::undo()
{
	dataSet()->insertRows(_start, _count);
	dataSet()->pasteSpreadsheet(_start, 0, _values, _labels, _colTypes);
}

void RemoveRowsCommand::redo()
{
	_values		. clear();
	_labels		. clear();
	_colTypes	. clear();

	for (int i = 0; i < dataSet()->columnCount(); i++)
	{
		_values		. push_back(std::vector<QString>());
		_labels		. push_back(std::vector<QString>());
		_colTypes	. push_back(dataSet()->data(dataSet()->index(0, i), int(dataPkgRoles::columnType)).toInt());

		for (int j = _start; j < _start + _count; j++)
			if (j < dataSet()->rowCount())
			{
				_values[i]	. push_back(dataSet()->data(dataSet()->index(j, i), int(dataPkgRoles::value)).toString());
				_labels[i]	. push_back(dataSet()->data(dataSet()->index(j, i), int(dataPkgRoles::label)).toString());
			}
	}

	dataSet()->removeRows(_start, _count);
}

PasteSpreadsheetCommand::PasteSpreadsheetCommand(DataSet * data, int row, int col, 
	const std::vector<std::vector<QString> > & values, const std::vector<std::vector<QString> > & labels, const std::vector<boolvec> & selected, const QStringList& colNames)
	: UndoModelCommand(data), _row{row}, _col{col}, _newValues{values}, _newLabels{labels}, _newColNames{colNames}, _selected{selected}
{
	JASPTIMER_SCOPE(PasteSpreadsheetCommand::PasteSpreadsheetCommand);
	
	setText(QObject::tr("Paste values at row '%1' column '%2'").arg(rowName(_row)).arg(columnName(_col)));
	
	auto isSelected = [&](int R, int C)
	{
		return _selected.size() == 0 || _selected[C][R];
	};

	for (int c = 0; c < _newValues.size(); c++)
	{
		_oldValues.push_back({});
		_oldLabels.push_back({});
		
		_oldColNames.push_back(dataSet()->headerData(_col + c, Qt::Horizontal).toString());
		for (int r = 0; r < _newValues[c].size(); r++)
		{
			_oldValues[c].push_back(!isSelected(r,c) ? "" : dataSet()->data(dataSet()->index(_row + r, _col + c),	int(dataPkgRoles::value)).toString());
			_oldLabels[c].push_back(!isSelected(r,c) ? "" : dataSet()->data(dataSet()->index(_row + r, _col + c),	int(dataPkgRoles::label)).toString());
		}
	}
}

void PasteSpreadsheetCommand::undo()
{
	dataSet()->pasteSpreadsheet(_row, _col, _oldValues, _oldLabels, {}, _oldColNames, _selected);
}

void PasteSpreadsheetCommand::redo()
{
	dataSet()->pasteSpreadsheet(_row, _col, _newValues, _newLabels, {}, _newColNames, _selected);
}



SetColumnTypeCommand::SetColumnTypeCommand(DataSet * data, stringset cols, int colType)
	: UndoModelCommandMultipleColumns(data, cols), _newColType{colType}
{
	setText(QObject::tr("Set type to '%1' for column(s) '%2'").arg(columnTypeToQString(columnType(colType)), tql(_cols).join(", ")));
}

void SetColumnTypeCommand::redo()
{
	dataSet()->setColumnTypes(_cols, columnType(_newColType));
}


ColumnReverseValuesCommand::ColumnReverseValuesCommand(DataSet * data, stringset cols)
: UndoModelCommandMultipleColumns(data, cols)
{
	setText(QObject::tr("Reverse values of column(s) '%1'").arg(tql(cols).join(", ")));
}

void ColumnReverseValuesCommand::redo()
{
	if(!dataSetStillExists())
		Log::log() << "Dataset of id " << _dataSetID << " is gone!\nSo skipping redo " << text() << std::endl;
	else
		dataSet()->columnsReverseValues(_cols);
}

ColumnToggleAutoSortByValuesCommand::ColumnToggleAutoSortByValuesCommand(DataSet * data, stringset cols)
: UndoModelCommandMultipleColumns(data, cols)
{
	QStringList columnNames = tql(cols);
	
	for(const std::string & col : cols)
		_colsNewAutoSort[col] = dataSet()->column(col) && !dataSet()->column(col)->autoSortByValue();
	
	setText(QObject::tr("Toggle autosorting labels by values for column(s) '%1'").arg(columnNames.join(", ")));
}

void ColumnToggleAutoSortByValuesCommand::redo()
{
	if(!dataSetStillExists())
		Log::log() << "Dataset of id " << _dataSetID << " is gone!\nSo skipping redo " << text() << std::endl;
	else
		dataSet()->columnsSetAutoSortForColumns(_colsNewAutoSort);
}

UndoModelCommandMultipleColumns::UndoModelCommandMultipleColumns(DataSet * data, stringset cols, bool serialize)
: UndoModelCommand(data), _cols(cols)
{
	JASPTIMER_SCOPE(UndoModelCommandMultipleColumns::UndoModelCommandMultipleColumns);
	
	if(serialize)
		for(const std::string & col : _cols)
			_serializedColumns[col] = dataSet()->column(col) ? dataSet()->column(col)->serialize() : Json::nullValue;
}

void UndoModelCommandMultipleColumns::undo()
{
	JASPTIMER_SCOPE(UndoModelCommandMultipleColumns::undo);
	
	for(const std::string & col : _cols)
		if(_serializedColumns.count(col) && !_serializedColumns[col].isNull())
        {
			Column *	column	= dataSet()->column(col);
			if(!column)
				continue;
            QString		oldName	= tq(column->name());
						column	->deserialize(_serializedColumns[col]);
						column	->data()->emitColumnChanged(oldName);
						column	->data()->emitColumnChanged(tq(column->name()));
        }

	dataSet()->refresh();
}

SetColumnPropertyCommand::SetColumnPropertyCommand(Column * column, QVariant newValue, ColumnProperty prop)
	: UndoModelCommandSingleColumn(column), _prop(prop), _newValue{newValue}
{
	assert(column);
	
	switch (_prop)
	{
	case ColumnProperty::Name:
		_oldValue = columnName();
		setText(QObject::tr("Change column name of '%1' from '%2' to '%3'").arg(columnName(), _oldValue.toString(), _newValue.toString()));
		break;
	
	case ColumnProperty::Title:
		_oldValue = column->titleQ();
		setText(QObject::tr("Change column title of '%1' from '%2' to '%3'").arg(columnName(), _oldValue.toString(), _newValue.toString()));
		break;
	
	case ColumnProperty::Description:
		_oldValue = column->descriptionQ();
		setText(QObject::tr("Change column description of '%1' from '%2' to '%3'").arg(columnName(), _oldValue.toString(), _newValue.toString()));
		break;
	
	case ColumnProperty::ComputedColumnType:
		_oldValue = int(column->codeType());
		setText(QObject::tr("Set computed type of '%1' from '%2' to '%3'").arg(columnName(), friendlyColumnType(_oldValue.toInt()), friendlyColumnType(_newValue.toInt())));
		break;
		
	case ColumnProperty::ComputeFilter:
		_oldValue = tq(column->computeFilter());
		setText(QObject::tr("Change column compute filter of '%1' from '%2' to '%3'").arg(columnName(), _oldValue.toString(), _newValue.toString()));
		break;
		
	case ColumnProperty::DropLevels:
		_oldValue = dropLevelsTypeToQString(column->dropLevels()); //Store the name, not the int: undo restores via dropLevelsTypeFromQString.
		setText(QObject::tr("Change column drop levels of '%1' from '%2' to '%3'").arg(columnName(), _oldValue.toString(), _newValue.toString()));
		break;
	}
}

void SetColumnPropertyCommand::undo()
{
	assert(dataSetStillExists());
			
	switch (_prop)
	{
	case ColumnProperty::Name:
		// In case that the command that deletes a column is undone, the id may change.
		// As the column can be also recognize with its name, use it.
		if(Column * col = dataSet()->column(fq(_newValue.toString())))
			col->setNameManually(_oldValue.toString());
		break;
		
	case ColumnProperty::Title:
		column()->setTitleQ(_oldValue.toString());
		break;
		
	case ColumnProperty::ComputeFilter:
		column()->setComputeFilter(fq(_oldValue.toString()));
		break;
		
	case ColumnProperty::Description:
		column()->setDescription(fq(_oldValue.toString()));
		break;

	case ColumnProperty::ComputedColumnType:
		column()->setCodeType(computedColumnType(_oldValue.toInt()));
		break;
		
	case ColumnProperty::DropLevels:
		column()->setDropLevels(dropLevelsTypeFromQString(_oldValue.toString()));
		break;
		
	case ColumnProperty::HasLabels:
		column()->setHasLabels(_oldValue.toBool());
		break;
	}
}

void SetColumnPropertyCommand::redo()
{
	if(!dataSetStillExists())
	{
		Log::log() << "Dataset of id " << _dataSetID << " is gone!\nSo skipping redo " << text() << std::endl;
		return;
	}
			
	switch (_prop)
	{
	case ColumnProperty::Name:
		// In case that the command that deletes a column is undone, the id may change.
		// As the column can be also recognize with its name, use it.
		if(Column * col = dataSet()->column(fq(_oldValue.toString())))
			col->setNameManually(_newValue.toString());
		break;
		
	case ColumnProperty::Title:
		column()->setTitleQ(_newValue.toString());
		break;
		
	case ColumnProperty::ComputeFilter:
		column()->setComputeFilter(fq(_newValue.toString()));
		break;
		
	case ColumnProperty::Description:
		column()->setDescription(fq(_newValue.toString()));
		break;

	case ColumnProperty::ComputedColumnType:
		column()->setCodeType(computedColumnType(_newValue.toInt()));
		break;
		
	case ColumnProperty::DropLevels:
		column()->setDropLevels(dropLevelsTypeFromQString(_newValue.toString()));
		break;
		
	case ColumnProperty::HasLabels:
		column()->setHasLabels(_newValue.toBool());
		break;
	}
}

QString SetColumnPropertyCommand::friendlyColumnType(int type)
{
	return Column::columnTypeFriendlyName(computedColumnType(type));
}


SetWorkspacePropertyCommand::SetWorkspacePropertyCommand(DataSet * data, QVariant newValue, WorkspaceProperty prop)
	: UndoModelCommand(data), _prop(prop), _newValue{newValue}
{
	if (prop == WorkspaceProperty::Description)
	{
		_oldValue = dataSet()->descriptionQ();
		setText(QObject::tr("Change workspace description from '%1' to '%2'").arg(_oldValue.toString(), _newValue.toString()));
	}
	else
		// No other properties are settable
		setObsolete(true);
}

void SetWorkspacePropertyCommand::undo()
{
	if (_prop == WorkspaceProperty::Description)
		dataSet()->setDescriptionQ(_oldValue.toString());
}

void SetWorkspacePropertyCommand::redo()
{
	if (_prop == WorkspaceProperty::Description)
		dataSet()->setDescriptionQ(_newValue.toString());
}

SetLabelCommand::SetLabelCommand(Column * column, int labelIndex, QString newLabel)
    : UndoModelCommandSingleColumn(column), _labelIndex{labelIndex}, _newLabel{newLabel}
{
			_oldLabel	= column->data(column->index(_labelIndex, 0)).toString();
	QString value		= column->data(column->index(_labelIndex, 0), int(dataPkgRoles::label)).toString();
	
	setText(QObject::tr("Set label for value '%1' of column '%2' from '%3' to '%4'").arg(value).arg(columnName()).arg(_oldLabel).arg(_newLabel));
}

void SetLabelCommand::redo()
{
    UndoModelCommandSingleColumn::redo(); //Makes sure we select the right column first
	if(column())
		column()->setData(column()->index(_labelIndex, 0), _newLabel, int(dataPkgRoles::label));
}

SetLabelOriginalValueCommand::SetLabelOriginalValueCommand(Column * column, int labelIndex, QString originalValue)
    : UndoModelCommandSingleColumn(column), _labelIndex{labelIndex}, _newOriginalValue{originalValue}
{

	_oldOriginalValue	= column->data(column->index(_labelIndex, 0), int(dataPkgRoles::value)).toString();
	_oldLabel			= column->data(column->index(_labelIndex, 0), int(dataPkgRoles::label)).toString();
	setText(QObject::tr("Set original value  from '%3' to '%4' for label '%1' of column '%2'").arg(_oldLabel).arg(columnName()).arg(_oldOriginalValue).arg(_newOriginalValue));
}

void SetLabelOriginalValueCommand::redo()
{
    UndoModelCommandSingleColumn::redo(); //Makes sure we select the right column first
	if(column())
		column()->setData(column()->index(_labelIndex, 0), _newOriginalValue, int(dataPkgRoles::value));
}

DeleteLabelCommand::DeleteLabelCommand(Column * column, int labelIndex)
	: UndoModelCommandSingleColumn(column), _labelIndex(labelIndex)
{
	setText(QObject::tr("Delete label %1 of column '%2'").arg(_labelIndex).arg(columnName()));
}

void DeleteLabelCommand::redo()
{
    UndoModelCommandSingleColumn::redo(); //Makes sure we select the right column first
	column()->deleteLabelManually(_labelIndex);
}

AddLabelCommand::AddLabelCommand(Column * column, QString value, QString label)
: UndoModelCommandSingleColumn(column), _value(value), _label(label)
{
	setText(QObject::tr("Adding value + label '%1' + '%2' to column '%3'").arg(_value).arg(_label).arg(columnName()));
}

void AddLabelCommand::redo()
{
    UndoModelCommandSingleColumn::redo(); //Makes sure we select the right column first
	column()->addLabelManually(_value, _label);
}

FilterLabelCommand::FilterLabelCommand(Column * column, int labelIndex, bool checked)
	: UndoModelCommandSingleColumn(column, false), _labelIndex{labelIndex}, _checked{checked}

{
	QString label = column->data(column->index(_labelIndex, 0)).toString();
	if (checked)
		setText(QObject::tr("Filter rows having label '%1' in column '%2'").arg(label).arg(columnName()));
	else
		setText(QObject::tr("Remove filter for rows having label '%1' in column '%2'").arg(label).arg(columnName()));

}

void FilterLabelCommand::undo()
{
	Column * col = dataSet()->column(_colId);
	if(col)
	{
		dataSet()->setShownColumn(col);
		col->setData(col->index(_labelIndex, 0), !_checked, int(dataPkgRoles::filter));
	}
}

void FilterLabelCommand::redo()
{
	Column * col = dataSet()->column(_colId);
	if(col)
	{
		dataSet()->setShownColumn(col);
		col->setData(col->index(_labelIndex, 0), _checked, int(dataPkgRoles::filter));
	}
}

MoveLabelCommand::MoveLabelCommand(Column * col, const std::vector<size_t> &indexes, bool up)
    : UndoModelCommandSingleColumn(col), _up{up}
{

	_labels.clear();
	_originalValues.clear();

	QStringList allLabels = tq(column()->nonEmptyLevelsStrings());
	for (int i : indexes)
	{
		if (i < allLabels.count())
		{
			_labels.push_back(allLabels[i]);
			if(Label * l = column()->labelByIndexNonEmpty(i))
				_originalValues.push_back(tq(l->originalValueAsString()));
		}
	}

	if (_labels.size() == 1)
	{
		QString label = _labels[0];
		if (_up)
			setText(QObject::tr("Move up label '%1' of column '%2'").arg(label).arg(columnName()));
		else
			setText(QObject::tr("Move down label '%1' of column '%2'").arg(label).arg(columnName()));
	}
	else
	{
		if (_up)
			setText(QObject::tr("Move up labels of column '%1'").arg(columnName()));
		else
			setText(QObject::tr("Move down labels of column '%1'").arg(columnName()));
	}

}

std::vector<qsizetype> MoveLabelCommand::_getIndexes()
{
	std::vector<qsizetype> indexes;
	//Match by original value instead of display string so that labels sharing a display
	//text (multiple values mapped to one label) still resolve to the correct row.
	for(const QString & originalValue : _originalValues)
	{
		for(int i=0; i<(int)column()->labelsNonEmptyCount(); i++)
		{
			Label * label = column()->labelByIndexNonEmpty(i);
			if(label && tq(label->originalValueAsString()) == originalValue)
			{
				indexes.push_back(i);
				break;
			}
		}
	}

	return indexes;
}

void MoveLabelCommand::redo()
{
    UndoModelCommandSingleColumn::redo(); //Makes sure we select the right column first
	std::vector<qsizetype> indexes = _getIndexes(); // The indexes must be recalculated each time
	column()->labelsMoveRows(indexes, _up); //through DataSetPackage to make sure signals get sent
}

ReverseLabelCommand::ReverseLabelCommand(Column * col)
	: UndoModelCommandSingleColumn(col, false)
{
	
	setText(QObject::tr("Reverse labels of column '%1'").arg(columnName()));
}

void ReverseLabelCommand::undo()
{
	redo();
}

void ReverseLabelCommand::redo()
{
	dataSet()->setShownColumn(column());
	column()->labelsReverse();
}

SetJsonFilterCommand::SetJsonFilterCommand(Filter * filter, const QString& newJsonValue)
	: UndoModelCommand(), _filter{filter}, _newJsonValue{newJsonValue}
{
	setText(QObject::tr("Change drag and drop filter"));
}

void SetJsonFilterCommand::undo()
{
	if(_filter)
		_filter->setConstructorJsonQ(_oldJsonValue);
}

void SetJsonFilterCommand::redo()
{
	if(!_filter)
		return;

	_oldJsonValue = _filter->constructorJsonQ();
	_filter->setConstructorJsonQ(_newJsonValue);
}

SetRFilterCommand::SetRFilterCommand(Filter * filter, const QString& newRFilter)
	: UndoModelCommand(), _filter{filter}, _newRFilter{newRFilter}
{
	setText(QObject::tr("Change R filter"));
}

void SetRFilterCommand::undo()
{
	if(_filter)
		_filter->setRFilterQ(_oldRFilter);
}

void SetRFilterCommand::redo()
{
	if(!_filter)
		return;

	_oldRFilter = _filter->rFilterQ();
	_filter->setRFilterQ(_newRFilter);
}

CreateComputedColumnCommand::CreateComputedColumnCommand(DataSet * data, const QString &name, columnType colType, computedColumnType codeType)
	: UndoModelCommand(data), _name{name}, _columnType{colType}, _computedColumnType{codeType}
{
	setText(QObject::tr("Create a computed column with name '%1'").arg(name));
}

void CreateComputedColumnCommand::undo()
{
	dataSet()->removeColumn(_name.toStdString());
}

void CreateComputedColumnCommand::redo()
{
	dataSet()->createComputedColumn(fq(_name), columnType(_columnType), computedColumnType(_computedColumnType));
}

SetComputedColumnCodeCommand::SetComputedColumnCodeCommand(Column * col, const QString& rCode, const QString& jsonCode)
	: UndoModelCommandSingleColumn(col, false)
	, _newRCode{	rCode}
	, _oldRCode{	col->rCodeQ()}
	, _newJsonCode{	jsonCode}
	, _oldJsonCode{ tq(col->constructorJsonStr())}
{
	setText(QObject::tr("Set code to computed column with name '%1'").arg(columnName()));
}

void SetComputedColumnCodeCommand::undo()
{
	dataSet()->setShownColumn(column());
	column()->setConstructorJson(fq(_oldJsonCode));
	column()->setRCodeQ(_oldRCode);
}

void SetComputedColumnCodeCommand::redo()
{
	dataSet()->setShownColumn(column());
	
	column()->setConstructorJson(fq(_newJsonCode));
	column()->setRCodeQ(_newRCode);
}

CopyColumnsCommand::CopyColumnsCommand(DataSet * data, int startCol, const std::vector<Json::Value>& copiedColumns)
	: UndoModelCommand(data), _startCol{startCol}, _copiedColumns{copiedColumns}
{
	if (copiedColumns.size() == 0)
		setObsolete(true);
	else
	{
		QString firstColName = tq(copiedColumns[0]["name"].asString());
		if (copiedColumns.size() == 1)
			setText(QObject::tr("Copy column '%1' into column number '%2'").arg(firstColName).arg(startCol));
		else
		{
			QString lastColName = tq(copiedColumns[copiedColumns.size() - 1]["name"].asString());
			setText(QObject::tr("Copy columns '%1' to '%2'").arg(firstColName).arg(lastColName));
		}
	}
}

void CopyColumnsCommand::undo()
{
	int colMax = dataSet()->columnCount();
	
	assert(dataSetStillExists());

	for (int i = 0; i < _originalColumns.size() && _startCol+i < colMax; i++)
		column(_startCol+i)->deserialize(_originalColumns[i]);
}

void CopyColumnsCommand::redo()
{
	int colMax = dataSet()->columnCount();
	_originalColumns.clear();
	
	assert(dataSetStillExists());

	for (int i = 0; i < _copiedColumns.size() && _startCol+i < colMax; i++)
		_originalColumns.push_back(column(_startCol + i)->serialize());

	for (int i = 0; i < _copiedColumns.size() && _startCol+i < colMax; i++)
		column(_startCol+i)->deserialize(_copiedColumns[i]);
	
}

SetUseCustomEmptyValuesCommand::SetUseCustomEmptyValuesCommand(Column * col, bool useCustom)
	: UndoModelCommandSingleColumn(col, false), _useCustom{useCustom}
{
	setText(QObject::tr(_useCustom ? "Use custom empty values for column '%1'" : "Use default empty values for column '%1'").arg(columnName()));	
}

void SetUseCustomEmptyValuesCommand::undo()
{
	column()->setHasCustomEmptyValues(!_useCustom);
}

void SetUseCustomEmptyValuesCommand::redo()
{
	column()->setHasCustomEmptyValues(_useCustom);
}

SetCustomEmptyValuesCommand::SetCustomEmptyValuesCommand(Column * col, const QStringList& customEmptyValues)
	: UndoModelCommandSingleColumn(col, false)
{
	
	_oldCustomEmptyValues = col->emptyValues()->emptyStrings();
	_newCustomEmptyValues.clear();
	for (const QString& val : customEmptyValues)
		_newCustomEmptyValues.insert(fq(val));

	setText(QObject::tr("Set empty values for column '%1'").arg(columnName()));
}

void SetCustomEmptyValuesCommand::undo()
{
	column()->setCustomEmptyValues(_oldCustomEmptyValues);
}

void SetCustomEmptyValuesCommand::redo()
{
	column()->setCustomEmptyValues(_newCustomEmptyValues);
}

SetWorkspaceEmptyValuesCommand::SetWorkspaceEmptyValuesCommand(DataSet * data, const QStringList& emptyValues)
	: UndoModelCommand(data)
{
	_oldEmptyValues = data->emptyValuesAsStrings();
	_newEmptyValues = fql(emptyValues);
	
	setText(QObject::tr("Set workspace empty values"));
}

void SetWorkspaceEmptyValuesCommand::undo()
{
	dataSet()->setEmptyValuesFromStrings(_oldEmptyValues);
}

void SetWorkspaceEmptyValuesCommand::redo()
{
	dataSet()->setEmptyValuesFromStrings(_newEmptyValues);
}


UndoModelCommand::UndoModelCommand(DataSet * data)
	: QUndoCommand(UndoStack::singleton()->parentCommand())
{
	_dataSetID = data ? data->id() : -1;
}

QString UndoModelCommand::columnName(int colIndex) const
{	
	if(colIndex == -1)
		return "???";
	
	if(dataSet()->column(colIndex))
		return dataSet()->column(colIndex)->nameQ();
	
	QString result = dataSet()->headerData(colIndex, Qt::Orientation::Horizontal).toString();
	if (result.isEmpty())
		result = QString::number(colIndex + 1);

	return result;
}

QString UndoModelCommand::rowName(int rowIndex) const
{
	QString result = dataSet()->headerData(rowIndex, Qt::Orientation::Vertical).toString();
	if (result.isEmpty())
		result = QString::number(rowIndex + 1);

	return result;
}

bool UndoModelCommand::dataSetStillExists() const
{
	return dataSet();
}

DataSet *UndoModelCommand::dataSet() const
{
	assert(_dataSetID > -1);
	
	DataSet * data = Workspace::singleton()->dataSetById(_dataSetID);
	
	return data;
}

Column *UndoModelCommand::column(int index) const
{
	assert(dataSetStillExists());
	
	return dataSet()->column(index);
}


UndoModelCommandSingleColumn::UndoModelCommandSingleColumn(Column * column, bool serialize)
	: UndoModelCommandMultipleColumns(column->data(), { column->name() }, serialize )
{
	_colId = column->nameQ();
}

Column *UndoModelCommandSingleColumn::column() const
{
	return dataSet()->column(_colId);
}

Column *UndoModelCommandSingleColumn::column()
{
	return dataSet()->column(_colId);
}

QString UndoModelCommandSingleColumn::columnName(int colIndex) const
{
	if(colIndex == -1)
		return column()->nameQ();
	
	return UndoModelCommand::columnName(colIndex);
}

void UndoModelCommandSingleColumn::redo()
{
    dataSet()->setShownColumn(dataSet()->column(_colId));
}

void UndoModelCommandSingleColumn::undo()
{
    UndoModelCommandMultipleColumns::undo();

    dataSet()->setShownColumn(dataSet()->column(_colId));
}
