#include "undostack.h"
#include "log.h"
#include "datasettablemodel.h"
#include "columnmodel.h"
#include "filtermodel.h"
#include "computedcolumnmodel.h"
#include "utilities/qutils.h"
#include "columnutils.h"

UndoStack* UndoStack::_undoStack = nullptr;

UndoStack::UndoStack(QObject* parent) : QUndoStack(parent)
{
	_undoStack = this;

	connect(this, &QUndoStack::indexChanged, []() { DataSetPackage::pkg()->setModified(true); });
}

void UndoStack::pushCommand(UndoModelCommand *command)
{
	if (!_parentCommand) // Push to the stack only when no macro is started: in this case the command is autmatically added to the _parentCommand
		push(command);
}

void UndoStack::startMacro(const QString &text)
{
	if (_parentCommand)
		Log::log() << "Macro started though last one is not finished!" << std::endl;
	_parentCommand = new UndoModelCommand();
	if (!text.isEmpty())
		_parentCommand->setText(text);
}

void UndoStack::endMacro(UndoModelCommand *command)
{
	if (command)
	{
		if (_parentCommand)
			_parentCommand->setText(command->text());
		else
			push(command); // Case when macro was not started
	}
	if (_parentCommand)
		push(_parentCommand);

	_parentCommand = nullptr;
}


SetDataCommand::SetDataCommand(QAbstractItemModel *model, int row, int col, const QVariant &value, int role)
	: UndoModelCommand(model), _newValue{value}, _row{row}, _col{col}, _role{role}
{
	setText(QObject::tr("Set value to '%1' at row %2 column '%3'").arg(_newValue.toString()).arg(rowName(_row)).arg(columnName(_col)));
}

void SetDataCommand::undo()
{
	_model->setData(_model->index(_row, _col), _oldValue, _role);
	if (_oldColType != _newColType)
		_model->setData(_model->index(0, _col), _oldColType, int(dataPkgRoles::columnType));
}

void SetDataCommand::redo()
{
	_oldValue = _model->data(_model->index(_row, _col));
	if(fq(_oldValue.toString()) == EmptyValues::displayString())
		_oldValue = "";

	_oldColType = _model->data(_model->index(0, _col), int(dataPkgRoles::columnType)).toInt();

	_model->setData(_model->index(_row, _col), _newValue, _role);

	_newColType = _model->data(_model->index(0, _col), int(dataPkgRoles::columnType)).toInt();
}

InsertColumnCommand::InsertColumnCommand(QAbstractItemModel *model, int column, const QMap<QString, QVariant>& props)
	: UndoModelCommand(model), _col{column}, _props{props}
{
	QString colName = props.contains("name") ? props["name"].toString() : columnName(_col);
	setText(props.contains("computed") ? QObject::tr("Insert computed column '%1'").arg(_col) : QObject::tr("Insert column '%1'").arg(colName));
}

void InsertColumnCommand::undo()
{
	_model->removeColumn(_col);
}

void InsertColumnCommand::redo()
{
	DataSetTableModel * dataSetTable = dynamic_cast<DataSetTableModel *>(_model);

	if (dataSetTable)
		dataSetTable->insertColumnSpecial(_col, _props);
}

InsertRowCommand::InsertRowCommand(QAbstractItemModel *model, int row)
	: UndoModelCommand(model), _row{row}
{
	setText(QObject::tr("Insert row %1").arg(rowName(_row)));
}

void InsertRowCommand::undo()
{
	_model->removeRow(_row);
}

void InsertRowCommand::redo()
{
	_model->insertRow(_row);
}

RemoveColumnsCommand::RemoveColumnsCommand(QAbstractItemModel *model, int start, int count)
	: UndoModelCommand(model), _start{start}, _count{count}
{
	if (count == 1)
		setText(QObject::tr("Remove column '%1'").arg(columnName(_start)));
	else
		setText(QObject::tr("Remove %1 columns from '%2'").arg(_count).arg(columnName(_start)));
}

void RemoveColumnsCommand::undo()
{
	_model->insertColumns(_start, _count);
	for (int col = _start; col < _start + _count; col++)
		DataSetPackage::pkg()->deserializeColumn(columnName(col).toStdString(), _serializedColumns[col - _start]);
}

void RemoveColumnsCommand::redo()
{
	_serializedColumns.clear();

	if (_start + _count > _model->columnCount())
		_count = _model->columnCount() - _start;
	for (int col = _start; col < _start + _count; col++)
		_serializedColumns.push_back(DataSetPackage::pkg()->serializeColumn(columnName(col).toStdString()));
	_model->removeColumns(_start, _count);
}

RemoveRowsCommand::RemoveRowsCommand(QAbstractItemModel *model, int start, int count)
	: UndoModelCommand(model), _start{start}, _count{count}
{
	if (count == 1)
		setText(QObject::tr("Remove row %1").arg(rowName(_start)));
	else
		setText(QObject::tr("Remove rows %1 to %2").arg(rowName(_start), rowName(_start + count)));
}

void RemoveRowsCommand::undo()
{
	_model->insertRows(_start, _count);

	DataSetTableModel* dataSetTable = qobject_cast<DataSetTableModel*>(_model);

	if (dataSetTable)
		dataSetTable->pasteSpreadsheet(_start, 0, _values, _labels, _colTypes);
	else
	{
		for (int i = 0; i < _model->columnCount() && i < _values.size(); i++)
			for (int j = _start; j < _count; j++)
				_model->setData(_model->index(j, i), _values[i][j], 0);
	}
}

void RemoveRowsCommand::redo()
{
	_values		. clear();
	_labels		. clear();
	_colTypes	. clear();

	for (int i = 0; i < _model->columnCount(); i++)
	{
		_values		. push_back(std::vector<QString>());
		_labels		. push_back(std::vector<QString>());
		_colTypes	. push_back(_model->data(_model->index(0, i), int(dataPkgRoles::columnType)).toInt());

		for (int j = _start; j < _start + _count; j++)
			if (j < _model->rowCount())
			{
				_values[i]	. push_back(_model->data(_model->index(j, i), int(dataPkgRoles::value)).toString());
				_labels[i]	. push_back(_model->data(_model->index(j, i), int(dataPkgRoles::label)).toString());
			}
	}

	_model->removeRows(_start, _count);
}

PasteSpreadsheetCommand::PasteSpreadsheetCommand(QAbstractItemModel *model, int row, int col, const std::vector<std::vector<QString> > & values, const std::vector<std::vector<QString> > & labels, const QStringList& colNames)
	: UndoModelCommand(model), _row{row}, _col{col}, _newValues{values}, _newLabels{labels}, _newColNames{colNames}
{
	setText(QObject::tr("Paste values at row %1 column '%2'").arg(rowName(_row)).arg(columnName(_col)));
}

void PasteSpreadsheetCommand::undo()
{
	DataSetTableModel* dataSetTable = qobject_cast<DataSetTableModel*>(_model);

	if (dataSetTable)
		dataSetTable->pasteSpreadsheet(_row, _col, _oldValues, _oldLabels, {}, _oldColNames);
}

void PasteSpreadsheetCommand::redo()
{
	_oldValues.clear();
	_oldLabels.clear();
	for (int c = 0; c < _newValues.size(); c++)
	{
		_oldValues.push_back(std::vector<QString>());
		_oldLabels.push_back(std::vector<QString>());
		
		_oldColNames.push_back(_model->headerData(_col + c, Qt::Horizontal).toString());
		for (int r = 0; r < _newValues[c].size(); r++)
		{
			_oldValues[c].push_back(_model->data(_model->index(_row + r, _col + c),	int(DataSetPackage::specialRoles::value)).toString());
			_oldLabels[c].push_back(_model->data(_model->index(_row + r, _col + c),	int(DataSetPackage::specialRoles::label)).toString());
		}
	}

	DataSetTableModel* dataSetTable = qobject_cast<DataSetTableModel*>(_model);

	if (dataSetTable)
		dataSetTable->pasteSpreadsheet(_row, _col, _newValues, _newLabels, {}, _newColNames);
}


SetColumnTypeCommand::SetColumnTypeCommand(QAbstractItemModel *model, int col, int colType)
	: UndoModelCommand(model), _col{col}, _newColType{colType}
{
	_oldColType = _model->data(_model->index(0, _col), int(dataPkgRoles::columnType)).toInt();
	setText(QObject::tr("Set column '%2''s type to '%1'").arg(columnTypeToQString(columnType(colType)), columnName(col)));
}

void SetColumnTypeCommand::undo()
{
	DataSetPackage::pkg()->setColumnType(_col, columnType(_oldColType));
}

void SetColumnTypeCommand::redo()
{
	if (!DataSetPackage::pkg()->setColumnType(_col, columnType(_newColType)))
		setObsolete(true);
}


ColumnReverseValuesCommand::ColumnReverseValuesCommand(QAbstractItemModel *model, int col)
: UndoModelCommand(model), _col{col}
{
	setText(QObject::tr("Reverse column '%1''s values").arg(columnName(col)));
}

void ColumnReverseValuesCommand::redo()
{
	DataSetPackage::pkg()->columnReverseValues(_col);
}


SetColumnPropertyCommand::SetColumnPropertyCommand(QAbstractItemModel *model, QVariant newValue, ColumnProperty prop)
	: UndoModelCommand(model), _prop(prop), _newValue{newValue}
{
	ColumnModel* columnModel = qobject_cast<ColumnModel*>(model);
	if (columnModel)
	{
		_colId = columnModel->chosenColumn();

		switch (_prop)
		{
		case ColumnProperty::Name:
			_oldValue = columnModel->columnNameQ();
			setText(QObject::tr("Change column name of '%1' from '%2' to '%3'").arg(columnModel->columnNameQ(), _oldValue.toString(), _newValue.toString()));
			break;
		case ColumnProperty::Title:
			_oldValue = columnModel->columnTitle();
			setText(QObject::tr("Change column title of '%1' from '%2' to '%3'").arg(columnModel->columnNameQ(), _oldValue.toString(), _newValue.toString()));
			break;
		case ColumnProperty::Description:
			_oldValue = columnModel->columnDescription();
			setText(QObject::tr("Change column description of '%1' from '%2' to '%3'").arg(columnModel->columnNameQ(), _oldValue.toString(), _newValue.toString()));
			break;
		case ColumnProperty::ComputedColumn:
			_oldValue = int(computedColumnTypeFromQString(columnModel->computedType()));
			setText(QObject::tr("Set computed type of '%1' from '%2' to '%3'").arg(columnModel->columnNameQ(), friendlyColumnType(_oldValue.toInt()), friendlyColumnType(_newValue.toInt())));
			break;
		}
	}
	else
	{
		Log::log() << "Try to change the column name with the wrong model!" << std::endl;
		setObsolete(true);
	}
}

void SetColumnPropertyCommand::undo()
{
	switch (_prop)
	{
	case ColumnProperty::Name:
		// In case that the command that deletes a column is undone, the id may change.
		// As the column can be also recognize with its name, use it.
		DataSetPackage::pkg()->setColumnName(DataSetPackage::pkg()->getColumnIndex(_newValue.toString()), fq(_oldValue.toString()));
		break;
	case ColumnProperty::Title:
		DataSetPackage::pkg()->setColumnTitle(_colId, fq(_oldValue.toString()));
		break;
	case ColumnProperty::Description:
		DataSetPackage::pkg()->setColumnDescription(_colId, fq(_oldValue.toString()));
		break;
	case ColumnProperty::ComputedColumn:
		DataSetPackage::pkg()->setColumnComputedType(_colId, computedColumnType(_oldValue.toInt()));
		break;
	}
}

void SetColumnPropertyCommand::redo()
{
	switch (_prop)
	{
	case ColumnProperty::Name:
		DataSetPackage::pkg()->setColumnName(DataSetPackage::pkg()->getColumnIndex(_oldValue.toString()), fq(_newValue.toString()));
		break;
	case ColumnProperty::Title:
		DataSetPackage::pkg()->setColumnTitle(_colId, fq(_newValue.toString()));
		break;
	case ColumnProperty::Description:
		DataSetPackage::pkg()->setColumnDescription(_colId, fq(_newValue.toString()));
		break;
	case ColumnProperty::ComputedColumn:
		DataSetPackage::pkg()->setColumnComputedType(_colId, computedColumnType(_newValue.toInt()));
		break;
	}

}

QString SetColumnPropertyCommand::friendlyColumnType(int type)
{
	return ColumnModel::columnTypeFriendlyName(computedColumnType(type));
}


SetWorkspacePropertyCommand::SetWorkspacePropertyCommand(QAbstractItemModel *model, QVariant newValue, WorkspaceProperty prop)
	: UndoModelCommand(model), _prop(prop), _newValue{newValue}
{
	if (prop == WorkspaceProperty::Description)
	{
		_oldValue = DataSetPackage::pkg()->description();
		setText(QObject::tr("Change workspace description from '%1' to '%2'").arg(_oldValue.toString(), _newValue.toString()));
	}
	else
		// No other properties are settable
		setObsolete(true);
}

void SetWorkspacePropertyCommand::undo()
{
	if (_prop == WorkspaceProperty::Description)
		DataSetPackage::pkg()->setDescription(_oldValue.toString());
}

void SetWorkspacePropertyCommand::redo()
{
	if (_prop == WorkspaceProperty::Description)
		DataSetPackage::pkg()->setDescription(_newValue.toString());
}


SetLabelCommand::SetLabelCommand(QAbstractItemModel *model, int labelIndex, QString newLabel)
	: UndoModelCommand(model), _labelIndex{labelIndex}, _newLabel{newLabel}
{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId = _columnModel->chosenColumn();
		_oldLabel = _model->data(_model->index(_labelIndex, 0)).toString();
		QString value = _model->data(_model->index(_labelIndex, 0), int(DataSetPackage::specialRoles::label)).toString();
		setText(QObject::tr("Set label for value '%1' of column '%2' from '%3' to '%4'").arg(value).arg(_columnModel->columnNameQ()).arg(_oldLabel).arg(_newLabel));
	}
	else
	{
		Log::log() << "Try to set a label name with a wrong model!" << std::endl;
		setObsolete(true);
	}
}

void SetLabelCommand::undo()
{
	assert(_columnModel && _model);
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _oldLabel, int(DataSetPackage::specialRoles::label));
	_columnModel->setLabelMaxWidth();
}

void SetLabelCommand::redo()
{
	assert(_columnModel && _model);
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _newLabel, int(DataSetPackage::specialRoles::label));
	_columnModel->setLabelMaxWidth();
}

SetLabelOriginalValueCommand::SetLabelOriginalValueCommand(QAbstractItemModel *model, int labelIndex, QString originalValue)
	: UndoModelCommand(model), _labelIndex{labelIndex}, _newOriginalValue{originalValue}
{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId				= _columnModel->chosenColumn();
		_oldOriginalValue	= _model->data(_model->index(_labelIndex, 0), int(DataSetPackage::specialRoles::value)).toString();
		_oldLabel			= _model->data(_model->index(_labelIndex, 0), int(DataSetPackage::specialRoles::label)).toString();
		setText(QObject::tr("Set original value  from '%3' to '%4' for label '%1' of column '%2'").arg(_oldLabel).arg(_columnModel->columnNameQ()).arg(_oldOriginalValue).arg(_newOriginalValue));
	}
	else
	{
		Log::log() << "Try to set a label name with a wrong model!" << std::endl;
		setObsolete(true);
	}
}

void SetLabelOriginalValueCommand::undo()
{
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _oldOriginalValue,	int(DataSetPackage::specialRoles::value));
	_model->setData(_model->index(_labelIndex, 0), _oldLabel,			int(DataSetPackage::specialRoles::label));
	_columnModel->setLabelMaxWidth();
}

void SetLabelOriginalValueCommand::redo()
{
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _newOriginalValue, int(DataSetPackage::specialRoles::value));
	_columnModel->setLabelMaxWidth();
}



FilterLabelCommand::FilterLabelCommand(QAbstractItemModel *model, int labelIndex, bool checked)
	: UndoModelCommand(model), _labelIndex{labelIndex}, _checked{checked}

{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId = _columnModel->chosenColumn();
		QString label = _model->data(_model->index(_labelIndex, 0)).toString();
		if (checked)
			setText(QObject::tr("Filter rows having label '%1' in column '%2'").arg(label).arg(_columnModel->columnNameQ()));
		else
			setText(QObject::tr("Remove filter for rows having label '%1' in column '%2'").arg(label).arg(_columnModel->columnNameQ()));
	}
	else
	{
		Log::log() << "Try to set a label filter with a wrong model!" << std::endl;
		setObsolete(true);
	}
}

void FilterLabelCommand::undo()
{
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), !_checked, int(DataSetPackage::specialRoles::filter));
}

void FilterLabelCommand::redo()
{
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _checked, int(DataSetPackage::specialRoles::filter));
}

MoveLabelCommand::MoveLabelCommand(QAbstractItemModel *model, const std::vector<qsizetype> &indexes, bool up)
	: UndoModelCommand(model), _up{up}
{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId = _columnModel->chosenColumn();
		_labels.clear();

		QStringList allLabels = DataSetPackage::pkg()->getColumnLabelsAsStringList(_colId);
		for (int i : indexes)
		{
			if (i < allLabels.count())
				_labels.push_back(allLabels[i]);
		}

		if (_labels.size() == 1)
		{
			QString label = _labels[0];
			if (_up)
				setText(QObject::tr("Move up label '%1' of column '%2'").arg(label).arg(_columnModel->columnNameQ()));
			else
				setText(QObject::tr("Move down label '%1' of column '%2'").arg(label).arg(_columnModel->columnNameQ()));
		}
		else
		{
			if (_up)
				setText(QObject::tr("Move up labels of column '%1'").arg(_columnModel->columnNameQ()));
			else
				setText(QObject::tr("Move down labels of column '%1'").arg(_columnModel->columnNameQ()));
		}
	}
	else
	{
		Log::log() << "Try to move a label with a wrong model!" << std::endl;
		setObsolete(true);
	}
}

std::vector<qsizetype> MoveLabelCommand::_getIndexes()
{
	std::vector<qsizetype> indexes;
	QStringList allLabels = DataSetPackage::pkg()->getColumnLabelsAsStringList(_colId);
	for (const QString& label : _labels)
	{
		int i = allLabels.indexOf(label);
		if (i >= 0)
			indexes.push_back(i);
	}

	return indexes;
}

void MoveLabelCommand::_moveLabels(bool up)
{
	_columnModel->setChosenColumn(_colId);
	std::vector<qsizetype> indexes = _getIndexes(); // The indexes must be recalculated each time
	DataSetPackage::pkg()->labelMoveRows(_colId, indexes, up); //through DataSetPackage to make sure signals get sent
}

void MoveLabelCommand::undo()
{
	_moveLabels(!_up);
}

void MoveLabelCommand::redo()
{
	_moveLabels(_up);
}

ReverseLabelCommand::ReverseLabelCommand(QAbstractItemModel *model)
	: UndoModelCommand(model)
{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId = _columnModel->chosenColumn();
		setText(QObject::tr("Reverse labels of column '%2'").arg(_columnModel->columnNameQ()));
	}
	else
	{
		Log::log() << "Try to reverse a label with a wrong model!" << std::endl;
		setObsolete(true);
	}
}

void ReverseLabelCommand::undo()
{
	redo();
}

void ReverseLabelCommand::redo()
{
	_columnModel->setChosenColumn(_colId);
	DataSetPackage::pkg()->labelReverse(_colId); //through DataSetPackage to make sure signals get sent
}

SetJsonFilterCommand::SetJsonFilterCommand(QAbstractItemModel *model, FilterModel* filterModel, const QString& newJsonValue)
	: UndoModelCommand(model), _filterModel{filterModel}, _newJsonValue{newJsonValue}
{
	setText(QObject::tr("Change drag and drop filter"));
}

void SetJsonFilterCommand::undo()
{
	_filterModel->setConstructorJson(_oldJsonValue);
}

void SetJsonFilterCommand::redo()
{
	_oldJsonValue = _filterModel->constructorJson();
	_filterModel->setConstructorJson(_newJsonValue);
}

SetRFilterCommand::SetRFilterCommand(QAbstractItemModel *model, FilterModel* filterModel, const QString& newRFilter)
	: UndoModelCommand(model), _filterModel{filterModel}, _newRFilter{newRFilter}
{
	setText(QObject::tr("Change R filter"));
}

void SetRFilterCommand::undo()
{
	_filterModel->setRFilter(_oldRFilter);
}

void SetRFilterCommand::redo()
{
	_oldRFilter = _filterModel->rFilter();
	_filterModel->setRFilter(_newRFilter);
}

CreateComputedColumnCommand::CreateComputedColumnCommand(QAbstractItemModel *model, const QString &name, int columnType, int computedColumnType)
	: UndoModelCommand(model), _name{name}, _columnType{columnType}, _computedColumnType{computedColumnType}
{
	setText(QObject::tr("Create a computed column with name '%1'").arg(name));
}

void CreateComputedColumnCommand::undo()
{
	DataSetPackage::pkg()->removeColumn(_name.toStdString());
}

void CreateComputedColumnCommand::redo()
{
	DataSetPackage::pkg()->createComputedColumn(fq(_name), columnType(_columnType), computedColumnType(_computedColumnType));
}

SetComputedColumnCodeCommand::SetComputedColumnCodeCommand(QAbstractItemModel *model, const std::string& name, const QString& rCode, const QString& jsonCode)
	: UndoModelCommand(model), _name{name}, _newRCode{rCode}, _newJsonCode{jsonCode}
{
    _computedColumnModel = ComputedColumnModel::singleton();
	setText(QObject::tr("Set code to computed column with name '%1'").arg(tq(name)));
}

void SetComputedColumnCodeCommand::undo()
{
	_computedColumnModel->selectColumn(DataSetPackage::pkg()->getColumn(_name));
	_computedColumnModel->setComputeColumnJson(_oldJsonCode);
	_computedColumnModel->sendCode(_oldRCode);
}

void SetComputedColumnCodeCommand::redo()
{
	_computedColumnModel->selectColumn(DataSetPackage::pkg()->getColumn(_name));
	_oldJsonCode = _computedColumnModel->computeColumnJson();
	_oldRCode = _computedColumnModel->computeColumnRCode();

	_computedColumnModel->setComputeColumnJson(_newJsonCode);
	_computedColumnModel->sendCode(_newRCode);
}

CopyColumnsCommand::CopyColumnsCommand(QAbstractItemModel *model, int startCol, const std::vector<Json::Value>& copiedColumns)
	: UndoModelCommand(model), _startCol{startCol}, _copiedColumns{copiedColumns}
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
	int colMax = _model->columnCount();

	for (int i = 0; i < _originalColumns.size(); i++)
	{
		if (colMax > _startCol + 1)
			DataSetPackage::pkg()->deserializeColumn(columnName(_startCol + i).toStdString(), _originalColumns[i]);
	}
}

void CopyColumnsCommand::redo()
{
	int colMax = _model->columnCount();
	_originalColumns.clear();

	for (int i = 0; i < _copiedColumns.size(); i++)
	{
		if (colMax > _startCol + i)
			_originalColumns.push_back(DataSetPackage::pkg()->serializeColumn(columnName(_startCol + i).toStdString()));
	}

	for (int i = 0; i < _copiedColumns.size(); i++)
	{
		if (colMax > _startCol + i)
			DataSetPackage::pkg()->deserializeColumn(columnName(_startCol + i).toStdString(), _copiedColumns[i]);
	}
}

SetUseCustomEmptyValuesCommand::SetUseCustomEmptyValuesCommand(QAbstractItemModel *model, bool useCustom)
	: UndoModelCommand(model), _useCustom{useCustom}
{
	ColumnModel* columnModel = qobject_cast<ColumnModel*>(model);
	if (columnModel)
	{
		Column* col = columnModel->column();
		_colId = columnModel->chosenColumn();
		std::string colName = col->name();

		if (_useCustom)
			setText(QObject::tr("Use custom empty values for column '%1'").arg(tq(colName)));
		else
			setText(QObject::tr("Use default empty values for column '%1'").arg(tq(colName)));
	}
	else
		setObsolete(true);
}

void SetUseCustomEmptyValuesCommand::undo()
{
	DataSetPackage::pkg()->setColumnHasCustomEmptyValues(_colId, !_useCustom);
}

void SetUseCustomEmptyValuesCommand::redo()
{
	DataSetPackage::pkg()->setColumnHasCustomEmptyValues(_colId, _useCustom);
}

SetCustomEmptyValuesCommand::SetCustomEmptyValuesCommand(QAbstractItemModel *model, const QStringList& customEmptyValues)
	: UndoModelCommand(model)
{
	ColumnModel* columnModel = qobject_cast<ColumnModel*>(model);
	if (columnModel)
	{
		Column* col = columnModel->column();
		_colId = columnModel->chosenColumn();
		std::string colName = col->name();
		_oldCustomEmptyValues = col->emptyValues()->emptyStrings();
		_newCustomEmptyValues.clear();
		for (const QString& val : customEmptyValues)
			_newCustomEmptyValues.insert(fq(val));

		setText(QObject::tr("Set empty values for column '%1'").arg(tq(colName)));
	}
	else
		setObsolete(true);
}

void SetCustomEmptyValuesCommand::undo()
{
	DataSetPackage::pkg()->setColumnCustomEmptyValues(_colId, _oldCustomEmptyValues);
}

void SetCustomEmptyValuesCommand::redo()
{
	DataSetPackage::pkg()->setColumnCustomEmptyValues(_colId, _newCustomEmptyValues);
}

SetWorkspaceEmptyValuesCommand::SetWorkspaceEmptyValuesCommand(QAbstractItemModel *model, const QStringList& emptyValues)
	: UndoModelCommand(model)
{
	_oldEmptyValues = DataSetPackage::pkg()->workspaceEmptyValues();
	_newEmptyValues.clear();
	for (const QString& val : emptyValues)
		_newEmptyValues.insert(fq(val));

	setText(QObject::tr("Set workspace empty values"));
}

void SetWorkspaceEmptyValuesCommand::undo()
{
	DataSetPackage::pkg()->setWorkspaceEmptyValues(_oldEmptyValues);
}

void SetWorkspaceEmptyValuesCommand::redo()
{
	DataSetPackage::pkg()->setWorkspaceEmptyValues(_newEmptyValues);
}


UndoModelCommand::UndoModelCommand(QAbstractItemModel *model)
	: QUndoCommand(UndoStack::singleton()->parentCommand()), _model{model}
{
}

QString UndoModelCommand::columnName(int colIndex) const
{
	QString result = _model->headerData(colIndex, Qt::Orientation::Horizontal).toString();
	if (result.isEmpty())
		result = QString::number(colIndex + 1);

	return result;
}

QString UndoModelCommand::rowName(int rowIndex) const
{
	QString result = _model->headerData(rowIndex, Qt::Orientation::Vertical).toString();
	if (result.isEmpty())
		result = QString::number(rowIndex + 1);

	return result;
}
