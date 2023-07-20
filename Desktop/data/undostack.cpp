#include "undostack.h"
#include "log.h"
#include "datasettablemodel.h"
#include "columnModel.h"
#include "filtermodel.h"
#include "computedcolumnsmodel.h"
#include "utilities/qutils.h"

UndoStack* UndoStack::_undoStack = nullptr;

UndoStack::UndoStack(QObject* parent) : QUndoStack(parent)
{
	_undoStack = this;
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
	_oldColType = _model->data(_model->index(0, _col), int(dataPkgRoles::columnType)).toInt();

	_model->setData(_model->index(_row, _col), _newValue, _role);

	_newColType = _model->data(_model->index(0, _col), int(dataPkgRoles::columnType)).toInt();
}

InsertColumnCommand::InsertColumnCommand(QAbstractItemModel *model, int column, bool computed, bool R)
	: UndoModelCommand(model), _col{column}, _computed{computed}, _R{R}
{
	setText(_computed ? QObject::tr("Insert computed column '%1'").arg(_col) : QObject::tr("Insert column '%1'").arg(columnName(_col)));
}

void InsertColumnCommand::undo()
{
	_model->removeColumn(_col);
}

void InsertColumnCommand::redo()
{
	DataSetTableModel * dataSetTable = dynamic_cast<DataSetTableModel *>(_model);

	if (dataSetTable)
		dataSetTable->insertColumnSpecial(_col, _computed, _R);
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

RemoveColumnCommand::RemoveColumnCommand(QAbstractItemModel *model, int col)
	: UndoModelCommand(model), _col{col}
{
	setText(QObject::tr("Remove column '%1'").arg(columnName(_col)));
}

void RemoveColumnCommand::undo()
{
	_model->insertColumn(_col);
	DataSetPackage::pkg()->deserializeColumn(columnName(_col).toStdString(), _serializedColumn);
}

void RemoveColumnCommand::redo()
{
	_serializedColumn = DataSetPackage::pkg()->serializeColumn(columnName(_col).toStdString());
	_model->removeColumn(_col);
}

RemoveRowCommand::RemoveRowCommand(QAbstractItemModel *model, int row)
	: UndoModelCommand(model), _row{row}
{
	setText(QObject::tr("Remove row %1").arg(rowName(_row)));
}

void RemoveRowCommand::undo()
{
	_model->insertRow(_row);

	for (int i = 0; i < _model->columnCount() && i < _values.count(); i++)
		_model->setData(_model->index(_row, i), _values[i], 0);
}

void RemoveRowCommand::redo()
{
	_values.clear();
	for (int i = 0; i < _model->columnCount(); i++)
		_values.push_back(_model->data(_model->index(_row, i)));

	_model->removeRow(_row);
}

PasteSpreadsheetCommand::PasteSpreadsheetCommand(QAbstractItemModel *model, int row, int col, const std::vector<std::vector<QString> > &cells, const QStringList &newColNames)
	: UndoModelCommand(model), _row{row}, _col{col}, _newCells{cells}, _newColNames{newColNames}
{
	setText(QObject::tr("Paste values at row %1 column '%2'").arg(rowName(_row)).arg(columnName(_col)));
}

void PasteSpreadsheetCommand::undo()
{
	DataSetTableModel* dataSetTable = qobject_cast<DataSetTableModel*>(_model);

	if (dataSetTable)
		dataSetTable->pasteSpreadsheet(_row, _col, _oldCells, _newColNames);
}

void PasteSpreadsheetCommand::redo()
{
	_oldCells.clear();
	for (int c = 0; c < _newCells.size(); c++)
	{
		_oldCells.push_back(std::vector<QString>());
		for (int r = 0; r < _newCells[c].size(); r++)
			_oldCells[c].push_back(_model->data(_model->index(_row + r, _col + c)).toString());
	}

	DataSetTableModel* dataSetTable = qobject_cast<DataSetTableModel*>(_model);

	if (dataSetTable)
		dataSetTable->pasteSpreadsheet(_row, _col, _newCells, _newColNames);
}


SetColumnTypeCommand::SetColumnTypeCommand(QAbstractItemModel *model, int col, int colType)
	: UndoModelCommand(model), _col{col}, _newColType{colType}
{
	setText(QObject::tr("Set type '%1' to column '%2'").arg(columnTypeToQString(columnType(colType))).arg(columnName(col)));
}

void SetColumnTypeCommand::undo()
{
	_model->setData(_model->index(0, _col), _oldColType, int(dataPkgRoles::columnType));
}

void SetColumnTypeCommand::redo()
{
	_oldColType = _model->data(_model->index(0, _col), int(dataPkgRoles::columnType)).toInt();
	_model->setData(_model->index(0, _col), _newColType, int(dataPkgRoles::columnType));
}

SetColumnPropertyCommand::SetColumnPropertyCommand(QAbstractItemModel *model, QString newValue, ColumnProperty prop)
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
			setText(QObject::tr("Change column name from '%1' to '%2'").arg(_oldValue).arg(_newValue));
			break;
		case ColumnProperty::Title:
			_oldValue = columnModel->columnTitle();
			setText(QObject::tr("Change column title from '%1' to '%2'").arg(_oldValue).arg(_newValue));
			break;
		case ColumnProperty::Description:
			_oldValue = columnModel->columnDescription();
			setText(QObject::tr("Change column description from '%1' to '%2'").arg(_oldValue).arg(_newValue));
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
		DataSetPackage::pkg()->setColumnName(DataSetPackage::pkg()->getColumnIndex(_newValue), fq(_oldValue));
		break;
	case ColumnProperty::Title:
		DataSetPackage::pkg()->setColumnTitle(_colId, fq(_oldValue));
		break;
	case ColumnProperty::Description:
		DataSetPackage::pkg()->setColumnDescription(_colId, fq(_oldValue));
		break;
	}
}

void SetColumnPropertyCommand::redo()
{
	switch (_prop)
	{
	case ColumnProperty::Name:
		DataSetPackage::pkg()->setColumnName(DataSetPackage::pkg()->getColumnIndex(_oldValue), fq(_newValue));
		break;
	case ColumnProperty::Title:
		DataSetPackage::pkg()->setColumnTitle(_colId, fq(_newValue));
		break;
	case ColumnProperty::Description:
		DataSetPackage::pkg()->setColumnDescription(_colId, fq(_newValue));
		break;
	}

}

SetLabelCommand::SetLabelCommand(QAbstractItemModel *model, int labelIndex, QString newLabel)
	: UndoModelCommand(model), _labelIndex{labelIndex}, _newLabel{newLabel}
{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId = _columnModel->chosenColumn();
		_oldLabel = _model->data(_model->index(_labelIndex, 0)).toString();
		QString value = _model->data(_model->index(_labelIndex, 0), int(DataSetPackage::specialRoles::value)).toString();
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
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _oldLabel);
	_columnModel->setLabelMaxWidth();
}

void SetLabelCommand::redo()
{
	_columnModel->setChosenColumn(_colId);
	_model->setData(_model->index(_labelIndex, 0), _newLabel);
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

MoveLabelCommand::MoveLabelCommand(QAbstractItemModel *model, const std::vector<size_t> &indexes, bool up)
	: UndoModelCommand(model), _up{up}
{
	_columnModel = qobject_cast<ColumnModel*>(model);
	if (_columnModel)
	{
		_colId = _columnModel->chosenColumn();

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

std::vector<size_t> MoveLabelCommand::_getIndexes()
{
	std::vector<size_t> indexes;
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
	std::vector<size_t> indexes = _getIndexes(); // The indexes must be recalculated each time
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
	setText(QObject::tr("Create a computed column with name '%1").arg(name));
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
	_computedColumnModel = ComputedColumnsModel::singleton();
	setText(QObject::tr("Set code to computed column with name '%1").arg(tq(name)));
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
