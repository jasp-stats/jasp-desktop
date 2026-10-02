#ifndef UNDOSTACK_H
#define UNDOSTACK_H

#include "utils.h"
#include <QUndoStack>
#include <QPointer>
#include <json/json.h>
#include "columntype.h"
#include <QAbstractItemModel>

class Column;
class Filter;
class DataSet;
class Workspace;

class UndoModelCommand : public QUndoCommand
{
public:
						UndoModelCommand(DataSet * dataSet = nullptr);

	virtual QString		columnName(int colIndex = -1)		const;
	QString				rowName(int rowIndex)				const;
	
	bool				dataSetStillExists()				const;
	DataSet		*		dataSet()							const;
	Column		*		column(int index)					const;

protected:
	int					_dataSetID	= -1;
};


class UndoModelCommandMultipleColumns : public UndoModelCommand
{
public:
	UndoModelCommandMultipleColumns(DataSet * dataSet, stringset cols, bool serialize = true);

	void undo()					override;

protected:
	stringset						_cols;

private:
	std::map<std::string, Json::Value>	_serializedColumns;
};

class UndoModelCommandSingleColumn : public UndoModelCommandMultipleColumns
{
public:
    UndoModelCommandSingleColumn(Column * column, bool serialize = true);

	Column	* column();
	Column	* column() const;
	
	QString	columnName(int colIndex = -1)		const override;
	
    void    redo() override;
    void    undo() override;
	
protected:
	QString			_colId		= "";
};

class SetColumnPropertyCommand: public UndoModelCommandSingleColumn
{
public:
	enum class ColumnProperty { Name, Title, Description, ComputedColumnType, ComputeFilter, DropLevels, HasLabels };

	SetColumnPropertyCommand(Column * column, QVariant newValue, ColumnProperty prop);

	void undo()					override;
	void redo()					override;

private:
	QString friendlyColumnType(int tyoe);

	ColumnProperty			_prop		= ColumnProperty::Name;
	QVariant				_newValue,
							_oldValue;
};

class SetWorkspacePropertyCommand: public UndoModelCommand
{
public:
	enum class WorkspaceProperty { Name, Description };

	SetWorkspacePropertyCommand(DataSet * data, QVariant newValue, WorkspaceProperty prop);

	void undo()					override;
	void redo()					override;

private:

	WorkspaceProperty		_prop	= WorkspaceProperty::Description;
	QVariant				_newValue,
							_oldValue;
};

class FilterLabelCommand: public UndoModelCommandSingleColumn
{
public:
	FilterLabelCommand(Column * column, int labelIndex, bool checked);

	void redo()					override;
	void undo()					override;

private:
	int						_labelIndex = -1;
	bool					_checked	= false;
};

class ReverseLabelCommand: public UndoModelCommandSingleColumn
{
public:
	ReverseLabelCommand(Column * column);

	void redo()					override;
	void undo()					override;

};

class SetJsonFilterCommand: public UndoModelCommand
{
public:
	SetJsonFilterCommand(Filter * filter, const QString& newJsonValue);

	void undo()					override;
	void redo()					override;

private:
	QPointer<Filter>			_filter;
	QString					_oldJsonValue,
							_newJsonValue;
};

class SetRFilterCommand: public UndoModelCommand
{
public:
	SetRFilterCommand(Filter* filter, const QString& newRValue);

	void undo()					override;
	void redo()					override;

private:
	QPointer<Filter>			_filter;
	QString					_oldRFilter,
							_newRFilter;
};

class CreateComputedColumnCommand: public UndoModelCommand
{
public:
	CreateComputedColumnCommand(DataSet * dataSet, const QString& name, columnType colType, computedColumnType codeType);

	void undo()					override;
	void redo()					override;

private:
	QString					_name					= "";
	columnType				_columnType				= columnType::unknown;
	computedColumnType		_computedColumnType		= computedColumnType::notComputed;
};

class SetComputedColumnCodeCommand: public UndoModelCommandSingleColumn
{
public:
	SetComputedColumnCodeCommand(Filter * f, Column * column, const QString& rCode, const QString& jsonCode);

	void undo()					override;
	void redo()					override;

private:
	QString					_filterName,
							_oldRCode,
							_newRCode,
							_oldJsonCode,
							_newJsonCode;
};

class SetDataCommand : public UndoModelCommand
{
public:
	SetDataCommand(DataSet * dataset, int row, int col, const QVariant &newData, int role);

	void undo()					override;
	void redo()					override;

private:
	QVariant				_oldValue,
							_oldLabel,
							_newData;
	int						_row		= -1,
							_col		= -1,
							_role		= -1;
};


class DeleteLabelCommand: public UndoModelCommandSingleColumn
{
public:
	DeleteLabelCommand(Column * column, int labelIndex);
	
	void redo()					override;
	
private:
	int						_labelIndex = -1;
};

class AddLabelCommand: public UndoModelCommandSingleColumn
{
public:
	AddLabelCommand(Column * column, QString value, QString label);

	void redo()					override;

private:
	QString					_value,
							_label;
};


class SetLabelCommand: public UndoModelCommandSingleColumn
{
public:
    SetLabelCommand(Column * column, int labelIndex, QString newLabel);

    void redo()					override;

private:
    int						_labelIndex = -1;
    QString					_newLabel,
							_oldLabel;
};

class SetLabelOriginalValueCommand: public UndoModelCommandSingleColumn
{
public:
    SetLabelOriginalValueCommand(Column * column, int labelIndex, QString originalValue);

    void redo()					override;

private:
    int						_labelIndex = -1;
    QString					_newOriginalValue,
                            _oldOriginalValue,
                            _oldLabel;
};


class MoveLabelCommand: public UndoModelCommandSingleColumn
{
public:
    MoveLabelCommand(Column * column, const std::vector<size_t>& indexes, bool up);

    void redo()					override;

private:

    std::vector<qsizetype>  _getIndexes();
    void					_moveLabels(bool up);

    QStringList				_labels;
	QStringList				_originalValues; //unique-ish handles to re-locate labels after reordering
    bool					_up			= false;
};

class PasteSpreadsheetCommand : public UndoModelCommand
{
public:
	PasteSpreadsheetCommand(DataSet * dataset, int row, int col, const std::vector<std::vector<QString>>& values, const std::vector<std::vector<QString>>& labels, const std::vector<boolvec> & selected, const QStringList & colNames);

	void undo()					override;
	void redo()					override;

private:
	
	std::vector<std::vector<QString>>		_newValues,
											_newLabels,
											_oldValues,
											_oldLabels;
	std::vector<boolvec>					_selected;
	QStringList								_newColNames,
											_oldColNames;
	int										_row = -1,
											_col = -1;
};

class SetColumnTypeCommand : public UndoModelCommandMultipleColumns
{
public:
	SetColumnTypeCommand(DataSet * dataset, stringset cols, int colType);

	void redo()					override;

private:
	int							_newColType = -1;
};

class ColumnToggleAutoSortByValuesCommand : public UndoModelCommandMultipleColumns
{
public:
	ColumnToggleAutoSortByValuesCommand(DataSet * dataset, stringset cols);

	void redo()					override;
	
private:
	std::map<std::string, bool>			_colsNewAutoSort;
};

class ColumnReverseValuesCommand : public UndoModelCommandMultipleColumns
{
public:
	ColumnReverseValuesCommand(DataSet * dataset, stringset cols);

	void undo()					override { redo(); }
	void redo()					override;
};

class InsertColumnCommand : public UndoModelCommand
{
public:
	InsertColumnCommand(DataSet * dataset, int col, const QMap<QString, QVariant>& props = {});

	void undo()					override;
	void redo()					override;

private:
	int						_col		= -1;
	QMap<QString, QVariant>	_props;
};

class InsertColumnsCommand : public UndoModelCommand
{
public:
	InsertColumnsCommand(DataSet * dataset, int col, int count = 1);

	void undo()					override;
	void redo()					override;

private:
	int						_col = -1,
							_count;
};

class InsertRowsCommand : public UndoModelCommand
{
public:
	InsertRowsCommand(DataSet * dataset, int row, int count = 1);

	void undo()					override;
	void redo()					override;

private:
	int						_row = -1,
							_count;
};

class RemoveColumnsCommand : public UndoModelCommand
{
public:
	RemoveColumnsCommand(DataSet * dataset, int start, int count);

	void undo()					override;
	void redo()					override;

private:
	int							_start = -1,
								_count = 0;
	std::vector<Json::Value>	_serializedColumns;
};


class RemoveRowsCommand : public UndoModelCommand
{
public:
	RemoveRowsCommand(DataSet * dataset, int start, int count);

	void undo()					override;
	void redo()					override;

private:
	int									_start = -1,
										_count = 0;
	std::vector<std::vector<QString>>	_values,
										_labels;
	std::vector<int>					_colTypes;
};

class CopyColumnsCommand : public UndoModelCommand
{
public:
	CopyColumnsCommand(DataSet * dataset, int startCol, const std::vector<Json::Value>& copiedColumns);

	void undo()					override;
	void redo()					override;

private:
	int							_startCol = -1;
	std::vector<Json::Value>	_copiedColumns,
								_originalColumns;

};

class SetUseCustomEmptyValuesCommand: public UndoModelCommandSingleColumn
{
public:
	SetUseCustomEmptyValuesCommand(Column * column, bool useCustom);

	void undo()					override;
	void redo()					override;

private:
	int							_colId = -1;
	bool						_useCustom = false;
};

class SetCustomEmptyValuesCommand: public UndoModelCommandSingleColumn
{
public:
	SetCustomEmptyValuesCommand(Column * column, const QStringList& emptyValues);

	void undo()					override;
	void redo()					override;

private:
	int							_colId = -1;
	stringset					_newCustomEmptyValues,
								_oldCustomEmptyValues;
};

class SetWorkspaceEmptyValuesCommand: public UndoModelCommand
{
public:
	SetWorkspaceEmptyValuesCommand(DataSet * dataSet, const QStringList& emptyValues);

	void undo()					override;
	void redo()					override;

private:
	stringset					_newEmptyValues,
								_oldEmptyValues;
};

/*
class ChangeSelectionCommand: public UndoModelCommand
{
public:
	ChangeSelectionCommand(???);

	void undo()					override;
	void redo()					override;

private:
	stringset					_newEmptyValues,
								_oldEmptyValues;
};
*/

class UndoStack : public QUndoStack
{
	Q_OBJECT
public:
	UndoStack(QObject* parent = nullptr);
	///The stacks are owned by their DataSet, so the current one can be destroyed by a workspace teardown. Don't leave the singleton dangling.
	~UndoStack() override { if(_currentUndoStack == this) _currentUndoStack = nullptr; }

	static UndoStack*	singleton() { return _currentUndoStack; }
	static void			setCurrent(UndoStack* stack) { _currentUndoStack = stack; }

	void				pushCommand(UndoModelCommand* command);
	void				startMacro(const QString& text = QString());
	void				endMacro(UndoModelCommand* command = nullptr);
	QUndoCommand*		parentCommand()		{ return _parentCommand; }
	
private:

	UndoModelCommand*			_parentCommand			= nullptr;

	static UndoStack*			_currentUndoStack;

};

#endif // UNDOSTACK_H
