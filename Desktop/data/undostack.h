#ifndef UNDOSTACK_H
#define UNDOSTACK_H

#include <QUndoStack>
#include <QAbstractItemModel>
#include <json/json.h>
#include "stringutils.h"

class ColumnModel;
class FilterModel;
class ComputedColumnModel;

class UndoModelCommand : public QUndoCommand
{
public:
	UndoModelCommand(QAbstractItemModel* model = nullptr);

	QString		columnName(int colIndex = -1)		const;
	QString		rowName(int rowIndex)				const;

protected:
	QAbstractItemModel*	_model = nullptr;
};

class SetColumnPropertyCommand: public UndoModelCommand
{
public:
	enum class ColumnProperty { Name, Title, Description, ComputedColumn };

	SetColumnPropertyCommand(QAbstractItemModel *model, QVariant newValue, ColumnProperty prop);

	void undo()					override;
	void redo()					override;

private:
	QString friendlyColumnType(int tyoe);

	ColumnProperty			_prop	= ColumnProperty::Name;
	int						_colId	= -1;
	QVariant				_newValue,
							_oldValue;
};

class SetWorkspacePropertyCommand: public UndoModelCommand
{
public:
	enum class WorkspaceProperty { Name, Description };

	SetWorkspacePropertyCommand(QAbstractItemModel *model, QVariant newValue, WorkspaceProperty prop);

	void undo()					override;
	void redo()					override;

private:

	WorkspaceProperty		_prop	= WorkspaceProperty::Description;
	QVariant				_newValue,
							_oldValue;
};


class SetLabelCommand: public UndoModelCommand
{
public:
	SetLabelCommand(QAbstractItemModel *model, int labelIndex, QString newLabel);
	
	void undo()					override;
	void redo()					override;
	
private:
	ColumnModel*			_columnModel = nullptr;
	int						_colId		= -1,
							_labelIndex = -1;
	QString					_newLabel,
							_oldLabel;
};

class SetLabelOriginalValueCommand: public UndoModelCommand
{
public:
	SetLabelOriginalValueCommand(QAbstractItemModel *model, int labelIndex, QString originalValue);
	
	void undo()					override;
	void redo()					override;
	
private:
	ColumnModel*			_columnModel = nullptr;
	int						_colId		= -1,
							_labelIndex = -1;
	QString					_newOriginalValue,
							_oldOriginalValue,
							_oldLabel;
};

class FilterLabelCommand: public UndoModelCommand
{
public:
	FilterLabelCommand(QAbstractItemModel *model, int labelIndex, bool checked);

	void undo()					override;
	void redo()					override;

private:
	ColumnModel*			_columnModel = nullptr;
	int						_colId		= -1,
							_labelIndex = -1;
	bool					_checked	= false;
};

class MoveLabelCommand: public UndoModelCommand
{
public:
	MoveLabelCommand(QAbstractItemModel *model, const std::vector<qsizetype>& indexes, bool up);

	void undo()					override;
	void redo()					override;

private:
	std::vector<qsizetype>	_getIndexes();
	void					_moveLabels(bool up);
	
	ColumnModel*			_columnModel = nullptr;
	int						_colId		= -1;
	QStringList				_labels;
	bool					_up			= false;
};

class ReverseLabelCommand: public UndoModelCommand
{
public:
	ReverseLabelCommand(QAbstractItemModel *model);

	void undo()					override;
	void redo()					override;

private:
	ColumnModel*			_columnModel = nullptr;
	int						_colId		= -1;
};

class SetJsonFilterCommand: public UndoModelCommand
{
public:
	SetJsonFilterCommand(QAbstractItemModel *model, FilterModel* filterModel, const QString& newJsonValue);

	void undo()					override;
	void redo()					override;

private:
	FilterModel*			_filterModel = nullptr;
	QString					_oldJsonValue,
							_newJsonValue;
};

class SetRFilterCommand: public UndoModelCommand
{
public:
	SetRFilterCommand(QAbstractItemModel *model, FilterModel* filterModel, const QString& newRValue);

	void undo()					override;
	void redo()					override;

private:
	FilterModel*			_filterModel = nullptr;
	QString					_oldRFilter,
							_newRFilter;
};

class CreateComputedColumnCommand: public UndoModelCommand
{
public:
	CreateComputedColumnCommand(QAbstractItemModel *model, const QString& name, int columnType, int computedColumnType);

	void undo()					override;
	void redo()					override;

private:
	QString					_name;
	int						_columnType				= -1;
	int						_computedColumnType		= -1;
};

class SetComputedColumnCodeCommand: public UndoModelCommand
{
public:
	SetComputedColumnCodeCommand(QAbstractItemModel *model, const std::string& name, const QString& rCode, const QString& jsonCode);

	void undo()					override;
	void redo()					override;

private:
        ComputedColumnModel*	_computedColumnModel = nullptr;
	std::string				_name;
	QString					_oldRCode,
							_newRCode,
							_oldJsonCode,
							_newJsonCode;
};

class SetDataCommand : public UndoModelCommand
{
public:
	SetDataCommand(QAbstractItemModel *model, int row, int col, const QVariant &value, int role);

	void undo()					override;
	void redo()					override;

private:
	QVariant				_oldValue,
							_newValue;
	int						_row		= -1,
							_col		= -1,
							_role		= -1,
							_newColType = -1,
							_oldColType = -1;
};

class UndoModelCommandMultipleColumns : public UndoModelCommand
{
public:
	UndoModelCommandMultipleColumns(QAbstractItemModel *model, intset cols);

	void undo()					override;

protected:
	intset						_cols;

private:
	std::map<int, Json::Value>	_serializedColumns;
};

class PasteSpreadsheetCommand : public UndoModelCommandMultipleColumns
{
public:
	PasteSpreadsheetCommand(QAbstractItemModel *model, int row, int col, const std::vector<std::vector<QString>>& values, const std::vector<std::vector<QString>>& labels, const std::vector<boolvec> & selected, const QStringList & colNames);


	void redo()					override;

private:
	std::vector<std::vector<QString>>	_values,
										_labels;
	std::vector<boolvec>				_selected;
	QStringList							_colNames;
	int									_row = -1,
										_col = -1;
};



class SetColumnTypeCommand : public UndoModelCommandMultipleColumns
{
public:
	SetColumnTypeCommand(QAbstractItemModel *model, intset cols, int colType);

	void redo()					override;

private:
	int							_newColType = -1;
};

class ColumnOrderByValuesCommand : public UndoModelCommandMultipleColumns
{
public:
	ColumnOrderByValuesCommand(QAbstractItemModel *model, intset cols);

	void redo()					override;
};

class ColumnReverseValuesCommand : public UndoModelCommand
{
public:
	ColumnReverseValuesCommand(QAbstractItemModel *model, intset cols);

	void undo()					override { redo(); }
	void redo()					override;
	
private:
	intset			_cols;
};

class InsertColumnCommand : public UndoModelCommand
{
public:
	InsertColumnCommand(QAbstractItemModel *model, int col, const QMap<QString, QVariant>& props = {});

	void undo()					override;
	void redo()					override;

private:
	int						_col		= -1;
	QMap<QString, QVariant>	_props;
};

class InsertRowsCommand : public UndoModelCommand
{
public:
	InsertRowsCommand(QAbstractItemModel *model, int row, int count = 1);

	void undo()					override;
	void redo()					override;

private:
	int						_row = -1,
							_count;
};

class RemoveColumnsCommand : public UndoModelCommand
{
public:
	RemoveColumnsCommand(QAbstractItemModel *model, int start, int count);

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
	RemoveRowsCommand(QAbstractItemModel *model, int start, int count);

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
	CopyColumnsCommand(QAbstractItemModel* model, int startCol, const std::vector<Json::Value>& copiedColumns);

	void undo()					override;
	void redo()					override;

private:
	int							_startCol = -1;
	std::vector<Json::Value>	_copiedColumns,
								_originalColumns;

};

class SetUseCustomEmptyValuesCommand: public UndoModelCommand
{
public:
	SetUseCustomEmptyValuesCommand(QAbstractItemModel* model, bool useCustom);

	void undo()					override;
	void redo()					override;

private:
	int							_colId = -1;
	bool						_useCustom = false;
};

class SetCustomEmptyValuesCommand: public UndoModelCommand
{
public:
	SetCustomEmptyValuesCommand(QAbstractItemModel* model, const QStringList& emptyValues);

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
	SetWorkspaceEmptyValuesCommand(QAbstractItemModel* model, const QStringList& emptyValues);

	void undo()					override;
	void redo()					override;

private:
	stringset					_newEmptyValues,
								_oldEmptyValues;
};

class UndoStack : public QUndoStack
{
	Q_OBJECT
public:
	UndoStack(QObject* parent = nullptr);

	static UndoStack*	singleton() { return _undoStack; }

	void				pushCommand(UndoModelCommand* command);
	void				startMacro(const QString& text = QString());
	void				endMacro(UndoModelCommand* command = nullptr);
	QUndoCommand*		parentCommand()		{ return _parentCommand; }

private:

	UndoModelCommand*			_parentCommand			= nullptr;

	static UndoStack*			_undoStack;

};

#endif // UNDOSTACK_H
