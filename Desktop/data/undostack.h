#ifndef UNDOSTACK_H
#define UNDOSTACK_H

#include <QUndoStack>
#include <QAbstractItemModel>
#include <json/json.h>

class ColumnModel;
class FilterModel;
class ComputedColumnsModel;

class UndoModelCommand : public QUndoCommand
{
public:
	UndoModelCommand(QAbstractItemModel* model = nullptr);

	QString				columnName(int colIndex)				const;
	QString				rowName(int rowIndex)					const;

protected:
	QAbstractItemModel*	_model = nullptr;
};

class SetColumnPropertyCommand: public UndoModelCommand
{
public:
	enum class ColumnProperty { Name, Title, Description };

	SetColumnPropertyCommand(QAbstractItemModel *model, QString newValue, ColumnProperty prop);

	void undo()					override;
	void redo()					override;

private:
	ColumnProperty			_prop	= ColumnProperty::Name;
	int						_colId	= -1;
	QString					_newValue,
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
	MoveLabelCommand(QAbstractItemModel *model, const std::vector<size_t>& indexes, bool up);

	void undo()					override;
	void redo()					override;

private:
	std::vector<size_t>		_getIndexes();
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
	ComputedColumnsModel*	_computedColumnModel = nullptr;
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

class PasteSpreadsheetCommand : public UndoModelCommand
{
public:
	PasteSpreadsheetCommand(QAbstractItemModel *model, int row, int col, const std::vector<std::vector<QString>>& cells, const QStringList& newColNames);

	void undo()					override;
	void redo()					override;

private:
	std::vector<std::vector<QString>>	_newCells,
										_oldCells;
	QStringList							_newColNames;
	int									_row = -1,
										_col = -1;
};

class SetColumnTypeCommand : public UndoModelCommand
{
public:
	SetColumnTypeCommand(QAbstractItemModel *model, int col, int colType);

	void undo()					override;
	void redo()					override;

private:
	int									_col		= -1,
										_newColType = -1,
										_oldColType = -1;
};


class InsertColumnCommand : public UndoModelCommand
{
public:
	InsertColumnCommand(QAbstractItemModel *model, int col, bool computed, bool R);

	void undo()					override;
	void redo()					override;

private:
	int						_col		= -1;
	bool					_computed	= false,
							_R			= false;
};

class InsertRowCommand : public UndoModelCommand
{
public:
	InsertRowCommand(QAbstractItemModel *model, int row);

	void undo()					override;
	void redo()					override;

private:
	int						_row = -1;
};

class RemoveColumnCommand : public UndoModelCommand
{
public:
	RemoveColumnCommand(QAbstractItemModel *model, int col);

	void undo()					override;
	void redo()					override;

private:
	int						_col = -1;
	Json::Value				_serializedColumn;
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
	std::vector<std::vector<QString>>	_values;
	std::vector<int>					_colTypes;
};

class RemoveRowCommand : public UndoModelCommand
{
public:
	RemoveRowCommand(QAbstractItemModel *model, int row);

	void undo()					override;
	void redo()					override;

private:
	int						_row = -1;
	QVariantList			_values;
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
