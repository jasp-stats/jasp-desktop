#ifndef EXPANDDATAPROXYMODEL_H
#define EXPANDDATAPROXYMODEL_H

#include <QAbstractItemModel>
#include "utils.h"
#include <QUndoCommand>
#include <json/json.h>

class ExpandDataProxyModel;

class UndoModelCommand : public QUndoCommand
{
public:
	UndoModelCommand(ExpandDataProxyModel* proxyModel);

	QAbstractItemModel* sourceModel()							const;
	QString				columnName(int colIndex)				const;
	QString				rowName(int rowIndex)					const;

protected:
	ExpandDataProxyModel*					_proxyModel = nullptr;
};

class SetDataCommand : public UndoModelCommand
{
public:
	SetDataCommand(ExpandDataProxyModel *model, int row, int col, const QVariant &value, int role);

	void undo()					override;
	void redo()					override;

private:
	QVariant				_oldValue,
							_newValue;
	int						_row,
							_col,
							_role;
};

class PasteSpreadsheetCommand : public UndoModelCommand
{
public:
	PasteSpreadsheetCommand(ExpandDataProxyModel *model, int row, int col, const std::vector<std::vector<QString>>& cells, const QStringList& newColNames);

	void undo()					override;
	void redo()					override;

private:
	std::vector<std::vector<QString>>	_newCells,
										_oldCells;
	QStringList							_newColNames;
	int									_row,
										_col;
};

class SetColumnTypeCommand : public UndoModelCommand
{
public:
	SetColumnTypeCommand(ExpandDataProxyModel *model, int col, int colType);

	void undo()					override;
	void redo()					override;

private:
	int									_col,
										_newColType,
										_oldColType;
};


class InsertColumnCommand : public UndoModelCommand
{
public:
	InsertColumnCommand(ExpandDataProxyModel *model, int col, bool computed, bool R);

	void undo()					override;
	void redo()					override;

private:
	int						_col;
	bool					_computed,
							_R;
};

class InsertRowCommand : public UndoModelCommand
{
public:
	InsertRowCommand(ExpandDataProxyModel *model, int row);

	void undo()					override;
	void redo()					override;

private:
	int						_row;
};

class RemoveColumnCommand : public UndoModelCommand
{
public:
	RemoveColumnCommand(ExpandDataProxyModel *model, int col);

	void undo()					override;
	void redo()					override;

private:
	int						_col;
	Json::Value				_serializedColumn;
};

class RemoveRowCommand : public UndoModelCommand
{
public:
	RemoveRowCommand(ExpandDataProxyModel *model, int row);

	void undo()					override;
	void redo()					override;

private:
	int						_row;
	QVariantList			_values;
};

class ExpandDataProxyModel : public QObject
{
	Q_OBJECT

public:
	explicit ExpandDataProxyModel(QObject *parent);

	int					rowCount(bool includeVirtuals = true)														const;
	int					columnCount(bool includeVirtuals = true)													const;
	QVariant			headerData(	int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const;
	void				setData(	int row, int col, const QVariant &value, int role);
	Qt::ItemFlags		flags(int row, int column)																	const;
	QModelIndex			index(int row, int column, const QModelIndex &parent = QModelIndex())						const;
	QVariant			data(int row, int column, int role = Qt::DisplayRole)										const;
	bool				filtered(int row, int column)																const;
	bool				isRowVirtual(int row)																		const;
	bool				isColumnVirtual(int col)																	const;
	bool				expandDataSet()																				const { return _expandDataSet; }
	void				setExpandDataSet(bool expand)																{ _expandDataSet = expand; }

	void				setSourceModel(QAbstractItemModel* model);
	QAbstractItemModel*	sourceModel()																				const { return _sourceModel; }

	void				removeRows(int start, int count);
	void				removeColumns(int start, int count);
	void				removeRow(int row);
	void				removeColumn(int col);
	void				insertRow(int row);
	void				insertColumn(int col, bool computed, bool R);
	void				pasteSpreadsheet(int row, int col, const std::vector<std::vector<QString>> & cells, QStringList newColNames = QStringList());
	int					setColumnType(int columnIndex, int columnType);

	int					getRole(const std::string& roleName)														const;

	void				undo();
	void				redo();
	QString				undoText()			{ return _undoStack->undoText(); }
	QString				redoText()			{ return _undoStack->redoText(); }
	QUndoCommand*		parentCommand()		{ return _parentCommand; }
	void				columnDataTypeChanged(QString colName);

signals:
	void				undoChanged();

protected:
	void				_setRolenames();
	void				_expandIfNecessary(int row, int col);
	void				_pushCommand(UndoModelCommand* command);
	void				_startMacro(const QString& text = QString());
	void				_endMacro(UndoModelCommand* command = nullptr);

	QAbstractItemModel*			_sourceModel			= nullptr;
	bool						_expandDataSet			= false;
	QUndoStack*					_undoStack				= nullptr;
	UndoModelCommand*			_parentCommand			= nullptr;

	strintmap					_roleNameToRole;

	const int	EXTRA_COLS				= 5;
	const int	EXTRA_ROWS				= 10;
};

#endif // EXPANDDATAPROXYMODEL_H
