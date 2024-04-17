#ifndef EXPANDDATAPROXYMODEL_H
#define EXPANDDATAPROXYMODEL_H

#include <QAbstractItemModel>
#include "utils.h"
#include "undostack.h"

class ExpandDataProxyModel : public QObject
{
	Q_OBJECT

public:
	explicit					ExpandDataProxyModel(QObject *parent);

	int							rowCount(bool includeVirtuals = true)														const;
	int							columnCount(bool includeVirtuals = true)													const;
	QVariant					headerData(	int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const;
	void						setData(	int row, int col, const QVariant &value, int role);
	Qt::ItemFlags				flags(int row, int column)																	const;
	QModelIndex					index(int row, int column, const QModelIndex &parent = QModelIndex())						const;
	QVariant					data(int row, int column, int role = Qt::DisplayRole)										const;
	bool						filtered(int row, int column)																const;
	bool						isRowVirtual(int row)																		const;
	bool						isColumnVirtual(int col)																	const;
	bool						expandDataSet()																				const { return _expandDataSet; }
	void						setExpandDataSet(bool expand)																{ _expandDataSet = expand; }

	void						setSourceModel(QAbstractItemModel* model);
	QAbstractItemModel*			sourceModel()																				const { return _sourceModel; }

	void						removeRows(int start, int count);
	void						removeColumns(int start, int count);
	void						insertRows(int row, int count = 1);
	void						insertColumn(int col, bool computed, bool R);
	void						pasteSpreadsheet(int row, int col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> & labels, const QStringList& colNames = {}, const std::vector<boolvec> & selected = {});
	int							setColumnType(intset columnIndex, int columnType);
	void						columnReverseValues(intset columnIndexes);
	void						columnOrderByValues(intset columnIndexes);
	void						copyColumns(int startCol, const std::vector<Json::Value>& copiedColumns);
	Json::Value					serializedColumn(int col);

	int							getRole(const std::string& roleName)														const;

	void						undo()				{ _undoStack->undo(); }
	void						redo()				{ _undoStack->redo(); }
	QString						undoText()			{ return _undoStack->undoText(); }
	QString						redoText()			{ return _undoStack->redoText(); }

signals:
	void						undoChanged();

protected:
	void						_setRolenames();
	void						_expandIfNecessary(int row, int col);

	QAbstractItemModel*			_sourceModel			= nullptr;
	bool						_expandDataSet			= false;

	strintmap					_roleNameToRole;
	UndoStack*					_undoStack				= nullptr;

	const int	EXTRA_COLS				= 7;
	const int	EXTRA_ROWS				= 20;
};

#endif // EXPANDDATAPROXYMODEL_H
