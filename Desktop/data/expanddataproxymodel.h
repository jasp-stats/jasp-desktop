#ifndef EXPANDDATAPROXYMODEL_H
#define EXPANDDATAPROXYMODEL_H

#include <QAbstractItemModel>
#include "utils.h"
#include "undostack.h"

class ExpandDataProxyModel : public QAbstractItemModel
{
	Q_OBJECT

public:
	explicit					ExpandDataProxyModel(QObject *parent);

	int							rowCount(			const QModelIndex &parent = QModelIndex())										const	override;
	int							columnCount(		const QModelIndex &parent = QModelIndex())										const	override;
	QVariant					data(				const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
	QVariant					headerData(			int section, Qt::Orientation orientation, int role = Qt::DisplayRole )			const	override;
	bool						setData(			const QModelIndex &index, const QVariant &value, int role)								override;
	Qt::ItemFlags				flags(				const QModelIndex &index)														const	override;
	QModelIndex					index(				int row, int column, const QModelIndex &parent = QModelIndex())					const	override;
	QModelIndex					parent(				const QModelIndex &index)														const	override;

	bool						isRowVirtual(		int row)																		const;
	bool						isColumnVirtual(	int col)																		const;
	bool						expandDataSet()																						const { return _expandDataSet; }
	void						setExpandDataSet(	bool expand)																			{ _expandDataSet = expand; }

	void						setSourceModel(		QAbstractItemModel* model);
	QAbstractItemModel*			sourceModel()																						const { return _sourceModel; }

	void						removeRows(			int start, int count);
	void						removeColumns(		int start, int count);
	void						removeRowGroups(	std::vector<std::pair<int,int>> groups);
	void						removeColumnGroups(	std::vector<std::pair<int,int>> groups);
	void						insertRows(			int row, int count = 1);
	void						insertColumns(		int col, int count = 1);
	void						insertColumn(		int col, bool computed, bool R);
	void						pasteSpreadsheet(	int row, int col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> & labels, const QStringList& colNames = {}, const std::vector<boolvec> & selected = {});
	int							setColumnType(		intset columnIndex, int columnType);
	void						columnReverseValues(intset columnIndexes);
	void						columnautoSortByValues(intset columnIndexes);
	void						copyColumns(		int startCol, const std::vector<Json::Value>& copiedColumns);
	Json::Value					serializedColumn(	int col);

	void						undo()				{ _undoStack->undo(); }
	void						redo()				{ _undoStack->redo(); }
	QString						undoText()			{ return _undoStack->undoText(); }
	QString						redoText()			{ return _undoStack->redoText(); }
	void						resize(int row, int col, bool onlyExpand = true, const QString& undoText = QString());
	bool						useUndoStack() const;

signals:
	void						undoChanged();

protected:
	QAbstractItemModel*			_sourceModel			= nullptr;
	bool						_expandDataSet			= false;

	UndoStack*					_undoStack				= nullptr;

	const int	EXTRA_COLS				= 7;
	const int	EXTRA_ROWS				= 20;
};

#endif // EXPANDDATAPROXYMODEL_H
