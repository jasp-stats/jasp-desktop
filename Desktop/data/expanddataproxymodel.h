#ifndef EXPANDDATAPROXYMODEL_H
#define EXPANDDATAPROXYMODEL_H

#include <QIdentityProxyModel>
#include "utils.h"
#include "undostack.h"

class ExpandDataProxyModel : public QIdentityProxyModel
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
	
	DataSet					*	dataSetSourceModel() const;

	bool						isRowVirtual(		int row)																		const;
	bool						isColumnVirtual(	int col)																		const;
	bool						expandDataSet()																						const { return _expandDataSet; }
	void						setExpandDataSet(	bool expand)																			{ _expandDataSet = expand; }

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

	UndoStack				*	undoStack()			{ return UndoStack::singleton(); }
	void						undo()				{ if (undoStack()) undoStack()->undo(); }
	void						redo()				{ if (undoStack()) undoStack()->redo(); }
	QString						undoText()			{ return undoStack() ? undoStack()->undoText() : ""; }
	QString						redoText()			{ return undoStack() ? undoStack()->redoText() : ""; }
	void						resize(int row, int col, bool onlyExpand = true, const QString& undoText = QString());
	bool						useUndoStack() const;

	stringset columnIndexesToNames(intset columnIndexes);
signals:
	void						undoChanged();

public slots:
	void						onCurrentUndoStackChanged();

protected:
	bool						_expandDataSet			= false;

	const int	EXTRA_COLS				= 7;
	const int	EXTRA_ROWS				= 20;

private:
	void						connectUndoStack();

	// Convert a shown (filtered/compacted) index into the raw DataSet index for that dimension.
	// Indexes past the shown region map to the end of the raw table (for appends).
	int							shownToRaw(int shownIndex, bool isRow) const;
	// Turn a contiguous run of shown indexes into a list of contiguous raw-index runs (collapsing gaps).
	std::vector<std::pair<int,int>>	rawRunsFromShown(bool isRow, int shownStart, int shownCount) const;
	// Remove only the shown rows/columns (as a macro of contiguous commands, high-index first).
	void						removeRuns(bool isRows, const std::vector<std::pair<int,int>>& shownGroups);

	QMetaObject::Connection		_undoChangedCon;
};

#endif // EXPANDDATAPROXYMODEL_H
