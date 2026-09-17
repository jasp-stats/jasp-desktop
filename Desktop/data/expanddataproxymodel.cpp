#include "expanddataproxymodel.h"
#include "datasettablemodel.h"
#include "dataenums.h"
#include "qutils.h"
#include "workspace.h"
#include "datasetpackage.h"
#include <algorithm>
#include <climits>

ExpandDataProxyModel::ExpandDataProxyModel(QObject *parent)
	: QIdentityProxyModel{parent}
{
	connectUndoStack();

	//Deliberately not Workspace::singleton(): that object is destroyed and recreated whenever a workspace
	//is (re)loaded, and this connection would go with it, after which the current undo stack is never
	//picked up again and undoChanged stops being emitted. DataSetPackage relays the very same signal and
	//sticks around for the whole session.
	if (DataSetPackage::pkg())
		connect(DataSetPackage::pkg(), &DataSetPackage::shownDataSetChanged, this, &ExpandDataProxyModel::onCurrentUndoStackChanged);
}

void ExpandDataProxyModel::connectUndoStack()
{
	if (_undoChangedCon)
		disconnect(_undoChangedCon);

	if (auto* stack = UndoStack::singleton())
		_undoChangedCon = connect(stack, &QUndoStack::indexChanged, this, &ExpandDataProxyModel::undoChanged);
}

void ExpandDataProxyModel::onCurrentUndoStackChanged()
{
	connectUndoStack();
	emit undoChanged();
}

int ExpandDataProxyModel::rowCount(const QModelIndex &) const
{
	if (!sourceModel())
		return 0;
	return sourceModel()->rowCount() + (_expandDataSet ? EXTRA_ROWS : 0);
}

int ExpandDataProxyModel::columnCount(const QModelIndex &) const
{
	if (!sourceModel())
		return 0;
	return sourceModel()->columnCount() + (_expandDataSet ? EXTRA_COLS : 0);
}

QVariant ExpandDataProxyModel::data(const QModelIndex &indexP, int role) const
{
	if (!sourceModel() || role == -1) // Role not defined
		return QVariant();

	int row		= indexP.row(),
		column	= indexP.column();

	// Real cell: forward to the (filtered) source model so the column/row filters are respected.
	if (column < sourceModel()->columnCount() && row < sourceModel()->rowCount())
		return sourceModel()->data(sourceModel()->index(row, column), role);

	// Virtual cell (past the filtered region, only present in expand mode): synthesize.
	switch(role)
	{
	case int(dataPkgRoles::selected):				return false;
	case int(dataPkgRoles::lines):					return DataSet::getDataSetViewLines(column>0, row>0, true, true);
	case int(dataPkgRoles::value):					return "";
	case int(dataPkgRoles::columnType):				return int(columnType::scale);
	default:										return QVariant();
	}

	return QVariant(); //gcc might complain some more I guess?
}

QVariant ExpandDataProxyModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (!sourceModel() || role == -1) // Role not defined
		return QVariant();

	if (orientation == Qt::Orientation::Horizontal)
	{
		if (section < sourceModel()->columnCount())
			return sourceModel()->headerData(section, orientation, role);
		else
			switch(role)
			{
			case int(dataPkgRoles::columnIsComputed):				return false;
			case int(dataPkgRoles::computedColumnIsInvalidated):	return false;
			case int(dataPkgRoles::filter):							return false;
			case int(dataPkgRoles::computedColumnError):			return "";
			case int(dataPkgRoles::columnType):						return int(columnType::unknown);
			case int(dataPkgRoles::maxColString):					return "XXXXXXXXXXX";
			default:												return "";
			}
	}
	else if (orientation == Qt::Orientation::Vertical)
	{
		if (section < sourceModel()->rowCount())
			return sourceModel()->headerData(section, orientation, role);
		else if (section == 0 && role == int(dataPkgRoles::maxRowHeaderString))
			return "XXXX";
		else
			return  section + 1;
	}

	return QVariant();
}

Qt::ItemFlags ExpandDataProxyModel::flags(const QModelIndex &index) const
{
	if (!sourceModel())
		return Qt::NoItemFlags;

	if (index.column() < sourceModel()->columnCount() && index.row() < sourceModel()->rowCount())
		return sourceModel()->flags(sourceModel()->index(index.row(), index.column()));

	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

QModelIndex ExpandDataProxyModel::index(int row, int column, const QModelIndex &) const
{
	if (!sourceModel())
		return QModelIndex();

	return createIndex(row, column);
}

QModelIndex ExpandDataProxyModel::parent(const QModelIndex &index) const
{
	return QModelIndex();
}


bool ExpandDataProxyModel::isRowVirtual(int row) const
{
	if (!sourceModel())
		return false;

	return row >= sourceModel()->rowCount();
}

bool ExpandDataProxyModel::isColumnVirtual(int col) const
{
	if (!sourceModel())
		return false;

	return col >= sourceModel()->columnCount();
}

int ExpandDataProxyModel::shownToRaw(int shownIndex, bool isRow) const
{
	QAbstractItemModel * src = sourceModel();
	if (!src)
		return shownIndex;

	DataSetTableModel * table = qobject_cast<DataSetTableModel *>(src);
	if (!table)
		return shownIndex;

	const int shownCount = isRow ? src->rowCount() : src->columnCount();

	if (shownIndex <= 0)
		shownIndex = 0;

	// Past the shown region (virtual/expand area): map to the raw slot the virtual cell will occupy
	// once the table is grown to include it (shown index "shownCount + k" sits at raw tail + k).
	if (shownIndex >= shownCount)
		return (isRow ? dataSetSourceModel()->rowCount() : dataSetSourceModel()->columnCount())
			+ (shownIndex - shownCount);

	QModelIndex shownIdx	= isRow ? table->index(shownIndex, 0) : table->index(0, shownIndex);
	QModelIndex raw			= table->mapToSource(shownIdx);

	if (raw.isValid())
		return isRow ? raw.row() : raw.column();

	return isRow ? dataSetSourceModel()->rowCount() : dataSetSourceModel()->columnCount();
}

std::vector<std::pair<int,int>> ExpandDataProxyModel::rawRunsFromShown(bool isRow, int shownStart, int shownCount) const
{
	std::vector<std::pair<int,int>> runs;

	if (!sourceModel())
		return runs;

	const int maxShown = isRow ? sourceModel()->rowCount() : sourceModel()->columnCount();

	int runStart = -1,
		lastRaw  = -1;

	for (int s = shownStart; s < shownStart + shownCount && s < maxShown; s++)
	{
		int r = shownToRaw(s, isRow);
		if (r < 0)
			continue;

		if (runStart < 0)
		{
			runStart = r;
			lastRaw  = r;
		}
		else if (r == lastRaw + 1)
			lastRaw = r;
		else
		{
			runs.push_back({runStart, lastRaw - runStart + 1});
			runStart = r;
			lastRaw  = r;
		}
	}

	if (runStart >= 0)
		runs.push_back({runStart, lastRaw - runStart + 1});

	return runs;
}

void ExpandDataProxyModel::removeRuns(bool isRows, const std::vector<std::pair<int,int>>& shownGroups)
{
	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	std::vector<std::pair<int,int>> rawRuns;
	for (const auto & startCount : shownGroups)
	{
		auto runs = rawRunsFromShown(isRows, startCount.first, startCount.second);
		rawRuns.insert(rawRuns.end(), runs.begin(), runs.end());
	}

	if (rawRuns.empty())
		return;

	// Sort ascending and merge adjacent raw runs (was only ever true if two shown groups touched).
	std::sort(rawRuns.begin(), rawRuns.end(), [](const auto & a, const auto & b){ return a.first < b.first; });

	std::vector<std::pair<int,int>> merged;
	for (const auto & run : rawRuns)
	{
		if (!merged.empty() && merged.back().first + merged.back().second == run.first)
			merged.back().second += run.second;
		else
			merged.push_back(run);
	}

	int total = 0;
	for (const auto & run : merged)
		total += run.second;

	UndoStack * stack = undoStack();
	stack->startMacro(isRows ? tr("Remove %1 rows").arg(total) : tr("Remove %1 columns").arg(total));

	// Push descending so redo removes high-index first (lower indices stay valid) and undo re-inserts ascending.
	for (auto it = merged.rbegin(); it != merged.rend(); ++it)
	{
		if (isRows)
			stack->pushCommand(new RemoveRowsCommand(ds, it->first, it->second));
		else
			stack->pushCommand(new RemoveColumnsCommand(ds, it->first, it->second));
	}

	stack->endMacro();
}

void ExpandDataProxyModel::removeRows(int start, int count)
{
	if (!sourceModel() || count <= 0 || start < 0 || start >= sourceModel()->rowCount())
		return;

	if (start + count > sourceModel()->rowCount())
		count = sourceModel()->rowCount() - start;

	removeRuns(true, {{start, count}});
}

void ExpandDataProxyModel::removeRowGroups(std::vector<std::pair<int, int> > groups)
{
	removeRuns(true, groups);
}

void ExpandDataProxyModel::removeColumns(int start, int count)
{
	if (!sourceModel() || count <= 0 || start < 0 || start >= sourceModel()->columnCount())
		return;

	if (start + count > sourceModel()->columnCount())
		count = sourceModel()->columnCount() - start;

	removeRuns(false, {{start, count}});
}

void ExpandDataProxyModel::removeColumnGroups(std::vector<std::pair<int, int> > groups)
{
	removeRuns(false, groups);
}

void ExpandDataProxyModel::insertRows(int row, int count)
{
	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	undoStack()->pushCommand(new InsertRowsCommand(ds, shownToRaw(row, true), count));
}


void ExpandDataProxyModel::insertColumns(int col, int count)
{
	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	undoStack()->pushCommand(new InsertColumnsCommand(ds, shownToRaw(col, false), count));
}


void ExpandDataProxyModel::insertColumn(int col, bool computed, bool R)
{
	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	QMap<QString, QVariant> props;
	if (computed)
		props["computed"] = int(R ? computedColumnType::rCode : computedColumnType::constructorCode);
	undoStack()->pushCommand(new InsertColumnCommand(ds, shownToRaw(col, false), props));
}

void ExpandDataProxyModel::resize(int row, int col, bool onlyExpand, const QString& undoText)
{
	if (!sourceModel() || row < 0 || col < 0)
		return;

	UndoStack * stack = undoStack();

	if (onlyExpand)
	{
		// Grow the table so the shown cell (row, col) exists. The distance to grow is computed in
		// shown (filtered/compacted) space; the new rows/columns are appended to the raw table.
		int colsToAdd = std::max(0, 1 + col - sourceModel()->columnCount()),
			rowsToAdd = std::max(0, 1 + row - sourceModel()->rowCount());

		if (colsToAdd == 0 && rowsToAdd == 0)
			return;

		stack->startMacro(undoText);
		if (colsToAdd > 0)
			stack->pushCommand(new InsertColumnsCommand(dataSetSourceModel(), dataSetSourceModel()->columnCount(), colsToAdd));
		if (rowsToAdd > 0)
			stack->pushCommand(new InsertRowsCommand(dataSetSourceModel(), dataSetSourceModel()->rowCount(), rowsToAdd));
		if (!undoText.isEmpty())
			stack->endMacro();
		return;
	}

	// Shrink path (only used by the whole-dataset "Resize data to NxM" dialog): operate on the raw table.
	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	const int rawRows	= ds->rowCount(),
			  rawCols	= ds->columnCount();

	int targetRow = std::max(0, row),
		targetCol = std::max(0, col);

	if (targetCol == rawCols - 1 && targetRow == rawRows - 1)
		return;

	stack->startMacro(undoText);

	if(targetCol >= rawCols)
	{
		int colC = 1 + targetCol - rawCols;
		if(colC > 0)
			stack->pushCommand(new InsertColumnsCommand(ds, rawCols, colC));
	}
	else if (targetCol < (rawCols - 1))
		stack->pushCommand(new RemoveColumnsCommand(ds, targetCol + 1, rawCols - targetCol - 1));

	if(targetRow >= rawRows)
	{
		int rowC = 1 + targetRow - rawRows;
		if(rowC > 0)
			stack->pushCommand(new InsertRowsCommand(ds, rawRows, rowC));
	}
	else if (targetRow < (rawRows - 1))
		stack->pushCommand(new RemoveRowsCommand(ds, targetRow + 1, rawRows - targetRow - 1));

	if (!undoText.isEmpty())
		stack->endMacro();
}

bool ExpandDataProxyModel::useUndoStack() const
{
	return sourceModel() != nullptr;
}

bool ExpandDataProxyModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (!sourceModel() || index.row() < 0 || index.column() < 0)
		return false;
	
	if(!useUndoStack())
	{
		return sourceModel()->setData(sourceModel()->index(index.row(), index.column()), value, role);	
	}

	resize(index.row(), index.column());

	int rawRow = shownToRaw(index.row(), true),
		rawCol = shownToRaw(index.column(), false);

	undoStack()->endMacro(new SetDataCommand(dataSetSourceModel(), rawRow, rawCol, value, role));
	return true;
}

void ExpandDataProxyModel::pasteSpreadsheet(int row, int col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> & labels, const QStringList & colNames, const std::vector<boolvec> & selected)
{
	if (!sourceModel() || row < 0 || col < 0 || values.size() == 0 || values[0].size() == 0 )
		return;

	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	const int shownRowCount	= sourceModel()->rowCount(),
			  shownColCount	= sourceModel()->columnCount();

	const int shownCols		= values.size(),
			  shownRows		= values[0].size();

	// Map every shown cell of the paste rectangle onto its raw DataSet position (skipping hidden cells).
	int rawMinR = INT_MAX, rawMaxR = INT_MIN,
		rawMinC = INT_MAX, rawMaxC = INT_MIN;

	for (int sc = 0; sc < shownCols; sc++)
	{
		int asc = col + sc;
		int rawCol = asc < shownColCount ? shownToRaw(asc, false) : ds->columnCount() + (asc - shownColCount);
		rawMinC = std::min(rawMinC, rawCol);
		rawMaxC = std::max(rawMaxC, rawCol);
	}

	for (int sr = 0; sr < shownRows; sr++)
	{
		int asr = row + sr;
		int rawRow = asr < shownRowCount ? shownToRaw(asr, true) : ds->rowCount() + (asr - shownRowCount);
		rawMinR = std::min(rawMinR, rawRow);
		rawMaxR = std::max(rawMaxR, rawRow);
	}

	const int rawCols = rawMaxC - rawMinC + 1,
			  rawRows = rawMaxR - rawMinR + 1;

	// Dense raw buffer; hidden/skipped cells stay empty and not "selected".
	std::vector<std::vector<QString>>	newValues(rawCols, std::vector<QString>(rawRows, "")),
										newLabels(rawCols, std::vector<QString>(rawRows, ""));
	std::vector<boolvec>				newSelected(rawCols, boolvec(rawRows, false));
	QStringList							newColNames;

	newColNames.reserve(rawCols);
	for (int c = 0; c < rawCols; c++)
		newColNames.append("");

	for (int sc = 0; sc < shownCols; sc++)
	{
		int asc = col + sc;
		int rawCol = asc < shownColCount ? shownToRaw(asc, false) : ds->columnCount() + (asc - shownColCount);

		if (sc < colNames.size())
			newColNames[rawCol - rawMinC] = colNames[sc];

		const bool colSelected = selected.size() == 0 || selected[sc].size() == 0;

		for (int sr = 0; sr < shownRows; sr++)
		{
			int asr = row + sr;
			int rawRow = asr < shownRowCount ? shownToRaw(asr, true) : ds->rowCount() + (asr - shownRowCount);

			int C = rawCol - rawMinC,
				R = rawRow - rawMinR;

			newValues[C][R]		= values[sc][sr];
			if (labels.size()  > sc && labels[sc].size() > sr)
				newLabels[C][R]	= labels[sc][sr];
			newSelected[C][R]	= colSelected || selected[sc][sr];
		}
	}

	undoStack()->endMacro(new PasteSpreadsheetCommand(ds, rawMinR, rawMinC, newValues, newLabels, newSelected, newColNames));
}


stringset ExpandDataProxyModel::columnIndexesToNames(intset columnIndexes)
{
	stringset colNames;

	for(int i : columnIndexes)
		colNames.insert(fq(headerData(i, Qt::Horizontal, int(dataPkgRoles::name)).toString()));

	return colNames;
}

int ExpandDataProxyModel::setColumnType(intset columnIndexes, int columnType)
{

	undoStack()->pushCommand(new SetColumnTypeCommand(dataSetSourceModel(), columnIndexesToNames(columnIndexes), columnType));

	return columnType; //it always works
}

void ExpandDataProxyModel::columnReverseValues(intset columnIndexes)
{
	undoStack()->pushCommand(new ColumnReverseValuesCommand(dataSetSourceModel(), columnIndexesToNames(columnIndexes)));
}

void ExpandDataProxyModel::columnautoSortByValues(intset columnIndexes)
{
	undoStack()->pushCommand(new ColumnToggleAutoSortByValuesCommand(dataSetSourceModel(), columnIndexesToNames(columnIndexes)));
}

void ExpandDataProxyModel::copyColumns(int startCol, const std::vector<Json::Value>& copiedColumns)
{
	if (!sourceModel() || startCol < 0 || copiedColumns.size() == 0)
		return;

	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return;

	int rawStart = shownToRaw(startCol, false);
	resize(0, startCol + copiedColumns.size() - 1);
	undoStack()->endMacro(new CopyColumnsCommand(ds, rawStart, copiedColumns));
}

Json::Value ExpandDataProxyModel::serializedColumn(int col)
{
	DataSet * ds = dataSetSourceModel();
	if (!ds)
		return Json::nullValue;

	int rawCol = shownToRaw(col, false);
	if (rawCol >= 0 && rawCol < ds->columnCount())
		return ds->column(rawCol)->serialize();

	return Json::nullValue;
}

DataSet	* ExpandDataProxyModel::dataSetSourceModel() const 
{ 
	DataSetTableModel * table = qobject_cast<DataSetTableModel*>(sourceModel()); 
	
	if(table)
		return table->dataSetSourceModel();
	
	return nullptr;
}
