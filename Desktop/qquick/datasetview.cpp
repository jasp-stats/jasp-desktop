#include "datasetview.h"

#include <QSGFlatColorMaterial>
#include <QSGGeometry>
#include <QSGNode>
#include "log.h"
#include <QScreen>
#include "data/datasetpackage.h"
#include <iostream>
#include <QGuiApplication>
#include <QClipboard>
#include "utils.h"

DataSetView * DataSetView::_mainDataSetView = nullptr;


DataSetView::DataSetView(QQuickItem *parent)
	: DataSetViewBase (parent)
	, _expandedModel(new ExpandDataProxyModel(this))
{
	connect(DataSetPackage::pkg(),		&DataSetPackage::dataModeChanged,				this, &DataSetView::onDataModeChanged);
	connect(_expandedModel,				&ExpandDataProxyModel::undoChanged,				this, &DataSetView::undoChanged);
	connect(_selectionModel,			&QItemSelectionModel::currentColumnChanged,		this, &DataSetView::currentSelectedColumnHandler);
}

void DataSetView::setModel(QAbstractItemModel * model)
{
	_expandedModel->setSourceModel(model);

	DataSetViewBase::setModel(_expandedModel);
}


void DataSetView::setMainData(bool newMainData)
{
	if (_mainData == newMainData)
		return;
	
	assert(!_mainData || ( !_mainDataSetView || _mainDataSetView == this));
	
	_mainData = newMainData;
	
	if(_mainData)						_mainDataSetView = this;
	else if(_mainDataSetView == this)	_mainDataSetView = nullptr;
	
	emit mainDataChanged();
}

void DataSetView::setExpandDataSet(bool expand)
{
	if (!_expandedModel || expand == expandDataSet())
		return;

	_expandedModel->setExpandDataSet(expand);
	emit expandDataSetChanged();
}

void DataSetView::setRolenames()
{
	_roleNameToRole.clear();

	if (!_expandedModel->sourceModel())
		return;

	auto roleNames = _expandedModel->sourceModel()->roleNames();

	for(const auto & rn : roleNames.keys())
		_roleNameToRole[roleNames[rn].toStdString()] = rn;
}

void DataSetView::_copy(QPoint where, bool clear)
{
	if (!_selectionModel->hasSelection())
		return;

	_copiedColumns				. clear();
	_lastJaspCopyValues			. clear();
	_lastJaspCopyLabels			. clear();
	_lastJaspCopyIntoClipboard	= "";

	//QModelIndexList selected = _selectionModel->selectedIndexes();

	std::vector<QStringList>	rows;

	QPoint	minIdx		= selectionMin(),
		maxIdx		= selectionMax();
	bool	allSelected = minIdx.x() <= 0 && minIdx.y() <= 0 && maxIdx.x() + 1 >= _expandedModel->sourceModel()->columnCount() && maxIdx.y() + 1 >= _expandedModel->sourceModel()->rowCount();

	for(size_t r=minIdx.y(); r<=maxIdx.y(); r++)
	{
		rows.push_back({});

		for(size_t c=minIdx.x(); c<=maxIdx.x(); c++)
			if(_selectionModel->columnIntersectsSelection(c))
			{
				if(!isSelected(r, c))
					rows[ rows.size()-1 ].append("");
				else
				{
					QVariant valVar = _expandedModel->data(_expandedModel->index(r, c));
					std::string val = fq(valVar.toString());

					rows[ rows.size()-1 ].append(
						valVar.type() == QVariant::Double				?
							QLocale::system().toString(valVar.toDouble())	: //To make sure commas etc are formatted as the system expects.
							tq(val).replace('\n', ' ').replace('\t', ' ') )	; //Because Excel/etc use tab/newline for cell/row we make sure they are not in the cells at least.
				}
			}
	}

	if(rows.size() == 0)
		return; //Nothing to copy

	int rowsSelected = rows.size();

	if(isColumnHeader(where))
	{
		QStringList headerRow;

		for(int c=minIdx.x(); c<=maxIdx.x(); c++)
			if(_selectionModel->columnIntersectsSelection(c))
			{
				_copiedColumns.push_back(_expandedModel->serializedColumn(c));
				headerRow.push_back(_expandedModel->headerData(c, Qt::Horizontal).toString());
			}

		rows.insert(rows.begin(), headerRow);
	}

	_lastJaspCopyValues.clear();
	_lastJaspCopyLabels.clear();
	_lastJaspCopySelect.clear();

	std::vector<boolvec> localSelected; //Doesnt ignore columns in which nothing was selected!

	//Collect values and labels shown for internal lossless copying, will obviously not work for copying to external jasp.
	for(int c=minIdx.x(); c<=maxIdx.x(); c++)
		if(_selectionModel->columnIntersectsSelection(c))
		{
			qstringvec	vals,
				labs;

			for(size_t r=minIdx.y(); r<=maxIdx.y(); r++)
			{
				QModelIndex expandedIndex = _expandedModel->index(r, c);
				vals.push_back(_expandedModel->data(expandedIndex, int(DataSetPackage::specialRoles::value)).toString());
				labs.push_back(_expandedModel->data(expandedIndex, int(DataSetPackage::specialRoles::label)).toString());
			}

			_lastJaspCopyValues	.push_back(vals);
			_lastJaspCopyLabels	.push_back(labs);
		}

	for(int c=minIdx.x(); c<=maxIdx.x(); c++)
	{
		boolvec		sels;

		for(size_t r=minIdx.y(); r<=maxIdx.y(); r++)
			sels.push_back(isSelected(r, c));

		if(_selectionModel->columnIntersectsSelection(c))
			_lastJaspCopySelect.push_back(sels);

		localSelected.push_back(sels);
	}


	QStringList	all;
	for(const auto & row : rows)
		all.append(row.join("\t"));
	QString copyThis = all.join("\n");

	QGuiApplication::clipboard()->setText(copyThis);
	_lastJaspCopyIntoClipboard = copyThis;

	if(clear)
	{
		if(allSelected)
		{
			UndoStack::singleton()->startMacro(tr("Cutting all data"));
			_expandedModel->removeColumns(	minIdx.x(), 1+(maxIdx.x() - minIdx.x()));
			_expandedModel->removeRows(		minIdx.y(), rowsSelected);
			_expandedModel->insertColumns(0);
			_expandedModel->insertRows(0);
			UndoStack::singleton()->endMacro();//new PasteSpreadsheetCommand(_expandedModel->sourceModel(), 0, 0, {{""}}, {{""}}, {}, {}));
		}
		else if(isColumnHeader(where))
			_expandedModel->removeColumns(minIdx.x(), 1 + (maxIdx.x() - minIdx.x()));
		else if(isRowHeader(where))
			_expandedModel->removeRows(minIdx.y(), rowsSelected);
		else
		{
			Log::log() << "DataSetViewBase about to clear at row: " << minIdx.y() << " and col: " << minIdx.x() << std::endl;
			auto emptyCells = 	std::vector<std::vector<QString>>(
				1+(maxIdx.x() - minIdx.x()),
				std::vector<QString>(
					rowsSelected,
					""
					)
				);

			_expandedModel->pasteSpreadsheet(minIdx.y(), minIdx.x(), emptyCells, emptyCells, {}, localSelected);
		}
	}
}

void DataSetView::paste(QPoint where)
{
	if (where == QPoint(-1, -1))
		where = selectionMin();

	QString clipboardStr = QGuiApplication::clipboard()->text();

	if (_lastJaspCopyIntoClipboard != clipboardStr)
	{
		// The text in the clipboard was not copied from the currently running JASP. So clear the reference to the "copied columns", so that they are not used for the paste action.
		_copiedColumns		.clear();
		_lastJaspCopyValues	.clear();
		_lastJaspCopyLabels	.clear();
		_lastJaspCopySelect	.clear();
	}

	if (isColumnHeader(where) && _copiedColumns.size() && where.x() >= 0) //internal column copy:
		_expandedModel->copyColumns(where.x(), _copiedColumns);
	else if(_lastJaspCopyValues.size()) //internal data copy:
	{
		if(!isColumnHeader(where))
			_expandedModel->pasteSpreadsheet(where.y(), where.x(), _lastJaspCopyValues, _lastJaspCopyLabels, {}, _lastJaspCopySelect);
		else
		{
			QStringList	colNames;
			auto		vals = _lastJaspCopyValues,
				labs = _lastJaspCopyLabels;
			auto		sels = _lastJaspCopySelect;

			for(size_t c=0; c<vals.size(); c++)
			{
				colNames.push_back(vals[c][0]);

				vals[c].erase(vals[c].begin());
				labs[c].erase(labs[c].begin());
				sels[c].erase(sels[c].begin());
			}

			_expandedModel->pasteSpreadsheet(0, where.x(), vals, labs, colNames, sels);
		}

	}
	else //its external data:
	{
		std::vector<qstringvec> newData;
		QStringList				colNames,
			rows = clipboardStr.contains("\r\n") ? clipboardStr.split("\r\n") : clipboardStr.split("\n");

		if (rows.isEmpty()) return;

		if (isColumnHeader(where))
		{
			colNames = rows[0].split("\t");
			rows.removeFirst();
		}

		size_t row = 0, col = 0;
		for(const QString & rowStr : rows)
		{
			col = 0;
			if (rowStr.isEmpty() && row == rows.length() - 1) continue; // Some editors might throw an empty line onto the end of the clipboard. Should at least have one value on the row
			for(const QString & cellStr : rowStr.split("\t"))
			{
				if(newData.size()		<= col) newData.	 resize(col+1);
				if(newData[col].size()	<= row)	newData[col].resize(row+1);

				newData[col][row] = cellStr;
				col++;
			}
			row++;
		}
		for(auto& column : newData)
			column.resize(row); // Make sure that all columns have the same number of rows

		_expandedModel->pasteSpreadsheet(isColumnHeader(where) ? 0 : where.y(), where.x(), newData, {}, colNames);
	}
}

bool DataSetView::isVirtual(const QPoint& point)
{
	return isColumnVirtual(point.x()) || isRowVirtual(point.y());
}

bool DataSetView::isRowVirtual(int row)
{
	return row >= _expandedModel->sourceModel()->rowCount();
}

bool DataSetView::isColumnVirtual(int column)
{
	return column >= _expandedModel->sourceModel()->columnCount();
}

bool DataSetView::isFiltered(int row, int col)
{
	if (isRowVirtual(row) || isColumnVirtual(col))
		return true;

	QModelIndex ind(_expandedModel->sourceModel()->index(row, col));
	return _expandedModel->sourceModel()->data(ind, getRole("filter")).toBool();
}

void DataSetView::columnReverseValues(int columnIndex)
{
	columnIndexSelectedApply(columnIndex, [&](intset col) { _expandedModel->columnReverseValues(col);  });
}

void DataSetView::columnautoSortByValues(int columnIndex)
{
	columnIndexSelectedApply(columnIndex, [&](intset col) { _expandedModel->columnautoSortByValues(col);  });
}

QString DataSetView::columnInsertBefore(int col, bool computed, bool R)
{
	destroyEditItem(false);

	if(col == -1)
		col = selectionMin().x() != -1 ? selectionMin().x() : 0;

	_expandedModel->insertColumn(col, computed, R);
	return _expandedModel->headerData(col, Qt::Horizontal).toString();
}

QString DataSetView::columnInsertAfter(int col, bool computed, bool R)
{
	if(col == -1)
		col = selectionMax().x() != -1 ? selectionMax().x() : _expandedModel->sourceModel()->columnCount() - 1;

	return columnInsertBefore(col + 1, computed, R);
}
void DataSetView::columnComputedInsertAfter(int col, bool R)
{
	columnInsertAfter(col, true, R);
}

void DataSetView::columnComputedInsertBefore(int col, bool R)
{
	columnInsertBefore(col, true, R);
}

void DataSetView::columnsDeleteSelected()
{
	columnsDelete(-1);
}

void DataSetView::columnsDelete(int col)
{
	if(_expandedModel->sourceModel()->columnCount() <= 1 || (selectionMin().x() == -1 && col == -1))
		return;

	int columnA	= selectionMin().x(),
		columnB	= selectionMax().x();

	destroyEditItem(false);

	if(columnA > columnB)
		std::swap(columnA, columnB);

	if(col == -1)
		col = columnA;

	if(col == -1)
		col = _expandedModel->sourceModel()->columnCount() - 1;

	if(columnA == -1 || columnA > col || columnB < col || (columnA == col && columnB == col))
		_expandedModel->removeColumns(col, 1);
	else
	{
		//Go through all columns and make separate removals for each contiguously selected set of columns
		std::vector<std::pair<int, int>> removals;
		int lastStart = columnA,
			c;
		for(c=columnA; c<=columnB; c++)
			if(!_selectionModel->columnIntersectsSelection(c))
			{
				if(lastStart > -1)
					removals.push_back(std::make_pair(lastStart, (c - lastStart))); //Dont do +1 because c is not selected, so not contiguous with the selected columns from lastStart!

				lastStart = -1;
			}
			else if(lastStart == -1)
				lastStart = c;


		removals.push_back(std::make_pair(lastStart, (c - lastStart)));
		std::reverse(removals.begin(), removals.end());
		_expandedModel->removeColumnGroups(removals);
	}

	_selectionModel->clear();
}


void DataSetView::rowInsertBefore(int row)
{
	destroyEditItem(false);

	if(row == -1)
		row = selectionMin().y() != -1 ? selectionMin().y() : 0;

	_expandedModel->insertRows(row);
}

void DataSetView::rowInsertAfter(int row)
{
	if(row == -1)
		row = selectionMax().y() != -1 ? selectionMax().y() : _expandedModel->sourceModel()->rowCount() - 1;

	rowInsertBefore(row + 1);
}

void DataSetView::rowsDeleteSelected()
{
	rowsDelete(-1);
}

void DataSetView::rowsDelete(int row)
{
	if(_expandedModel->sourceModel()->rowCount() <= 1 || (selectionMin().y() == -1 && row == -1))
		return;

	int rowA	= selectionMin().y(),
		rowB	= selectionMax().y();

	destroyEditItem();

	if(rowA > rowB)
		std::swap(rowA, rowB);

	if(row == -1)
		row = rowA;

	if(row == -1)
		row = _expandedModel->sourceModel()->rowCount() - 1;

	if(rowA == -1 || rowA > row || rowB < row || (rowA == row && rowB == row))
		_expandedModel->removeRows(row, 1);
	else
	{
		//Go through all rows and make separate removals for each contiguously selected set of rows
		std::vector<std::pair<int, int>> removals;
		int lastStart	= rowA,
			r			= rowA + 1;
		for(; r<=rowB; r++)
			if(!_selectionModel->rowIntersectsSelection(r))
			{
				if(lastStart != -1)
					removals.push_back(std::make_pair(lastStart, (r - lastStart))); //Dont do +1 because r is not contiguous with lastStart!
				lastStart = -1;
			}
			else if(lastStart == -1)
				lastStart = r;


		removals.push_back(std::make_pair(lastStart, (r - lastStart)));
		std::reverse(removals.begin(), removals.end());
		_expandedModel->removeRowGroups(removals);
	}

	_selectionModel->clear();
}

void DataSetView::cellsClear()
{
	QPoint	selMin(selectionMin()),
		selMax(selectionMax());

	if(selMin.x() == -1 || selMin.y() == -1)
		return;

	int cols = 1 + std::abs(selMax.x() - selMin.x()),
		rows = 1 + std::abs(selMax.y() - selMin.y()),
		col0 = selMin.x(),
		row0 = selMin.y();

	std::vector<std::vector<QString>> cells(cols, std::vector<QString>(rows, QString("")));

	_expandedModel->pasteSpreadsheet(row0, col0, cells, cells);
}

void DataSetView::currentSelectedColumnHandler(const QModelIndex &current, const QModelIndex &previous)
{
	int selectedColumn = current.column();
	if (selectedColumn >= 0)
		emit DataSetPackage::pkg()->chooseColumn(selectedColumn);
}

void DataSetView::setColumnType(int columnIndex, int newColumnType)
{
	columnIndexSelectedApply(columnIndex, [&](intset col) { _expandedModel->setColumnType(col, newColumnType);  });
}

void DataSetView::resizeData(int rows, int columns)
{
	// Argument row and column of the resize method are indices
	_expandedModel->resize(rows - 1, columns - 1, false, tr("Resize data to %1 rows and %2 columns").arg(rows).arg(columns));
}


