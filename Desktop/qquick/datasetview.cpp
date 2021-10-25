#include "datasetview.h"

#include <QSGFlatColorMaterial>
#include <QSGGeometry>
#include <QSGNode>
#include <queue>
#include "timers.h"
#include "log.h"
#include "gui/preferencesmodel.h"
#include "qquick/jasptheme.h"
#include <QScreen>
#include "data/datasetpackage.h"
#include <iostream>

DataSetView * DataSetView::_lastInstancedDataSetView = nullptr;

DataSetView::DataSetView(QQuickItem *parent) : QQuickItem (parent)
{
	setFlag(QQuickItem::ItemHasContents, true);
	setFlag(ItemIsFocusScope);

	material.setColor(Qt::gray);

	connect(this, &DataSetView::parentChanged,					this, &DataSetView::myParentChanged);

	connect(this, &DataSetView::viewportXChanged,				this, &DataSetView::viewportChanged);
	connect(this, &DataSetView::viewportYChanged,				this, &DataSetView::viewportChanged);
	connect(this, &DataSetView::viewportWChanged,				this, &DataSetView::viewportChanged);
	connect(this, &DataSetView::viewportHChanged,				this, &DataSetView::viewportChanged);

	connect(this, &DataSetView::itemDelegateChanged,			this, &DataSetView::reloadTextItems);
	connect(this, &DataSetView::rowNumberDelegateChanged,		this, &DataSetView::reloadRowNumbers);
	connect(this, &DataSetView::columnHeaderDelegateChanged,	this, &DataSetView::reloadColumnHeaders);

	connect(this, &DataSetView::itemHorizontalPaddingChanged,	this, &DataSetView::calculateCellSizes);
	connect(this, &DataSetView::itemVerticalPaddingChanged,		this, &DataSetView::calculateCellSizes);
	connect(this, &DataSetView::extraColumnItemChanged,			this, &DataSetView::calculateCellSizes);

	connect(this, &DataSetView::itemSizeChanged,				this, &DataSetView::reloadTextItems);
	connect(this, &DataSetView::itemSizeChanged,				this, &DataSetView::reloadRowNumbers);
	connect(this, &DataSetView::itemSizeChanged,				this, &DataSetView::reloadColumnHeaders);

	connect(PreferencesModel::prefs(), &PreferencesModel::uiScaleChanged,		this, &DataSetView::resetItems, Qt::QueuedConnection);
	connect(PreferencesModel::prefs(), &PreferencesModel::interfaceFontChanged,	this, &DataSetView::resetItems, Qt::QueuedConnection);

	setZ(10);

	_lastInstancedDataSetView = this;
}

void DataSetView::setModel(QAbstractItemModel * model)
{
	if(_model != model)
	{
		_model = model;

		connect(_model, &QAbstractItemModel::dataChanged,			this, &DataSetView::modelDataChanged		);
		connect(_model, &QAbstractItemModel::headerDataChanged,		this, &DataSetView::modelHeaderDataChanged	);
		connect(_model, &QAbstractItemModel::modelAboutToBeReset,	this, &DataSetView::modelAboutToBeReset		);
		connect(_model, &QAbstractItemModel::modelReset,			this, &DataSetView::modelWasReset			);

		setRolenames();

		setRowNumberWidth(getRowHeaderSize().width());

		//recalculateCellSizes = true;
		calculateCellSizes();
		update();

		emit modelChanged();
	}
}

void DataSetView::setRolenames()
{
	_roleNameToRole.clear();

	if(_model == nullptr) return;

	auto roleNames = _model->roleNames();

	for(auto rn : roleNames.keys())
		_roleNameToRole[roleNames[rn].toStdString()] = rn;

}


QSizeF DataSetView::getColumnSize(int col)
{
	QVariant maxColStringVar = _model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["maxColString"]);
	if(!maxColStringVar.isNull())
		return getTextSize(maxColStringVar.toString());
	else
	{
		QVariant columnWidthFallbackVar = _model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["columnWidthFallback"]);

		QSizeF columnSize = getTextSize("??????");

		if(!columnWidthFallbackVar.isNull())
			columnSize.setWidth(columnWidthFallbackVar.toFloat() - itemHorizontalPadding() * 2);

		return columnSize;
	}
}

QSizeF DataSetView::getRowHeaderSize()
{
	QString text = _model->headerData(0, Qt::Orientation::Vertical, _roleNameToRole["maxRowHeaderString"]).toString();

	return getTextSize(text);
}

void DataSetView::modelDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles)
{
	int col = topLeft.column();
	QSizeF calcSize = getColumnSize(col);

	if (_cacheItems || int(_cellSizes[size_t(col)].width() * 10) != int(calcSize.width() * 10)) //If we cache items we are not expecting the user to make regular manual changes to the data, so if something changes we can do a reset. Otherwise we are in TableView and we do it only when the column size changes.
		calculateCellSizes();
	else if (roles.contains(int(DataSetPackage::specialRoles::selected)) || roles.contains(Qt::DisplayRole))
	{
		// This is a special case for the VariablesWindows & TableView: caching mixed up the items, so it can't be used
		// but the selected context property must be updated for VariablesWindows
		// and the itemText must be updated for Grid TableView (used in Plot Editor).
		for (int col = topLeft.column(); col <= bottomRight.column(); col++)
			for (int row = topLeft.row(); row <= bottomRight.row(); row++)
			{
				ItemContextualized* itemCon = _cellTextItems[col][row];

				if (itemCon)
				{
					QQmlContext* context = itemCon->context;
					if (roles.contains(int(DataSetPackage::specialRoles::selected)))
						context->setContextProperty("itemSelected",	_model->data(_model->index(row, col), _roleNameToRole["selected"]));
					if (roles.contains(Qt::DisplayRole))
						context->setContextProperty("itemText", _model->data(_model->index(row, col)));
				}
			}
	}

	
	//The following else would be good but it doesnt seem to work on mac for some reason. It does work on linux though
	/*else 
		for(size_t row=topLeft.row(); row<=bottomRight.row(); row++)
			for(size_t col=topLeft.column(); col<=bottomRight.column(); col++)
			{
				storeTextItem(row, col);
				createTextItem(row, col);
			}*/

}

void DataSetView::modelHeaderDataChanged(Qt::Orientation, int, int)
{
	calculateCellSizes();
}

void DataSetView::modelAboutToBeReset()
{
	_storedLineFlags.clear();
	_storedDisplayText.clear();
}

void DataSetView::modelWasReset()
{
	setRolenames();
	calculateCellSizes();
}

void DataSetView::resetItems()
{
	calculateCellSizesAndClear(true); //We clear storage because otherwise the wrong font/scaling gets remembered by the item
}

void DataSetView::calculateCellSizesAndClear(bool clearStorage)
{
	JASPTIMER_RESUME(calculateCellSizes);

	_cellSizes.clear();
	_dataColsMaxWidth.clear();
	_storedLineFlags.clear();
	_storedDisplayText.clear();

	for(auto & col : _cellTextItems)
	{
		for(auto row : col.second)
			storeTextItem(row.first, col.first, false);
		col.second.clear();
	}

	std::list<int> cols, rows;

	for(auto col : _columnHeaderItems)
		cols.push_back(col.first);

	for(auto col : cols)
		storeColumnHeader(col);

	for(auto row : _rowNumberItems)
		rows.push_back(row.first);

	for(auto row : rows)
		storeRowNumber(row);

	if(clearStorage)
	{
		_rowNumberStorage		= {};
		_columnHeaderStorage	= {};
		_textItemStorage		= {};
	}

	if(_model == nullptr) return;

	_cellSizes.resize(_model->columnCount());
	_colXPositions.resize(_model->columnCount());
	_cellTextItems.clear();

	for(int col=0; col<_model->columnCount(); col++)
		_cellSizes[col] = getColumnSize(col);

	_dataColsMaxWidth.resize(_model->columnCount());

	for(int col=0; col<_model->columnCount(); col++)
		_dataColsMaxWidth[col] = _cellSizes[col].width() + _itemHorizontalPadding * 2;

	setHeaderHeight(_model->columnCount() == 0 ? 0 : _cellSizes[0].height() + _itemVerticalPadding * 2);
	setRowNumberWidth(getRowHeaderSize().width());

	float w = _rowNumberMaxWidth;
	for(int col=0; col<_model->columnCount(); col++)
		w += _dataColsMaxWidth[col];
  /// @todo, AMIR: the line above randomly throws! So, for some reason columnCount() returns 1 at line 221, and
  /// then 0 here! I am not yet 100% sure how the model is initialized but I see that most of the initialization
  /// are happening outside the constructor. Generally, we don't want uninitialized variables around. One way to
  /// deal with this is to follow RAII principle, https://en.cppreference.com/w/cpp/language/raii.

	float x = _rowNumberMaxWidth;

	for(int col=0; col<_model->columnCount(); col++)
	{
		_colXPositions[col] = x;
		x += _dataColsMaxWidth[col];
	}

	_dataWidth = w;

	qreal	newWidth	= (_extraColumnItem != nullptr ? _dataRowsMaxHeight + 1 : 0 ) + _dataWidth,
			newHeight	= _dataRowsMaxHeight * (_model->rowCount() + 1);

	//Log::log() << "Settings WxH: " << newWidth << "X" << newHeight << std::endl;

	setWidth(	newWidth);
	setHeight(	newHeight);
	_recalculateCellSizes = false;

	//emit itemSizeChanged(); //This calls reloadTextItems, reloadRowNumbers and reloadColumnHeaders and those all call viewPortChanged. Which recreates them all every time if necessary... Nobody else seems to emit this signal anywhere so I dont see the point. Ill replace it with viewPortChanged

	viewportChanged();
	
	JASPTIMER_STOP(calculateCellSizes);
}

void DataSetView::viewportChanged()
{
	if(_model == nullptr || _viewportX != _viewportX || _viewportY != _viewportY || _viewportW != _viewportW || _viewportH != _viewportH ) //only possible if they are NaN
		return;

	if(_dataColsMaxWidth.size() != _model->columnCount())
		return;

	JASPTIMER_RESUME(viewportChanged);

#ifdef DATASETVIEW_DEBUG_VIEWPORT
	Log::log() << "viewportChanged!\n" <<std::flush;
#endif

	determineCurrentViewPortIndices();
	storeOutOfViewItems();
	buildNewLinesAndCreateNewItems();

	JASPTIMER_RESUME(updateCalledForRender);
	update();
	JASPTIMER_STOP(updateCalledForRender);

	_previousViewportColMin = _currentViewportColMin;
	_previousViewportColMax = _currentViewportColMax;
	_previousViewportRowMin = _currentViewportRowMin;
	_previousViewportRowMax = _currentViewportRowMax;

	JASPTIMER_STOP(viewportChanged);
}


void DataSetView::determineCurrentViewPortIndices()
{
	JASPTIMER_RESUME(determineCurrentViewPortIndices);
	QVector2D leftTop(_viewportX, _viewportY);
	QVector2D viewSize(_viewportW, _viewportH);
	QVector2D rightBottom(leftTop + viewSize);

	_currentViewportColMax = -1;
	_currentViewportColMin = -1;

	float cumulative = 0;
	for(int col=0; col<_model->columnCount() && _currentViewportColMax == -1; col++)
	{
		if(_currentViewportColMax == -1 && cumulative > rightBottom.x())						_currentViewportColMax = col;

		float prevCum = cumulative;
		cumulative += _dataColsMaxWidth[col];

		if(_currentViewportColMin == -1 && cumulative > leftTop.x() && prevCum < leftTop.x())	_currentViewportColMin = col;
	}

	if(_currentViewportColMax == -1)
		_currentViewportColMax = _model->columnCount();

	_currentViewportColMin = std::max(0, std::min(_model->columnCount(),	_currentViewportColMin							- _viewportMargin));
	_currentViewportColMax = std::max(0, std::min(_model->columnCount(),	_currentViewportColMax							+ _viewportMargin));

	_currentViewportRowMin = std::max(0, std::min(_model->rowCount(),		qRound(leftTop.y()		/ _dataRowsMaxHeight)	- (1 + _viewportMargin)));
	_currentViewportRowMax = std::max(0, std::min(_model->rowCount(),		qRound(rightBottom.y()	/ _dataRowsMaxHeight)	+ (1 + _viewportMargin)));

#ifdef DATASETVIEW_DEBUG_VIEWPORT
	Log::log() << "viewport X: " << _viewportX << " Y: " << _viewportY << " W: " << _viewportW << " H: " << _viewportH <<  std::endl;
	Log::log() << "_previousViewport\tColMin: " << _previousViewportColMin << "\tColMax: " << _previousViewportColMax << "\tRowMin: " << _previousViewportRowMin << "\tRowMax: " << _previousViewportRowMax << "\n";
	Log::log() << "_currentViewport\tColMin: "  << _currentViewportColMin  << "\tColMax: " << _currentViewportColMax  << "\tRowMin: " << _currentViewportRowMin  << "\tRowMax: " << _currentViewportRowMax  << "\n" << std::flush;
#endif
	JASPTIMER_STOP(determineCurrentViewPortIndices);
}

void DataSetView::storeOutOfViewItems()
{
	JASPTIMER_RESUME(storeOutOfViewItems);

	int maxRows = _model->rowCount(), maxCols = _model->columnCount();
	if(
			_previousViewportRowMin >= 0		&& _previousViewportRowMax >= 0			&& _previousViewportColMin >= 0			&& _previousViewportColMax >= 0 &&
			_previousViewportRowMin < maxRows	&& _previousViewportRowMax <= maxRows	&& _previousViewportColMin < maxCols	&& _previousViewportColMax <= maxCols
	)
	{
		for(int col=_previousViewportColMin; col<_previousViewportColMax; col++)
		{
			for(int row=_previousViewportRowMin; row < _currentViewportRowMin; row++)
				storeTextItem(row, col);

			for(int row=_currentViewportRowMax; row < _previousViewportRowMax; row++)
				storeTextItem(row, col);
		}

		for(int row=_previousViewportRowMin; row<_previousViewportRowMax; row++)
		{
			for(int col=_previousViewportColMin; col < _currentViewportColMin; col++)
				storeTextItem(row, col);

			for(int col=_currentViewportColMax; col < _previousViewportColMax; col++)
				storeTextItem(row, col);
		}

		for(int row=_previousViewportRowMin; row < _currentViewportRowMin; row++)
			storeRowNumber(row);

		for(int row=_currentViewportRowMax; row < _previousViewportRowMax; row++)
			storeRowNumber(row);
	}

	JASPTIMER_STOP(storeOutOfViewItems);
}

void DataSetView::addLine(float x0, float y0, float x1, float y1)
{
	if(_lines.size() < _linesActualSize + 4)
		_lines.resize(_lines.size() + 64);

	//_linesActualSize must always be a multiple of 4 because:
	_lines[_linesActualSize++] = x0;
	_lines[_linesActualSize++] = y0;
	_lines[_linesActualSize++] = x1;
	_lines[_linesActualSize++] = y1;
}

QSizeF DataSetView::getTextSize(const QString & text) const
{
	return JaspTheme::fontMetrics().size(Qt::TextSingleLine, text);
}

void DataSetView::buildNewLinesAndCreateNewItems()
{
	JASPTIMER_RESUME(buildNewLinesAndCreateNewItems);

	if(_currentViewportColMax == -1 ||  _currentViewportColMin == -1 || _currentViewportRowMax == -1 || _currentViewportRowMin == -1)
		return;

#ifdef DATASETVIEW_ADD_LINES_PLEASE
	_linesActualSize = 0;
	size_t expectedLinesSize = (_currentViewportColMax - _currentViewportColMin) * (_currentViewportRowMax - _currentViewportRowMin) * 4 * 2;
	if(_lines.size() < expectedLinesSize)
		_lines.resize(expectedLinesSize);
#endif

	//and now we should create some new ones!

	float	maxXForVerticalLine	= _viewportX + _viewportW - extraColumnWidth(), //To avoid seeing lines through add computed column button
			maxYForVerticalLine = _viewportY + _dataRowsMaxHeight;

	JASPTIMER_RESUME(buildNewLinesAndCreateNewItems_GRID);

	for(int col=_currentViewportColMin; col<_currentViewportColMax; col++)
		for(int row=_currentViewportRowMin; row<_currentViewportRowMax; row++)
		{
			float	pos0x(				_colXPositions[col]		),
					pos0y((1 + row) *	_dataRowsMaxHeight		),
					pos1x(pos0x +		_dataColsMaxWidth[col]	),
					pos1y((2 + row) *	_dataRowsMaxHeight		);

			JASPTIMER_RESUME(buildNewLinesAndCreateNewItems_GRID_DATA);
			if(_storedLineFlags.count(row) == 0 || _storedLineFlags[row].count(col) == 0)
				_storedLineFlags[row][col] = static_cast<unsigned char>(_model->data(_model->index(row, col), _roleNameToRole["lines"]).toInt());
			unsigned char lineFlags = _storedLineFlags[row][col];
			JASPTIMER_STOP(buildNewLinesAndCreateNewItems_GRID_DATA);

			/*
			 *			---------- up ----------
			 *			|left|            |right|
			 *			--------- down ---------
			 */
			bool	left	= (lineFlags & 1) > 0	&& pos0x  > _rowNumberMaxWidth + _viewportX,
					right	= (lineFlags & 2) > 0	&& pos1x  > _rowNumberMaxWidth + _viewportX,
					up		= (lineFlags & 4) > 0	&& pos0y  > _dataRowsMaxHeight + _viewportY,
					down	= (lineFlags & 8) > 0	&& pos1y  > _dataRowsMaxHeight + _viewportY;

#ifdef DATASETVIEW_SHOW_ITEMS_PLEASE
			createTextItem(row, col);
#endif


#ifdef DATASETVIEW_ADD_LINES_PLEASE
			if(up)		addLine(pos1x, pos0y, pos0x, pos0y);
			if(down)	addLine(pos0x, pos1y, pos1x, pos1y);


			if(left)
			{
				if(pos0x > maxXForVerticalLine)	addLine(pos0x, std::max(pos1y, maxYForVerticalLine),	pos0x, std::max(pos0y, maxYForVerticalLine));
				else							addLine(pos0x, pos1y,									pos0x, pos0y);
			}

			if(right)
			{
				if(pos1x > maxXForVerticalLine) addLine(pos1x, std::max(pos0y, maxYForVerticalLine),	pos1x, std::max(pos1y, maxYForVerticalLine));
				else							addLine(pos1x, pos0y,									pos1x, pos1y);
			}
#endif
		}

	JASPTIMER_STOP(buildNewLinesAndCreateNewItems_GRID);

#ifdef DATASETVIEW_ADD_LINES_PLEASE
	addLine(_viewportX + 0.5f,					_viewportY,							_viewportX + 0.5f,					_viewportY + _viewportH);
	addLine(_viewportX + _rowNumberMaxWidth,	_viewportY,							_viewportX + _rowNumberMaxWidth,	_viewportY + _viewportH);

	addLine(_viewportX,							_viewportY + 0.5f,					_viewportX + _viewportW,			_viewportY+ 0.5f);
	addLine(_viewportX,							_viewportY + _dataRowsMaxHeight,	_viewportX + _viewportW,			_viewportY + _dataRowsMaxHeight);

	if(_extraColumnItem != nullptr)
	{
		addLine(_viewportX + _viewportW - extraColumnWidth(),	_viewportY,		_viewportX + _viewportW - extraColumnWidth(),	_viewportY + _dataRowsMaxHeight);
		addLine(_viewportX + _viewportW,						_viewportY,		_viewportX + _viewportW,						_viewportY + _dataRowsMaxHeight);
	}
#endif

	for(int row=_currentViewportRowMin; row<_currentViewportRowMax; row++)
	{
		if(createRowNumber(row))
		{

#ifdef DATASETVIEW_ADD_LINES_PLEASE
			float	pos0x(_viewportX),
					pos0y((1 + row) * _dataRowsMaxHeight),
					pos1x(_viewportX + _rowNumberMaxWidth),
					pos1y((2 + row) * _dataRowsMaxHeight);

			if(pos0y > _dataRowsMaxHeight + _viewportY)
				addLine(pos0x, pos0y, pos1x, pos0y);


			if(row == _model->rowCount() - 1 && pos1y > _dataRowsMaxHeight + _viewportY)
				addLine(pos0x, pos1y, pos1x, pos1y);
#endif
		}
	}

	for(int col=_currentViewportColMin; col<_currentViewportColMax; col++)
	{

		createColumnHeader(col);

#ifdef DATASETVIEW_ADD_LINES_PLEASE
		float	pos0x(_colXPositions[col]),
				pos0y(_viewportY),
				pos1x(pos0x + _dataColsMaxWidth[col]),
				pos1y(pos0y + _dataRowsMaxHeight);

		if(pos0x  > _rowNumberMaxWidth + _viewportX && pos0x <= maxXForVerticalLine)
			addLine(pos0x, pos0y, pos0x, pos1y);


		if(col == _model->columnCount() - 1 && pos1x  > _rowNumberMaxWidth + _viewportX && pos1x <= maxXForVerticalLine)
			addLine(pos1x, pos0y, pos1x, pos1y);
#endif
	}

#ifdef DATASETVIEW_ADD_LINES_PLEASE
	_linesWasChanged = true;
#endif

	createleftTopCorner();
	updateExtraColumnItem();

	JASPTIMER_STOP(buildNewLinesAndCreateNewItems);
}

QQuickItem * DataSetView::createTextItem(int row, int col)
{
	JASPTIMER_RESUME(createTextItem);

#ifdef DATASETVIEW_DEBUG_CREATION
	Log::log() << "createTextItem(\t"<<row<<",\t"<<col<<") called!\titemStore contains #"<< _textItemStorage.size() << "\n" << std::flush;
#endif

	if((_cellTextItems.count(col) == 0 && _cellTextItems[col].count(row) == 0) || _cellTextItems[col][row] == nullptr)
	{

		if(_itemDelegate == nullptr)
		{
			_itemDelegate = new QQmlComponent(qmlEngine(this));
			_itemDelegate->setData("import QtQuick 2.9\nText { text: itemText; color: itemActive ? 'black' : 'grey'; verticalAlignment: Text.AlignVCenter; }", QUrl());
		}

		QQuickItem			* textItem	= nullptr;
		ItemContextualized	* itemCon	= nullptr;

		QModelIndex ind(_model->index(row, col));
		bool active = _model->data(ind, _roleNameToRole["filter"]).toBool();

		if(_textItemStorage.size() > 0)
		{
			JASPTIMER_RESUME(createTextItem textItemStorage has something);
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createTextItem("<<row<<", "<<col<<") from storage!\n" << std::flush;
#endif
			itemCon = _textItemStorage.top();
			textItem = itemCon->item;
			_textItemStorage.pop();
			setStyleDataItem(itemCon->context, active, col, row);
			JASPTIMER_STOP(createTextItem textItemStorage has something);
		}
		else
		{
			JASPTIMER_RESUME(createTextItem textItemStorage has NOTHING);
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createTextItem("<<row<<", "<<col<<") ex nihilo!\n" << std::flush;
#endif
			QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
			itemCon = new ItemContextualized(setStyleDataItem(nullptr, active, col, row));
			_itemDelegate->create(localIncubator, itemCon->context);

            if(localIncubator.isError())
                throw std::runtime_error("Something went wrong incubating an item delegate for tableview!");

			textItem = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = textItem;

			textItem->setParent(this);
			textItem->setParentItem(this);

			JASPTIMER_STOP(createTextItem textItemStorage has NOTHING);
		}

		JASPTIMER_RESUME(createTextItem setValues);

		textItem->setHeight(_dataRowsMaxHeight		- (2 * _itemVerticalPadding));
		textItem->setWidth(_dataColsMaxWidth[col]	- (2 * _itemHorizontalPadding));

		textItem->setX(_colXPositions[col]				+ _itemHorizontalPadding);
		textItem->setY(((row + 1) * _dataRowsMaxHeight)	+ _itemVerticalPadding);

		textItem->setZ(-4);
		textItem->setVisible(true);

		_cellTextItems[col][row] = itemCon;

		JASPTIMER_STOP(createTextItem setValues);
	}

	JASPTIMER_STOP(createTextItem);

	return _cellTextItems[col][row]->item;
}

void DataSetView::storeTextItem(int row, int col, bool cleanUp)
{
	if((_cellTextItems.count(col) == 0 && _cellTextItems[col].count(row) == 0) || _cellTextItems[col][row] == nullptr) return;

#ifdef DATASETVIEW_DEBUG_CREATION
	Log::log() << "storeTextItem("<<row<<", "<<col<<") in storage!\n" << std::flush;
#endif

	JASPTIMER_RESUME(storeTextItem);

	ItemContextualized * textItem = _cellTextItems[col][row];
	_cellTextItems[col][row] = nullptr;

	if(cleanUp)
	{
		_cellTextItems[col].erase(row);

		if(_cellTextItems[col].size() == 0)
			_cellTextItems.erase(col);
	}

	textItem->item->setVisible(false);

	if (_cacheItems)		_textItemStorage.push(textItem);
	else					delete textItem;

	JASPTIMER_STOP(storeTextItem);
}



QQuickItem * DataSetView::createRowNumber(int row)
{
	//Log::log() << "createRowNumber("<<row<<") called!\n" << std::flush;

	if(rowNumberWidth() == 0)
		return nullptr;

	if(_rowNumberDelegate == nullptr)
	{
		_rowNumberDelegate = new QQmlComponent(qmlEngine(this));
        _rowNumberDelegate->setData("import QtQuick 2.9\nItem {\n"
			"Rectangle	{ color: jaspTheme.uiBackground;	anchors.fill: parent }\n"
			"Text		{ text: rowIndex; anchors.centerIn: parent; color: jaspTheme.textEnabled; }\n"
		"}", QUrl());
	}

	QQuickItem * rowNumber = nullptr;
	ItemContextualized * itemCon = nullptr;

	if(_rowNumberItems.count(row) == 0  || _rowNumberItems[row] == nullptr)
	{
		if(_rowNumberStorage.size() > 0)
		{
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createRowNumber("<<row<<") from storage!\n" << std::flush;
#endif
			 itemCon = _rowNumberStorage.top();
			_rowNumberStorage.pop();
			rowNumber = itemCon->item;
			
			setStyleDataRowNumber(itemCon->context,
								  _model->headerData(row, Qt::Orientation::Vertical).toString(),
								  row);
		}
		else
		{
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createRowNumber("<<row<<") ex nihilo!\n" << std::flush;
#endif
			QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
			itemCon = new ItemContextualized(setStyleDataRowNumber(nullptr,
																   _model->headerData(row, Qt::Orientation::Vertical).toString(),
																   row));

			_rowNumberDelegate->create(localIncubator, itemCon->context);
			rowNumber = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = rowNumber;

			rowNumber->setParent(this);
			rowNumber->setParentItem(this);
		}

		//rowNumber->setProperty("text", QString::fromStdString(std::to_string(row + 1))); //Nobody wants zero-based rows...

		rowNumber->setHeight(_dataRowsMaxHeight);
		rowNumber->setWidth(_rowNumberMaxWidth);

		rowNumber->setVisible(true);

		_rowNumberItems[row] = itemCon;
	}
	else
		rowNumber = _rowNumberItems[row]->item;

	rowNumber->setX(_viewportX);
	rowNumber->setY(_dataRowsMaxHeight * (1 + row));
	rowNumber->setZ(-3);

	return _rowNumberItems[row]->item;
}

void DataSetView::storeRowNumber(int row)
{
	if(_rowNumberItems.count(row) == 0  || _rowNumberItems[row] == nullptr) return;

#ifdef DATASETVIEW_DEBUG_CREATION
	Log::log() << "storeRowNumber("<<row<<") in storage!\n" << std::flush;
#endif

	ItemContextualized * rowNumber = _rowNumberItems[row];
	_rowNumberItems[row] = nullptr;

	_rowNumberItems.erase(row);

	rowNumber->item->setVisible(false);

	_rowNumberStorage.push(rowNumber);
}


QQuickItem * DataSetView::createColumnHeader(int col)
{
	//Log::log() << "createColumnHeader("<<col<<") called!\n" << std::flush;

	if(_columnHeaderDelegate == nullptr)
	{
		_columnHeaderDelegate = new QQmlComponent(qmlEngine(this));
        _columnHeaderDelegate->setData("import QtQuick 2.9\nItem {\n"
			"Rectangle	{ color: jaspTheme.uiBackground;	anchors.fill: parent }\n"
			"Text		{ text: headerText; anchors.centerIn: parent; color: jaspTheme.textEnabled; }\n"
		"}", QUrl());
	}


	QQuickItem * columnHeader = nullptr;
	ItemContextualized * itemCon = nullptr;

	if(_columnHeaderItems.count(col) == 0  || _columnHeaderItems[col] == nullptr)
	{
		if(_columnHeaderStorage.size() > 0)
		{
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createColumnHeader("<<col<<") from storage!\n" << std::flush;
#endif
			itemCon = _columnHeaderStorage.top();
			_columnHeaderStorage.pop();
			columnHeader = itemCon->item;

			setStyleDataColumnHeader(itemCon->context,
									_model->headerData(col, Qt::Orientation::Horizontal).toString(),
									col,
									_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["columnIsComputed"]).toBool(),
									_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["computedColumnIsInvalidated"]).toBool(),
									_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["filter"]).toBool(),
									_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["computedColumnError"]).toString());
		}
		else
		{
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createColumnHeader("<<col<<") ex nihilo!\n" << std::flush;
#endif
			QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
			itemCon = new ItemContextualized(setStyleDataColumnHeader(
												nullptr,
												_model->headerData(col, Qt::Orientation::Horizontal).toString(),
												col,
												_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["columnIsComputed"]).toBool(),
												_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["computedColumnIsInvalidated"]).toBool(),
												_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["filter"]).toBool(),
												_model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["computedColumnError"]).toString()));

			_columnHeaderDelegate->create(localIncubator, itemCon->context);
			columnHeader = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = columnHeader;

			columnHeader->setParent(this);
			columnHeader->setParentItem(this);
		}


		columnHeader->setHeight(_dataRowsMaxHeight);
		columnHeader->setWidth(_dataColsMaxWidth[col]);

		columnHeader->setVisible(true);

		_columnHeaderItems[col] = itemCon;
	}
	else
		columnHeader = _columnHeaderItems[col]->item;

	columnHeader->setX(_colXPositions[col]);
	columnHeader->setY(_viewportY);
	columnHeader->setZ(-3);

	return columnHeader;
}

void DataSetView::storeColumnHeader(int col)
{
	if(_columnHeaderItems.count(col) == 0  || _columnHeaderItems[col] == nullptr) return;

#ifdef DATASETVIEW_DEBUG_CREATION
	Log::log() << "storeColumnHeader("<<col<<") in storage!\n" << std::flush;
#endif

	ItemContextualized * columnHeader = _columnHeaderItems[col];
	_columnHeaderItems[col] = nullptr;

	_columnHeaderItems.erase(col);

	columnHeader->item->setVisible(false);

	_columnHeaderStorage.push(columnHeader);
}

QQuickItem * DataSetView::createleftTopCorner()
{
	//Log::log() << "createleftTopCorner() called!\n" << std::flush;
	if(_leftTopItem == nullptr)
	{

		if(_leftTopCornerDelegate == nullptr)
		{
			_leftTopCornerDelegate = new QQmlComponent(qmlEngine(this));
            _leftTopCornerDelegate->setData("import QtQuick 2.9\nItem {}", QUrl());
		}

		QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
		_leftTopCornerDelegate->create(localIncubator);
		_leftTopItem = qobject_cast<QQuickItem*>(localIncubator.object());

		_leftTopItem->setParent(this);
		_leftTopItem->setParentItem(this);

		_leftTopItem->setZ(-1);



		_leftTopItem->setVisible(true);
	}

	_leftTopItem->setHeight(_dataRowsMaxHeight);
	_leftTopItem->setWidth(_rowNumberMaxWidth);
	_leftTopItem->setX(_viewportX);
	_leftTopItem->setY(_viewportY);

	return _leftTopItem;
}

void DataSetView::updateExtraColumnItem()
{
	//Log::log() << "createleftTopCorner() called!\n" << std::flush;
	if(_extraColumnItem == nullptr)
		return;

	_extraColumnItem->setHeight(_dataRowsMaxHeight);
	_extraColumnItem->setX(_viewportX + _viewportW - extraColumnWidth());
	_extraColumnItem->setY(_viewportY);

	connect(_extraColumnItem, &QQuickItem::widthChanged, this, &DataSetView::setExtraColumnX, Qt::UniqueConnection);
}

void DataSetView::setExtraColumnX()
{
	_extraColumnItem->setX(_viewportX + _viewportW - extraColumnWidth());
}

QQmlContext * DataSetView::setStyleDataItem(QQmlContext * previousContext, bool active, size_t col, size_t row)
{
	QModelIndex idx = _model->index(row, col);

	bool isEditable(_model->flags(idx) & Qt::ItemIsEditable);

	if(isEditable || _storedDisplayText.count(row) == 0 || _storedDisplayText[row].count(col) == 0)
		_storedDisplayText[row][col] = _model->data(idx, Qt::DisplayRole).toString();

	QString text = _storedDisplayText[row][col];

	if(previousContext == nullptr)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("itemText",			text);
	previousContext->setContextProperty("itemActive",		active);
	previousContext->setContextProperty("itemEditable",		isEditable);
	previousContext->setContextProperty("itemSelected",		_model->data(idx, _roleNameToRole["selected"]));
	previousContext->setContextProperty("itemFiltered",		_model->data(idx, _roleNameToRole["filter"]));
	previousContext->setContextProperty("itemValue",		_model->data(idx, _roleNameToRole["value"]));
	previousContext->setContextProperty("itemInputType",	_model->data(idx, _roleNameToRole["itemInputType"]));
	previousContext->setContextProperty("columnIndex",		static_cast<int>(col));
	previousContext->setContextProperty("rowIndex",			static_cast<int>(row));
	previousContext->setContextProperty("isDynamic",		true);
	previousContext->setContextProperty("tableView",		_tableViewItem);

	return previousContext;
}

QQmlContext * DataSetView::setStyleDataRowNumber(QQmlContext * previousContext, QString text, int row)
{
	if(previousContext == nullptr)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("rowIndex",			row);
	previousContext->setContextProperty("headerText",		text);

	return previousContext;
}

QQmlContext * DataSetView::setStyleDataColumnHeader(QQmlContext * previousContext, QString text, int column, bool isComputed, bool isInvalidated, bool isFiltered, QString computedError)
{
	if(previousContext == nullptr)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("headerText",			text);
	previousContext->setContextProperty("columnIndex",			column);
	previousContext->setContextProperty("columnIsComputed",		isComputed);
	previousContext->setContextProperty("columnIsInvalidated",	isInvalidated);
	previousContext->setContextProperty("columnIsFiltered",		isFiltered);
	previousContext->setContextProperty("columnError",			computedError);

	return previousContext;
}

void DataSetView::setItemDelegate(QQmlComponent * newDelegate)
{
	if(newDelegate != _itemDelegate)
	{
		if(_itemDelegate != nullptr)
			delete _itemDelegate;
		_itemDelegate = newDelegate;
		emit itemDelegateChanged();
	}
}

void DataSetView::setRowNumberDelegate(QQmlComponent * newDelegate)
{
	if(newDelegate != _rowNumberDelegate)
	{
		if(_rowNumberDelegate != nullptr)
			delete _rowNumberDelegate;
		_rowNumberDelegate = newDelegate;
		emit rowNumberDelegateChanged();
	}
}


void DataSetView::setColumnHeaderDelegate(QQmlComponent * newDelegate)
{
	if(newDelegate != _columnHeaderDelegate)
	{
		if(_columnHeaderDelegate != nullptr)
			delete _columnHeaderDelegate;
		_columnHeaderDelegate = newDelegate;
		emit columnHeaderDelegateChanged();
	}
}


void DataSetView::setLeftTopCornerItem(QQuickItem * newItem)
{
	if(newItem != _leftTopItem)
	{
		if(_leftTopItem != nullptr)
			delete _leftTopItem;
		_leftTopItem = newItem;

		if(_leftTopItem != nullptr)
		{

			_leftTopItem->setParent(this);
			_leftTopItem->setParentItem(this);


			_leftTopItem->setProperty("text", "?");

			_leftTopItem->setZ(-1);

			_leftTopItem->setVisible(true);

			_leftTopItem->setHeight(_dataRowsMaxHeight);
			_leftTopItem->setWidth(_rowNumberMaxWidth);
			_leftTopItem->setX(_viewportX);
			_leftTopItem->setY(_viewportY);

		}

		emit leftTopCornerItemChanged();
	}
}

void DataSetView::setExtraColumnItem(QQuickItem * newItem)
{
	if(newItem != _extraColumnItem)
	{
		if(_extraColumnItem != nullptr)
			delete _extraColumnItem;
		_extraColumnItem = newItem;

		if(_extraColumnItem != nullptr)
		{

			_extraColumnItem->setParent(this);
			_extraColumnItem->setParentItem(this);

			_extraColumnItem->setZ(-1);

			_extraColumnItem->setVisible(true);

			_extraColumnItem->setHeight(_dataRowsMaxHeight);
			_extraColumnItem->setX(_dataWidth);
			_extraColumnItem->setY(_viewportY);

		}

		emit extraColumnItemChanged();
	}
}

void DataSetView::reloadTextItems()
{
	//Store all current items
	for(int col=_previousViewportColMin; col< _previousViewportColMax; col++)
		for(int row=_previousViewportRowMin; row<_previousViewportRowMax; row++)
			storeTextItem(row, col);

	viewportChanged(); //rerun to get new items
}

void DataSetView::reloadRowNumbers()
{
	//Store all current items
	for(int row=_previousViewportRowMin; row<_previousViewportRowMax; row++)
		storeRowNumber(row);

	viewportChanged(); //rerun to get new items
}

void DataSetView::reloadColumnHeaders()
{
	//Store all current items
	for(int col=_previousViewportColMin; col< _previousViewportColMax; col++)
		storeColumnHeader(col);

	viewportChanged(); //rerun to get new items
}


void DataSetView::myParentChanged(QQuickItem * newParentItem)
{

	/*if(newParentItem->property("viewport").isValid())
	{
		QQuickItem * viewport = newParentItem->property("viewport").
		connect
	}
	void xChanged();
	void yChanged();
	void widthChanged();
	void heightChanged();
*/
}

#ifdef DATASETVIEW_ADD_LINES_PLEASE
QSGNode * DataSetView::updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *)
{
	//JASPTIMER_RESUME(updatePaintNode);
	if (width() <= 0 || height() <= 0) {
		delete oldNode;
		return 0;
	}

	//if(recalculateCellSizes) calculateCellContentSizes();
	//const QRectF rect = boundingRect();


	const int linesPerNode = 2048;

	if(oldNode)
	{
		delete oldNode;
		oldNode = nullptr;
	}

	if(!oldNode)				oldNode = new QSGNode();
	else if(!_linesWasChanged)	return oldNode;

	
	QSGGeometryNode * currentNode = static_cast<QSGGeometryNode*>(oldNode->firstChild());

	oldNode->markDirty(QSGNode::DirtyGeometry);


	for(int lineIndex=0; lineIndex < _linesActualSize;)
	{
		bool justAdded = false;
		
		if(currentNode == nullptr)
		{
			currentNode = new QSGGeometryNode;

			currentNode->setFlag(QSGNode::OwnsMaterial, false);
			currentNode->setFlag(QSGNode::OwnsGeometry, true);
			currentNode->setMaterial(&material);

			justAdded = true;

			oldNode->markDirty(QSGNode::DirtyNodeAdded);
		}

		int geomSize = std::min(linesPerNode, (int)(_linesActualSize - lineIndex) / 4); //_lines is floats x, y, x, y so each set of 4 is a single line.
		geomSize *= 2;
		
		QSGGeometry *geometry = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), geomSize);
		geometry->setLineWidth(1);
        geometry->setDrawingMode(QSGGeometry::DrawLines);

		assert(sizeof(float) * 2 == geometry->sizeOfVertex());

		float * vertexData = static_cast<float*>(geometry->vertexData());

		memcpy(vertexData, _lines.data() + lineIndex, geomSize * 2 * sizeof(float));
		lineIndex += 2 * geomSize;

		currentNode->setGeometry(geometry);
		
		if(justAdded)
			oldNode->appendChildNode(currentNode);

		currentNode = static_cast<QSGGeometryNode*>(currentNode->nextSibling());
	}

/*
	std::queue<QSGGeometryNode*> killThem;

	while(currentNode != nullptr) //superfluous children! Lets kill em
	{
		killThem.push(currentNode);
		currentNode = static_cast<QSGGeometryNode*>(currentNode->nextSibling());
	}

	while(killThem.size() > 0)
	{
		QSGGeometryNode * childToDie = killThem.front();
		killThem.pop();

		//oldNode->removeChildNode(childToDie);
		//oldNode->markDirty(QSGNode::DirtyNodeRemoved);

		std::cout << "DELETING STUFF" << std::endl;

		delete childToDie;
	}*/

	_linesWasChanged = false;

	//JASPTIMER_STOP(updatePaintNode);

	return oldNode;
}
#endif
