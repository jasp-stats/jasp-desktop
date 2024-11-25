#include "datasetview.h"

#include <QSGFlatColorMaterial>
#include <QSGGeometry>
#include <QSGNode>
#include <queue>
#include "timers.h"
#include "log.h"
#include "gui/preferencesmodel.h"
#include "jasptheme.h"
#include <QScreen>
#include "data/datasetpackage.h"
#include <iostream>
#include <QGuiApplication>
#include <QClipboard>
#include "utils.h"

DataSetView * DataSetView::_mainDataSetView = nullptr;


DataSetView::DataSetView(QQuickItem *parent)
	: QQuickItem (parent)
	, _selectionModel(new QItemSelectionModel(nullptr, this))
	, _model(new ExpandDataProxyModel(this))
{
	setFlag(QQuickItem::ItemHasContents);
	//setFlag(QQuickItem::ItemIsFocusScope);

	_material.setColor(Qt::gray);
	
	_delayViewportChangedTimer = new QTimer(this);
	_delayViewportChangedTimer->setInterval(0);
	_delayViewportChangedTimer->setSingleShot(true);

	connect(this,						&DataSetView::parentChanged,					this, &DataSetView::myParentChanged);
	
	connect(this,						&DataSetView::viewportXChanged,					this, &DataSetView::viewportChangedDelayed);
	connect(this,						&DataSetView::viewportYChanged,					this, &DataSetView::viewportChangedDelayed);
	connect(this,						&DataSetView::viewportWChanged,					this, &DataSetView::viewportChangedDelayed);
	connect(this,						&DataSetView::viewportHChanged,					this, &DataSetView::viewportChangedDelayed);
	connect(_delayViewportChangedTimer, &QTimer::timeout,								this, &DataSetView::viewportChanged);

	connect(this,						&DataSetView::itemDelegateChanged,				this, &DataSetView::reloadTextItems);
	connect(this,						&DataSetView::rowNumberDelegateChanged,			this, &DataSetView::reloadRowNumbers);
	connect(this,						&DataSetView::columnHeaderDelegateChanged,		this, &DataSetView::reloadColumnHeaders);

	connect(this,						&DataSetView::itemHorizontalPaddingChanged,		this, &DataSetView::calculateCellSizes);
	connect(this,						&DataSetView::itemVerticalPaddingChanged,		this, &DataSetView::calculateCellSizes);
	connect(this,						&DataSetView::extraColumnItemChanged,			this, &DataSetView::calculateCellSizes);

	connect(this,						&DataSetView::itemSizeChanged,					this, &DataSetView::reloadTextItems);
	connect(this,						&DataSetView::itemSizeChanged,					this, &DataSetView::reloadRowNumbers);
	connect(this,						&DataSetView::itemSizeChanged,					this, &DataSetView::reloadColumnHeaders);
	
	connect(this,						&DataSetView::selectionChanged,					this, &DataSetView::selectionMinChanged);
	connect(this,						&DataSetView::selectionChanged,					this, &DataSetView::selectionMaxChanged);
	
	connect(PreferencesModel::prefs(),	&PreferencesModel::uiScaleChanged,				this, &DataSetView::resetItems,				Qt::QueuedConnection);
	connect(PreferencesModel::prefs(),	&PreferencesModel::interfaceFontChanged,		this, &DataSetView::resetItems,				Qt::QueuedConnection);

	connect(DataSetPackage::pkg(),		&DataSetPackage::dataModeChanged,				this, &DataSetView::onDataModeChanged);
	connect(_model,						&ExpandDataProxyModel::undoChanged,				this, &DataSetView::undoChanged);
	
	setZ(10);
	
	

	
}

void DataSetView::setModel(QAbstractItemModel * model)
{
	_model->setSourceModel(model);

	if (model)
	{
		connect(model,				&QAbstractItemModel::modelReset,				this, &DataSetView::modelWasReset				);
		connect(model,				&QAbstractItemModel::dataChanged,				this, &DataSetView::modelDataChanged			);
		connect(model,				&QAbstractItemModel::headerDataChanged,			this, &DataSetView::modelHeaderDataChanged		);
		connect(model,				&QAbstractItemModel::modelAboutToBeReset,		this, &DataSetView::modelAboutToBeReset			);

		connect(_selectionModel,	&QItemSelectionModel::selectionChanged,			this, &DataSetView::selectionChanged);
		connect(_selectionModel,	&QItemSelectionModel::currentColumnChanged,		this, &DataSetView::currentSelectedColumnHandler);

		connect(model,				&QAbstractItemModel::columnsAboutToBeInserted,	this, &DataSetView::columnsAboutToBeInserted	);
		connect(model,				&QAbstractItemModel::columnsAboutToBeRemoved,	this, &DataSetView::columnsAboutToBeRemoved		);
		connect(model,				&QAbstractItemModel::rowsAboutToBeInserted,		this, &DataSetView::rowsAboutToBeInserted		);
		connect(model,				&QAbstractItemModel::rowsAboutToBeRemoved,		this, &DataSetView::rowsAboutToBeRemoved		);
		connect(model,				&QAbstractItemModel::columnsInserted,			this, &DataSetView::columnsInserted,			Qt::QueuedConnection);
		connect(model,				&QAbstractItemModel::columnsRemoved,			this, &DataSetView::columnsRemoved,				Qt::QueuedConnection);
		connect(model,				&QAbstractItemModel::rowsInserted,				this, &DataSetView::rowsInserted,				Qt::QueuedConnection);
		connect(model,				&QAbstractItemModel::rowsRemoved,				this, &DataSetView::rowsRemoved,				Qt::QueuedConnection);
		
		
		_selectionModel->setModel(model);
		emit selectionModelChanged(); //Or maybe it hasn't?
		
		setRowNumberWidth(getRowHeaderSize().width());

		//recalculateCellSizes = true;
		calculateCellSizes();
		update();

		emit modelChanged();
	}
}

QSizeF DataSetView::getColumnSize(int col)
{
	QVariant maxColStringVar = _model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("maxColString"));
	
	QSizeF colSize;
	
	if(!maxColStringVar.isNull())
		colSize = getTextSize(maxColStringVar.toString());
	else
	{
		QVariant columnWidthFallbackVar = _model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("columnWidthFallback"));

		colSize = getTextSize("??????");

		if(!columnWidthFallbackVar.isNull())
			colSize.setWidth(columnWidthFallbackVar.toFloat() - itemHorizontalPadding() * 2);
	}
	
	return  QSizeF(_maxColWidth <= 0 ? colSize.width() : std::min(colSize.width(), double(_maxColWidth)), colSize.height());
}

QSizeF DataSetView::getRowHeaderSize()
{
	QString text = _model->headerData(0, Qt::Orientation::Vertical, _model->getRole("maxRowHeaderString")).toString();

	return getTextSize(text);
}

void DataSetView::clearCaches()
{
	JASPTIMER_SCOPE(DataSetView::clearCaches);
	Log::log() << "DataSetView::clearCaches\n" << std::flush;
	
	for(auto storage : {_textItemStorage, _rowNumberStorage, _columnHeaderStorage})
		while(storage.size() > 0)
		{
			const ItemContextualized * ic = storage.top();	
			storage.pop();
			delete ic;
		}
	
	_textItemStorage		= {};
	_rowNumberStorage		= {};
	_columnHeaderStorage	= {};
}

void DataSetView::modelDataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight, const QVector<int> &roles)
{
	const int	colMin = std::max(0,						topLeft.column()),
				colMax = std::min(_model->columnCount(),	bottomRight.column()),
				rowMin = std::max(0,						topLeft.row()),
				rowMax = std::min(_model->rowCount(),		bottomRight.row());

	QSizeF calcSize = getColumnSize(colMin);

	if (_cacheItems || int(_cellSizes[size_t(colMin)].width() * 10) != int(calcSize.width() * 10)) //If we cache items we are not expecting the user to make regular manual changes to the data, so if something changes we can do a reset. Otherwise we are in TableView and we do it only when the column size changes.
		calculateCellSizes();
	else if (roles.contains(int(DataSetPackage::specialRoles::selected)) || roles.contains(Qt::DisplayRole))
	{
		// This is a special case for the VariablesWindows & TableView: caching mixed up the items, so it can't be used
		// but the selected context property must be updated for VariablesWindows
		// and the itemText must be updated for Grid TableView (used in Plot Editor).
		for (int col = colMin; col <= colMax; col++)
			for (int row = rowMin; row <= rowMax; row++)
			{
				ItemContextualized* itemCon = _cellTextItems[col][row];

				if (itemCon)
				{
					QQmlContext* context = itemCon->context;
					if (roles.contains(int(DataSetPackage::specialRoles::selected)))
						context->setContextProperty("itemSelected",	_model->data(row, col, _model->getRole("selected")));
					
					if (roles.contains(Qt::DisplayRole))
					{
						//Changes here should be considered also for DataSetView::setStyleDataItem:
						context->setContextProperty("itemText",			_model->data(row, col));
						context->setContextProperty("itemShadowText",	_model->data(row, col, _model->getRole("shadowDisplay")));
						context->setContextProperty("itemLabel",		_model->data(row, col, _model->getRole("label")));
						context->setContextProperty("itemValue",		_model->data(row, col, _model->getRole("value")));
					}
				}
			}
	}

	if (editing() && (topLeft == bottomRight)) // In case of Undo/Redo set the focus on the correspondig cell
		positionEditItem(topLeft.row(), topLeft.column());


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
	calculateCellSizes();
	
	/*QModelIndex startIndex	= _model->index(_selectionStart.y(),	_selectionStart.x()),
				endIndex	= _model->index(_selectionEnd.y(),		_selectionEnd.x());
	
	if(!endIndex.isValid())
	{
		if(!startIndex.isValid())
			_selectionModel->clear();
		else
			_selectionModel->select(startIndex, QItemSelectionModel::ClearAndSelect);
	}
	else
		_selectionModel->select(QItemSelection(startIndex, endIndex), QItemSelectionModel::ClearAndSelect);*/
}

void DataSetView::resetItems()
{
	calculateCellSizesAndClear(true); //We clear storage because otherwise the wrong font/scaling gets remembered by the item
}

void DataSetView::calculateCellSizesAndClear(bool clearStorage)
{
	JASPTIMER_SCOPE(DataSetView::calculateCellSizes);

	_cellSizes.clear();
	_dataColsMaxWidth.clear();
	_storedLineFlags.clear();
	_storedDisplayText.clear();

    storeAllItems();
	
	//if(clearStorage)
	//	clearCaches();

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

	float x = _rowNumberMaxWidth;

	for(int col=0; col<_model->columnCount(); col++)
	{
		_colXPositions[col] = x;
		x += _dataColsMaxWidth[col];
	}

	_dataWidth = w;

	qreal	newWidth	= (_extraColumnItem != nullptr && !expandDataSet() ? _dataRowsMaxHeight + 1 : 0 ) + _dataWidth,
			newHeight	= _dataRowsMaxHeight * (_model->rowCount() + 1);

	//Log::log() << "Settings WxH: " << newWidth << "X" << newHeight << std::endl;

	setWidth(	newWidth);
	setHeight(	newHeight);
	_recalculateCellSizes = false;

	//emit itemSizeChanged(); //This calls reloadTextItems, reloadRowNumbers and reloadColumnHeaders and those all call viewPortChanged. Which recreates them all every time if necessary... Nobody else seems to emit this signal anywhere so I dont see the point. Ill replace it with viewPortChanged

	viewportChangedDelayed();
}

void DataSetView::viewportChangedDelayed()
{
    //_delayViewportChangedTimer->start();
    viewportChanged();
}

void DataSetView::viewportChanged()
{
	if(_model == nullptr || _model->sourceModel() == nullptr || _viewportX != _viewportX || _viewportY != _viewportY || _viewportW != _viewportW || _viewportH != _viewportH ) //only possible if they are NaN
		return;

	if(_dataColsMaxWidth.size() != _model->columnCount())
		return;

	JASPTIMER_RESUME(DataSetView::viewportChanged);

#ifdef DATASETVIEW_DEBUG_VIEWPORT
	Log::log() << "viewportChanged!\n" <<std::flush;
#endif

	determineCurrentViewPortIndices();
    storeOutOfViewItems();
	buildNewLinesAndCreateNewItems();

	JASPTIMER_RESUME(DataSetView::updateCalledForRender);
	update();
	JASPTIMER_STOP(DataSetView::updateCalledForRender);

	_previousViewportColMin = _currentViewportColMin;
	_previousViewportColMax = _currentViewportColMax;
	_previousViewportRowMin = _currentViewportRowMin;
	_previousViewportRowMax = _currentViewportRowMax;

	JASPTIMER_STOP(DataSetView::viewportChanged);
}


void DataSetView::determineCurrentViewPortIndices()
{
	JASPTIMER_RESUME(DataSetView::determineCurrentViewPortIndices);
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
	JASPTIMER_STOP(DataSetView::determineCurrentViewPortIndices);
}

void DataSetView::storeAllItems()
{
    JASPTIMER_SCOPE(DataSetView::storeAllItems);

    for(auto & subVec : _cellTextItems)
    {
        for(auto & intTextItem : subVec.second)
        {
            if(intTextItem.second)
            {
                intTextItem.second->item->setVisible(false);

                if (_cacheItems)		_textItemStorage.push(intTextItem.second);
                else					delete intTextItem.second;
            }
        }
        subVec.second.clear();
    }

    _cellTextItems.clear();

    for(auto & intItem : _columnHeaderItems)
    {
        intItem.second->item->setVisible(false);

        if (_cacheItems)		_columnHeaderStorage.push(intItem.second);
        else					delete intItem.second;
    }

    _columnHeaderItems.clear();

    for(auto & intItem : _rowNumberItems)
    {
        intItem.second->item->setVisible(false);

        if (_cacheItems)		_rowNumberStorage.push(intItem.second);
        else					delete intItem.second;
    }

    _rowNumberItems.clear();
}

void DataSetView::storeOutOfViewItems()
{
    JASPTIMER_SCOPE(DataSetView::storeOutOfViewItems);

    {
        ItemCxsByColRow cleanList;

        for(auto & subVec : _cellTextItems)
        {
            for(auto & intTextItem : subVec.second)
            {
                if(intTextItem.second)
                {
                    int col = subVec.first,
                        row = intTextItem.first;

                    if(col < _currentViewportColMin || col > _currentViewportColMax || row < _currentViewportRowMin || row > _currentViewportRowMax)
                    {
                        intTextItem.second->item->setVisible(false);

                        if (_cacheItems)		_textItemStorage.push(intTextItem.second);
                        else					delete intTextItem.second;
                    }
                    else
                    {
                        cleanList[col][row] = intTextItem.second;
                    }
                }
            }
        }

       _cellTextItems = cleanList ;
    }

    {
        ItemCxsByIndex cleanList;

        for(auto & intItem : _columnHeaderItems)
        {
            int col = intItem.first;

            if(col < _currentViewportColMin || col > _currentViewportColMax)
            {
                intItem.second->item->setVisible(false);

                if (_cacheItems)		_columnHeaderStorage.push(intItem.second);
                else					delete intItem.second;
            }
            else
                cleanList[col]  = intItem.second;
        }

        _columnHeaderItems = cleanList;
    }


    {
        ItemCxsByIndex cleanList;

        for(auto & intItem : _rowNumberItems)
        {
            int row = intItem.first;

            if(row < _currentViewportRowMin || row > _currentViewportRowMax)
            {
                intItem.second->item->setVisible(false);

                if (_cacheItems)		_rowNumberStorage.push(intItem.second);
                else					delete intItem.second;
            }
            else
                cleanList[row] = intItem.second;
        }

        _rowNumberItems = cleanList;
    }
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
	JASPTIMER_RESUME(DataSetView::buildNewLinesAndCreateNewItems);

	if(_currentViewportColMax == -1 ||  _currentViewportColMin == -1 || _currentViewportRowMax == -1 || _currentViewportRowMin == -1)
		return;

#ifdef ADD_LINES_PLEASE
	_linesActualSize = 0;
	size_t expectedLinesSize = (_currentViewportColMax - _currentViewportColMin) * (_currentViewportRowMax - _currentViewportRowMin) * 4 * 2;
	if(_lines.size() < expectedLinesSize)
		_lines.resize(expectedLinesSize);
#endif

	//and now we should create some new ones!

	float	maxXForVerticalLine	= _viewportX + _viewportW - extraColumnWidth(), //To avoid seeing lines through add computed column button
			maxYForVerticalLine = _viewportY + _dataRowsMaxHeight;

	JASPTIMER_RESUME(DataSetView::buildNewLinesAndCreateNewItems_GRID);

	for(int col=_currentViewportColMin; col<_currentViewportColMax; col++)
		for(int row=_currentViewportRowMin; row<_currentViewportRowMax; row++)
		{
			float	pos0x(				_colXPositions[col]		),
					pos0y((1 + row) *	_dataRowsMaxHeight		),
					pos1x(pos0x +		_dataColsMaxWidth[col]	),
					pos1y((2 + row) *	_dataRowsMaxHeight		);

			JASPTIMER_RESUME(DataSetView::buildNewLinesAndCreateNewItems_GRID_DATA);
			if(_storedLineFlags.count(row) == 0 || _storedLineFlags[row].count(col) == 0)
				_storedLineFlags[row][col] = static_cast<unsigned char>(_model->data(row, col, _model->getRole("lines")).toInt());
			unsigned char lineFlags = _storedLineFlags[row][col];
			JASPTIMER_STOP(DataSetView::buildNewLinesAndCreateNewItems_GRID_DATA);

			/*
			 *			---------- up ----------
			 *			|left|            |right|
			 *			--------- down ---------
			 */
			bool	left	= (lineFlags & 1) > 0	&& pos0x  > _rowNumberMaxWidth + _viewportX,
					right	= (lineFlags & 2) > 0	&& pos1x  > _rowNumberMaxWidth + _viewportX,
					up		= (lineFlags & 4) > 0	&& pos0y  > _dataRowsMaxHeight + _viewportY,
					down	= (lineFlags & 8) > 0	&& pos1y  > _dataRowsMaxHeight + _viewportY;

#ifdef SHOW_ITEMS_PLEASE
			if(!(editing() && row == _prevEditRow && col == _prevEditCol))
				createTextItem(row, col);
#endif


#ifdef ADD_LINES_PLEASE
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

	JASPTIMER_STOP(DataSetView::buildNewLinesAndCreateNewItems_GRID);

#ifdef ADD_LINES_PLEASE
	addLine(_viewportX + 0.5f,					_viewportY,							_viewportX + 0.5f,					_viewportY + _viewportH);
	addLine(_viewportX + _rowNumberMaxWidth,	_viewportY,							_viewportX + _rowNumberMaxWidth,	_viewportY + _viewportH);

	addLine(_viewportX,							_viewportY + 0.5f,					_viewportX + _viewportW,			_viewportY+ 0.5f);
	addLine(_viewportX,							_viewportY + _dataRowsMaxHeight,	_viewportX + _viewportW,			_viewportY + _dataRowsMaxHeight);

	if(_extraColumnItem != nullptr && !expandDataSet())
	{
		addLine(_viewportX + _viewportW - extraColumnWidth(),	_viewportY,		_viewportX + _viewportW - extraColumnWidth(),	_viewportY + _dataRowsMaxHeight);
		addLine(_viewportX + _viewportW,						_viewportY,		_viewportX + _viewportW,						_viewportY + _dataRowsMaxHeight);
	}
#endif

	for(int row=_currentViewportRowMin; row<_currentViewportRowMax; row++)
	{
		if(createRowNumber(row))
		{

#ifdef ADD_LINES_PLEASE
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

#ifdef ADD_LINES_PLEASE
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

#ifdef ADD_LINES_PLEASE
	_linesWasChanged = true;
#endif

	createleftTopCorner();
	updateExtraColumnItem();

	if(editing())
		positionEditItem(_prevEditRow, _prevEditCol);
	else
		destroyEditItem();

	JASPTIMER_STOP(DataSetView::buildNewLinesAndCreateNewItems);
}

QQuickItem * DataSetView::createTextItem(int row, int col)
{
	JASPTIMER_RESUME(DataSetView::createTextItem);

#ifdef DATASETVIEW_DEBUG_CREATION
	Log::log() << "createTextItem(\t"<<row<<",\t"<<col<<") called!\titemStore contains #"<< _textItemStorage.size() << "\n" << std::flush;
#endif

	if((_cellTextItems.count(col) == 0 && _cellTextItems[col].count(row) == 0) || _cellTextItems[col][row] == nullptr)
	{

		if(_itemDelegate == nullptr)
		{
			_itemDelegate = new QQmlComponent(qmlEngine(this));
			_itemDelegate->setData("import QtQuick 2.9\nText { text: itemText; color: itemActive ? 'black' : 'grey'; verticalAlignment: Text.AlignVCenter; }", QUrl());

			emit itemDelegateChanged();
		}

		QQuickItem			* textItem	= nullptr;
		ItemContextualized	* itemCon	= nullptr;

		bool active = _model->filtered(row, col);

		if(_textItemStorage.size() > 0)
		{
			JASPTIMER_RESUME(DataSetView::createTextItem textItemStorage has something);
#ifdef DATASETVIEW_DEBUG_CREATION
			Log::log() << "createTextItem("<<row<<", "<<col<<") from storage!\n" << std::flush;
#endif
			itemCon = _textItemStorage.top();
			textItem = itemCon->item;
			_textItemStorage.pop();
			setStyleDataItem(itemCon->context, active, col, row);
			JASPTIMER_STOP(DataSetView::createTextItem textItemStorage has something);
		}
		else
		{
			JASPTIMER_RESUME(DataSetView::createTextItem textItemStorage has NOTHING);
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

			JASPTIMER_STOP(DataSetView::createTextItem textItemStorage has NOTHING);
		}

		JASPTIMER_RESUME(DataSetView::createTextItem setValues);

		setTextItemInfo(row, col, textItem);

		_cellTextItems[col][row] = itemCon;

		JASPTIMER_STOP(DataSetView::createTextItem setValues);
	}

	JASPTIMER_STOP(DataSetView::createTextItem);

	return _cellTextItems[col][row]->item;
}

void DataSetView::setTextItemInfo(int row, int col, QQuickItem * textItem)
{
	JASPTIMER_SCOPE(DataSetView::setTextItemInfo);
	
	double	desiredX = _colXPositions.size()	> col ? _colXPositions[col]		: 0,
			desiredW = _dataColsMaxWidth.size() > col ? _dataColsMaxWidth[col]	: 0;

	textItem->setHeight(_dataRowsMaxHeight		- (2 * _itemVerticalPadding));
	textItem->setWidth(desiredW					- (2 * _itemHorizontalPadding));

	textItem->setX(desiredX							+ _itemHorizontalPadding);
	textItem->setY(((row + 1) * _dataRowsMaxHeight)	+ _itemVerticalPadding);

	textItem->setZ(-4);
	textItem->setVisible(true);
}

void DataSetView::storeTextItem(int row, int col, bool cleanUp)
{
	if((_cellTextItems.count(col) == 0 && _cellTextItems[col].count(row) == 0) || _cellTextItems[col][row] == nullptr) return;

#ifdef DATASETVIEW_DEBUG_CREATION
	Log::log() << "storeTextItem("<<row<<", "<<col<<") in storage!\n" << std::flush;
#endif

	JASPTIMER_RESUME(DataSetView::storeTextItem);

	ItemContextualized * textItem = _cellTextItems[col][row];
	_cellTextItems[col][row] = nullptr;

	if(cleanUp)
	{
		_cellTextItems[col].erase(row);

		if(_cellTextItems[col].size() == 0)
			_cellTextItems.erase(col);
	}

	textItem->item->setFocus(	false);
	textItem->item->setVisible(	false);

	if (_cacheItems)		_textItemStorage.push(textItem);
	else					delete textItem;

	JASPTIMER_STOP(DataSetView::storeTextItem);
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
			"Text		{ text: rowNumber; anchors.centerIn: parent; color: jaspTheme.textEnabled; }\n"
		"}", QUrl());

		emit rowNumberDelegateChanged();
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

		rowNumber->setHeight(_dataRowsMaxHeight		- 1);
		rowNumber->setWidth(_rowNumberMaxWidth		- 1);

		rowNumber->setVisible(true);

		_rowNumberItems[row] = itemCon;
	}
	else
		rowNumber = _rowNumberItems[row]->item;

	rowNumber->setX(0.5 + _viewportX);
	rowNumber->setY(0.5 + _dataRowsMaxHeight * (1 + row));
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

	if (_cacheItems)		_rowNumberStorage.push(rowNumber);
	else					delete rowNumber;
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

		emit columnHeaderDelegateChanged();
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
									_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("columnIsComputed")	).toBool(),
									_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("computedColumnIsInvalidated")).toBool(),
									_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("filter")				).toBool(),
									_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("computedColumnError")	).toString(),
									_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("columnType")			).toInt(),
									_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("computedColumnType"	)	).toInt());
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
												_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("columnIsComputed")	).toBool(),
												_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("computedColumnIsInvalidated")).toBool(),
												_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("filter")				).toBool(),
												_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("computedColumnError")	).toString(),
												_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("columnType")			).toInt(),
												_model->headerData(col, Qt::Orientation::Horizontal, _model->getRole("computedColumnType")	).toInt()));

			_columnHeaderDelegate->create(localIncubator, itemCon->context);
			columnHeader = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = columnHeader;

			columnHeader->setParent(this);
			columnHeader->setParentItem(this);
		}


		columnHeader->setHeight(_dataRowsMaxHeight    - 1);
		columnHeader->setWidth(_dataColsMaxWidth[col] - 1);

		columnHeader->setVisible(true);

		_columnHeaderItems[col] = itemCon;
	}
	else
		columnHeader = _columnHeaderItems[col]->item;

	columnHeader->setX(0.5 + _colXPositions[col]);
	columnHeader->setY(0.5 + _viewportY);
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

	if (_cacheItems)		_columnHeaderStorage.push(columnHeader);
	else					delete columnHeader;
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



		_leftTopItem->setVisible(true);
	}

	_leftTopItem->setHeight(_dataRowsMaxHeight - 1);
	_leftTopItem->setWidth(_rowNumberMaxWidth  - 1);
	_leftTopItem->setX(_viewportX + 0.5);
	_leftTopItem->setY(_viewportY + 0.5);
	_leftTopItem->setZ(-1);

	return _leftTopItem;
}

void DataSetView::updateExtraColumnItem()
{
	//Log::log() << "createleftTopCorner() called!\n" << std::flush;
	if(!_extraColumnItem || expandDataSet())
		return;

	_extraColumnItem->setHeight(_dataRowsMaxHeight - 1);
	_extraColumnItem->setX(_viewportX + _viewportW - extraColumnWidth());
	_extraColumnItem->setY(0.5 + _viewportY);

	connect(_extraColumnItem, &QQuickItem::widthChanged, this, &DataSetView::setExtraColumnX, Qt::UniqueConnection);
}

void DataSetView::destroyEditItem(bool createItem)
{
	if(!_editItemContextual)
	{
		_prevEditRow = -1;
		_prevEditCol = -1;
		return;
	}

	Log::log() << "Destroying old edit item (row=" << _prevEditRow << ", col=" << _prevEditCol << ") and context" << std::endl;

	_editItemContextual->item		->setVisible(false);
	_editItemContextual->item		->deleteLater();
	_editItemContextual->item		= nullptr;

	_editItemContextual->context	->deleteLater();
	_editItemContextual->context	= nullptr;

	delete _editItemContextual;
	_editItemContextual				= nullptr;

	if(createItem && !(_prevEditRow == -1 || _prevEditCol == -1))
	{
		Log::log() << "Restoring text item for old edit item at " << _prevEditRow << ", " << _prevEditCol << std::endl;
		QQuickItem * item = createTextItem(_prevEditRow, _prevEditCol);

		size_t col=_prevEditCol, row=_prevEditRow;

		//A delay might help the focus problem? No it doesnt...
		//QTimer::singleShot(10, _cellTextItems[col][row]->item, [col, row, this](){ if (_cellTextItems.contains(col) && _cellTextItems[col].contains(row) && _cellTextItems[col][row]->item) _cellTextItems[col][row]->item->forceActiveFocus(); });

		//Log::log() << "Restored text item has _storedDisplayText[" << _prevEditRow << "][" << _prevEditCol << "]: '" << _storedDisplayText[_prevEditRow][_prevEditCol] << "'" << std::endl;
	}
	else
		Log::log() << "Not creating text item" << std::endl;


	_prevEditRow = -1;
	_prevEditCol = -1;
}

void DataSetView::positionEditItem(int row, int col)
{
	if (row == -1 || col == -1)
		return;

	if(!_editDelegate)
	{
		_editDelegate = new QQmlComponent(qmlEngine(this));

		_editDelegate->setData(
"import QtQuick 2.9"																					"\n"
"TextInput { text: itemText; color: itemActive ? 'black' : 'grey'; verticalAlignment: Text.AlignVCenter; \n"
" onEditingFinished:					 dataview.commitEdit(rowIndex, columnIndex, text); "									"\n"
"}", QUrl());

		emit editDelegateChanged(_editDelegate);
	}

	bool			active		= _model->filtered(row, col);

	if(_editItemContextual && !(_prevEditRow == row && _prevEditCol == col)) //remove previous edit item to avoid old values or broken bindings messing everything up. But only if it is in a different place than where we're at
		destroyEditItem();


	if(!_editItemContextual)
	{
		_editItemContextual = new ItemContextualized(setStyleDataItem(nullptr, active, col, row, false));
		//Log::log() << "Edit item has          _storedDisplayText[" << row << "][" << col << "]: '" << _storedDisplayText[row][col] << "'" << std::endl;

		//forceActiveFocus();

		Log::log() << "Destroying old text item (row=" << row << ", col=" << col << ") and creating edit item + context" << std::endl;
		storeTextItem(row, col, true);
		_prevEditRow = row; //Store info to recreate it later
		_prevEditCol = col;
		
		emit editCoordinatesChanged();

		QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
		_editDelegate->create(localIncubator, _editItemContextual->context);

		if(localIncubator.isError())
			throw std::runtime_error("Something went wrong incubating an edit item delegate for tableview!");

		_editItemContextual->item = qobject_cast<QQuickItem*>(localIncubator.object());
		_editItemContextual->item->setParent(this);
		_editItemContextual->item->setParentItem(this);
	}
	else
	{
		//Log::log() << "repositioning current edit item (row=" << row << ", col=" << col << ")" << std::endl;
		setStyleDataItem(_editItemContextual->context, active, col, row, false);
		//Log::log() << "Edit item has          _storedDisplayText[" << row << "][" << col << "]: '" << _storedDisplayText[row][col] << "'" << std::endl;
	}

	setTextItemInfo(row, col, _editItemContextual->item); //Will set it visible
	//_editItemContextual->item->setFocus(true);
}

void DataSetView::setExtraColumnX()
{
	_extraColumnItem->setX(_viewportX + _viewportW - extraColumnWidth());
}


bool DataSetView::isSelected(int row, int col)
{
	return _selectionModel->isSelected(_model->index(row, col));
}


bool DataSetView::relaxForSelectScroll()
{
	JASPTIMER_SCOPE(DataSetView::relaxForSelectScroll);
	long curMs = Utils::currentMillis();

	//Log::log() << "_selectScrollMs = " << _selectScrollMs << ", curMs = " << curMs << std::endl;

	if(curMs < _selectScrollMs)
		return false;

	_selectScrollMs = curMs + 100;
	return true;
}

bool DataSetView::isVirtual(const QPoint& point)
{
	return point.x() >= _model->columnCount(false) || point.y() >= _model->rowCount(false);
}


void DataSetView::pollSelectScroll(int row, int col)
{
	if(!relaxForSelectScroll())
		return;


	const int b = 3;

	//Log::log() << "DataSetView::pollSelectScroll row=" << mouseIndex.row() << "col=" << mouseIndex.column() << std::endl;


	int		minMouseCol = col == -1 ? _currentViewportColMin :	col - b,
			maxMouseCol = col == -1 ? _currentViewportColMin :	col + b,
			minMouseRow = row == -1 ? _currentViewportRowMin :	row - b,
			maxMouseRow = row == -1 ? _currentViewportRowMin :	row + b;

	bool	maybeBudgeLeft	= minMouseCol <= 	_currentViewportColMin + _viewportMargin,
			maybeBudgeRight	= maxMouseCol >		_currentViewportColMax - _viewportMargin,
			maybeBudgeUp	= minMouseRow < 	_currentViewportRowMin + _viewportMargin,
			maybeBudgeDown	= maxMouseRow >=	_currentViewportRowMax - _viewportMargin;

			maybeBudgeLeft	= maybeBudgeLeft	&& (_currentViewportColMin > 0						|| 0						<	col);
			maybeBudgeUp	= maybeBudgeUp		&& (_currentViewportRowMin > 0						|| 0						<	row);
			maybeBudgeRight	= maybeBudgeRight	&& (_currentViewportColMax < _model->columnCount()	|| _model->columnCount()	>	col);
			maybeBudgeDown	= maybeBudgeDown	&& (_currentViewportRowMax < _model->rowCount()		|| _model->rowCount()		>	row);

	if(maybeBudgeLeft)	emit selectionBudgesLeft	();
	if(maybeBudgeUp)	emit selectionBudgesUp		();
	if(maybeBudgeRight) emit selectionBudgesRight	();
	if(maybeBudgeDown)	emit selectionBudgesDown	();
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
	bool	allSelected = minIdx.x() <= 0 && minIdx.y() <= 0 && maxIdx.x() + 1 >= _model->columnCount(false) && maxIdx.y() + 1 >= _model->rowCount(false);
		
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
					QVariant valVar = _model->data(r, c);
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
				_copiedColumns.push_back(_model->serializedColumn(c));
				headerRow.push_back(_model->headerData(c, Qt::Horizontal).toString());
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
				vals.push_back(_model->data(r, c, int(DataSetPackage::specialRoles::value)).toString());
				labs.push_back(_model->data(r, c, int(DataSetPackage::specialRoles::label)).toString());
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
			_model->removeColumns(	minIdx.x(), 1+(maxIdx.x() - minIdx.x()));
			_model->removeRows(		minIdx.y(), rowsSelected);
			_model->insertColumns(0);
			_model->insertRows(0);
			UndoStack::singleton()->endMacro();//new PasteSpreadsheetCommand(_model->sourceModel(), 0, 0, {{""}}, {{""}}, {}, {}));
		}
		else if(isColumnHeader(where))
			_model->removeColumns(minIdx.x(), 1 + (maxIdx.x() - minIdx.x()));
		else if(isRowHeader(where))
			_model->removeRows(minIdx.y(), rowsSelected);
		else
		{
			Log::log() << "DataSetView about to clear at row: " << minIdx.y() << " and col: " << minIdx.x() << std::endl;
			auto emptyCells = 	std::vector<std::vector<QString>>(
											 1+(maxIdx.x() - minIdx.x()),
											 std::vector<QString>(
												 rowsSelected,
												 ""
											 )
										 );
			
			_model->pasteSpreadsheet(minIdx.y(), minIdx.x(), emptyCells, emptyCells, {}, localSelected);
		}
	}
}

bool DataSetView::clipBoardPasteIsCells() const
{
	QString clipboardStr = QGuiApplication::clipboard()->text();

	if (_lastJaspCopyIntoClipboard != clipboardStr)
		return clipboardStr.contains("\n");
	
	return !(_lastJaspCopyValues.size() == 1 && _lastJaspCopyValues[0].size() == 1);
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
		_model->copyColumns(where.x(), _copiedColumns);
	else if(_lastJaspCopyValues.size()) //internal data copy:
	{
		if(!isColumnHeader(where))
			_model->pasteSpreadsheet(where.y(), where.x(), _lastJaspCopyValues, _lastJaspCopyLabels, {}, _lastJaspCopySelect);
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
			
			_model->pasteSpreadsheet(0, where.x(), vals, labs, colNames, sels);
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

		_model->pasteSpreadsheet(isColumnHeader(where) ? 0 : where.y(), where.x(), newData, {}, colNames);
	}
}

void DataSetView::selectAll()
{
	JASPTIMER_SCOPE(DataSetView::selectAll);
	clearEdit();
	_selectionModel->select(QItemSelection(_model->index(0, 0), _model->index(_model->rowCount(false)-1, _model->columnCount(false)-1)), QItemSelectionModel::Current | QItemSelectionModel::ClearAndSelect);
}

void DataSetView::select(int row, int col, bool shiftPressed, bool ctrlCmdPressed)
{
	JASPTIMER_SCOPE(DataSetView::select);
	
	bool	wholeRow	= col < 0,
			wholeCol	= row < 0;
	
	if(wholeRow && wholeCol)
	{
		selectAll();
		return;
	}
	
					row = std::max(0, row);
					col = std::max(0, col);

	QModelIndex		tl	= _model->index(row,					col),
					br	= _model->index(row,					col);

	if(wholeRow)	br	= _model->index(row,						_model->columnCount(false)-1);
	if(wholeCol)	br	= _model->index(_model->rowCount(false)-1,	col);
	
	QItemSelection	selection(tl, br);
	
	QItemSelectionModel::SelectionFlags flags = QItemSelectionModel::Current | QItemSelectionModel::ClearAndSelect; //Current means we will update the stored selection, so we commit here
	
	if(ctrlCmdPressed)
	{
		QItemSelection old = _selectionModel->selection();
		if(!wholeRow && !wholeCol)	old.merge(selection, QItemSelectionModel::Toggle);
		else if(wholeRow) //If a whole row is "toggled" we first make it all selected if it isnt yet fully, otherwise we deselect
		{
			if(_selectionModel->isRowSelected(row))		old.merge(selection, QItemSelectionModel::Deselect);
			else										old.merge(selection, QItemSelectionModel::Select);
		}
		else if(wholeCol) //We do the same for the column
		{
			if(_selectionModel->isColumnSelected(col))		old.merge(selection, QItemSelectionModel::Deselect);
			else											old.merge(selection, QItemSelectionModel::Select);
		}
			
		selection = old;
	}
	else if(shiftPressed)
	{
		QPoint	minNewSelection = minQModelIndex(selection),
				maxNewSelection = maxQModelIndex(selection),
				minOldSelection = selectionMin(),
				maxOldSelection = selectionMax();
		
		if(minOldSelection != QPoint{-1, -1} && minNewSelection != QPoint{-1, -1})
			selection = QItemSelection(	_model->index(std::min(minNewSelection.y(), minOldSelection.y()), std::min(minNewSelection.x(), minOldSelection.x())), 
										_model->index(std::max(maxNewSelection.y(), maxOldSelection.y()), std::max(maxNewSelection.x(), maxOldSelection.x())));
	}
	
	//Reset edit if we are selecting things with shift or ctrl/cmd, or when the last clicked place is not the same as the editthing
	if(shiftPressed || ctrlCmdPressed || row != _prevEditRow || col != _prevEditCol)
		clearEdit();
	
	if(!wholeCol && !shiftPressed && !ctrlCmdPressed && ( row != _prevEditRow || col != _prevEditCol) )
		edit(row, col);

	
/* //Even if you reenable this we prob dont want this in release par accident
#ifdef JASP_DEBUG
	{	
		QStringList l;
		for(QModelIndex i : selection.indexes())
			l << QString("(r=%1, c=%2)").arg(i.row()).arg(i.column());
		
		Log::log()	<< "DataSetView::select(row=" << row << ", col=" << col << ", shift=" << shiftPressed << ", control="<< ctrlCmdPressed << "):\n"
					<< QString("-\tModel size: r=%1, c=%2\n").arg(_model->columnCount(false)).arg(_model->rowCount(false))
					<< QString("-\ttl = (r=%1, c=%2)\n").arg(tl.row()).arg(tl.column()) 
					<< QString("-\tbr = (r=%1, c=%2)\n").arg(br.row()).arg(br.column()) 
					<< "-\tset = " << l.join(", ") << std::endl;
	}
#endif*/
	
	_selectionModel->select(selection, flags);
}

void DataSetView::selectionClear()
{
	JASPTIMER_SCOPE(DataSetView::selectClear);
	_selectionModel->clear();
}

void DataSetView::selectHover(int row, int col)
{
	JASPTIMER_SCOPE(DataSetView::selectHover);
	pollSelectScroll(row, col);
}

QPoint DataSetView::selectionMin() const	
{ 
	JASPTIMER_SCOPE(DataSetView::selectionMin);
	return minQModelIndex(_selectionModel->selection());	
}

QPoint DataSetView::selectionMax() const	
{ 
	JASPTIMER_SCOPE(DataSetView::selectionMax);
	return maxQModelIndex(_selectionModel->selection());
}

void DataSetView::columnIndexSelectedApply(int columnIndex, std::function<void(intset columnIndexes)> applyThis)
{
	int		columnA			= selectionMin().x(),
			columnB			= selectionMax().x();
	bool	outSelection	= columnA == -1 || columnB == -1 || !(columnIndex >= columnA && columnIndex <= columnB);
	
	if(outSelection)
		columnA = columnB = columnIndex;
	
	intset ints;
	for	(columnIndex	= columnA; columnIndex <= columnB; columnIndex++)
		if(outSelection || _selectionModel->columnIntersectsSelection(columnIndex))
			ints.insert(columnIndex);
	
	applyThis(ints);
}

void DataSetView::columnIndexSelectedApply(int columnIndex, std::function<void(int columnIndex)> applyThis)
{
	columnIndexSelectedApply(columnIndex, [&](intset ints)
	{
		for	(int anInt : ints)
			applyThis(anInt);
	});
	
}

void DataSetView::setColumnType(int columnIndex, int newColumnType)
{ 
	columnIndexSelectedApply(columnIndex, [&](intset col) { _model->setColumnType(col, newColumnType);  });
}

void DataSetView::resizeData(int rows, int columns)
{
	// Argument row and column of the resize method are indices
	_model->resize(rows - 1, columns - 1, false, tr("Resize data to %1 rows and %2 columns").arg(rows).arg(columns));
}

void DataSetView::currentSelectedColumnHandler(const QModelIndex &current, const QModelIndex &previous)
{
	int selectedColumn = current.column();
	if (selectedColumn >= 0)
		emit DataSetPackage::pkg()->chooseColumn(selectedColumn);
}

void DataSetView::columnReverseValues(int columnIndex)
{
	columnIndexSelectedApply(columnIndex, [&](intset col) { _model->columnReverseValues(col);  });
}

void DataSetView::columnautoSortByValues(int columnIndex)
{
	columnIndexSelectedApply(columnIndex, [&](intset col) { _model->columnautoSortByValues(col);  });
}

QString DataSetView::columnInsertBefore(int col, bool computed, bool R)
{
	destroyEditItem(false);

	if(col == -1)
		col = selectionMin().x() != -1 ? selectionMin().x() : 0;

	_model->insertColumn(col, computed, R);
	return _model->headerData(col, Qt::Horizontal).toString();
}

QString DataSetView::columnInsertAfter(int col, bool computed, bool R)
{
	if(col == -1)
		col = selectionMax().x() != -1 ? selectionMax().x() : _model->columnCount(false) - 1;	

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
	if(_model->columnCount(false) <= 1 || (selectionMin().x() == -1 && col == -1))
		return;
	
	int columnA	= selectionMin().x(),
		columnB	= selectionMax().x();
	
	destroyEditItem(false);

	if(columnA > columnB)
		std::swap(columnA, columnB);
	
	if(col == -1)
		col = columnA;
	
	if(col == -1)
		col = _model->columnCount(false) - 1;

	if(columnA == -1 || columnA > col || columnB < col || (columnA == col && columnB == col))
		_model->removeColumns(col, 1);
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
		_model->removeColumnGroups(removals);
	}

	_selectionModel->clear();
}


void DataSetView::rowInsertBefore(int row)
{
	destroyEditItem(false);

	if(row == -1)
		row = selectionMin().y() != -1 ? selectionMin().y() : 0;

	_model->insertRows(row);
}

void DataSetView::rowInsertAfter(int row)
{
	if(row == -1)
		row = selectionMax().y() != -1 ? selectionMax().y() : _model->rowCount(false) - 1;

	rowInsertBefore(row + 1);
}

void DataSetView::rowsDeleteSelected()
{
	rowsDelete(-1);
}

void DataSetView::rowsDelete(int row)
{
	if(_model->rowCount(false) <= 1 || (selectionMin().y() == -1 && row == -1))
		return;

	int rowA	= selectionMin().y(),
		rowB	= selectionMax().y();
	
	destroyEditItem();	

	if(rowA > rowB)
		std::swap(rowA, rowB);
	
	if(row == -1)
		row = rowA;
	
	if(row == -1)
		row = _model->rowCount(false) - 1;
	
	if(rowA == -1 || rowA > row || rowB < row || (rowA == row && rowB == row))
		_model->removeRows(row, 1);
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
		_model->removeRowGroups(removals);
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

	_model->pasteSpreadsheet(row0, col0, cells, cells);
}
;
void DataSetView::columnsAboutToBeInserted(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelAboutToBeReset();
}

void DataSetView::columnsAboutToBeRemoved(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelAboutToBeReset();
}

void DataSetView::rowsAboutToBeInserted(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelAboutToBeReset();
}

void DataSetView::rowsAboutToBeRemoved(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelAboutToBeReset();
}

void DataSetView::columnsInserted(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelWasReset();
}

void DataSetView::columnsRemoved(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelWasReset();
}

void DataSetView::rowsInserted(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelWasReset();
}

void DataSetView::rowsRemoved(const QModelIndex & parent, int first, int last)
{
	//temp:
	modelWasReset();
}

void DataSetView::setEditDelegate(QQmlComponent *editDelegate)
{
	if (_editDelegate == editDelegate)
		return;

	_editDelegate = editDelegate;
	emit editDelegateChanged(_editDelegate);
}



void DataSetView::setEditing(bool editing)
{
	if (_editing == editing)
		return;

	_editing = editing;

	//Log::log() << "DataSetView::setShiftSelectActive( " << (shiftSelectActive? "active!" : "inactive!") << " )" << std::endl;

	emit editingChanged(_editing);
}

void DataSetView::clearEdit()
{
	if(editing())
	{
		commitLastEdit();
		destroyEditItem();
		setEditing(false);
	}
}


void DataSetView::edit(int row, int col)
{
	if (row == -1 || col == -1)
		return;

	_selectionModel->setCurrentIndex(_model->index(row, col), QItemSelectionModel::Current);

	clearEdit();
	
	setEditing(true);
	positionEditItem(row, col);
}

void DataSetView::commitEdit(int row, int col, QVariant editedValue)
{
	if(!editing())
	{
		Log::log() << "commitEdit called while not editing..." << std::endl;
	}
	else
	{
		QVariant oldValue = _model->data(row, col);

		Log::log() << "editing finished! old value: '" << oldValue.toString() << "'  and new value: '" << editedValue.toString() << "' (row=" << row << ", col=" << col << ")" << std::endl;

		if(oldValue.toString() != editedValue.toString())
			_model->setData(row, col, editedValue, 0);
	}
}

void DataSetView::onDataModeChanged(bool dataMode)
{
	if(!dataMode && editing())
	{
		destroyEditItem();
		setEditing(false);
	}
}

void DataSetView::commitLastEdit()
{
	Log::log() << "Commit last edit" << std::endl;
	if(_prevEditRow != -1 && _prevEditCol != -1 && _editItemContextual && _editItemContextual->item)
		commitEdit(_prevEditRow, _prevEditCol, _editItemContextual->item->property("text"));
}

QQmlContext * DataSetView::setStyleDataItem(QQmlContext * previousContext, bool active, size_t col, size_t row, bool emptyValLabel)
{
	JASPTIMER_SCOPE(DataSetView::setStyleDataItem);


	bool isEditable(_model->flags(row, col) & Qt::ItemIsEditable);

	if(isEditable || _storedDisplayText.count(row) == 0 || _storedDisplayText[row].count(col) == 0)
		_storedDisplayText[row][col] = _model->data(row, col, Qt::DisplayRole).toString();

	QString text = _storedDisplayText[row][col];

	if(isEditable && text == tq(EmptyValues::displayString()) && !emptyValLabel)
		text = "";

	if(previousContext == nullptr)
		previousContext = new QQmlContext(qmlContext(this), this);
	

	//The first four also get updated in DataSetView::modelDataChanged!
	//If adding or changes behaviour also do that there
	previousContext->setContextProperty("itemText",			text);
	previousContext->setContextProperty("itemShadowText",	_model->data(row, col, _model->getRole("shadowDisplay")));
	previousContext->setContextProperty("itemLabel",		_model->data(row, col, _model->getRole("label")));
	previousContext->setContextProperty("itemValue",		_model->data(row, col, _model->getRole("value")));
	previousContext->setContextProperty("itemActive",		active);
	previousContext->setContextProperty("itemEditable",		isEditable);
	previousContext->setContextProperty("itemSelected",		_model->data(row, col, _model->getRole("selected")));
	previousContext->setContextProperty("itemFiltered",		_model->filtered(row, col));
	previousContext->setContextProperty("itemInputType",	_model->data(row, col, _model->getRole("itemInputValue")));
	previousContext->setContextProperty("columnIndex",		static_cast<int>(col));
	previousContext->setContextProperty("rowIndex",			static_cast<int>(row));
	//previousContext->setContextProperty("index",			idx);
	previousContext->setContextProperty("isDynamic",		true);
	previousContext->setContextProperty("tableView",		_tableViewItem);
	previousContext->setContextProperty("dataviewer",		this);

	return previousContext;
}

QQmlContext * DataSetView::setStyleDataRowNumber(QQmlContext * previousContext, QString text, int row)
{
	if(previousContext == nullptr)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("rowIndex",			row);
	previousContext->setContextProperty("rowNumber",		_model->headerData(row, Qt::Vertical, Qt::DisplayRole)); //gives original row, could be different from rowIndex if filtered-out-values arent visible
	previousContext->setContextProperty("virtual",			_model->isRowVirtual(row));
	previousContext->setContextProperty("headerText",		text);

	return previousContext;
}

QQmlContext * DataSetView::setStyleDataColumnHeader(QQmlContext * previousContext, QString text, int column, bool isComputed, bool isInvalidated, bool isFiltered, QString computedError, int columnType, int computedColumnType)
{
	if(previousContext == nullptr)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("headerText",			text);
	previousContext->setContextProperty("columnIndex",			column);
	previousContext->setContextProperty("columnTitle",			_model->headerData(column, Qt::Horizontal, _model->getRole("title")));
	previousContext->setContextProperty("columnDescription",	_model->headerData(column, Qt::Horizontal, _model->getRole("description")));
	previousContext->setContextProperty("columnIsComputed",		isComputed);
	previousContext->setContextProperty("columnIsInvalidated",	isInvalidated);
	previousContext->setContextProperty("computedColumnType",	computedColumnType);
	previousContext->setContextProperty("columnIsFiltered",		isFiltered);
	previousContext->setContextProperty("columnError",			computedError);
	previousContext->setContextProperty("columnType",			columnType);
	previousContext->setContextProperty("virtual",				_model->isColumnVirtual(column));

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
	if (expandDataSet())
		newItem = nullptr;

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

void DataSetView::setCacheItems(bool cacheItems)
{
	if(cacheItems == _cacheItems)
		return;

	_cacheItems = cacheItems;
		
	emit cacheItemsChanged();

	calculateCellSizesAndClear(!_cacheItems);
}

void DataSetView::setExpandDataSet(bool expand)
{
	if (!_model || expand == expandDataSet())
		return;

	_model->setExpandDataSet(expand);
	emit expandDataSetChanged();
}

void DataSetView::reloadTextItems()
{
	//Store all current items
	for(int col=_previousViewportColMin; col< _previousViewportColMax; col++)
		for(int row=_previousViewportRowMin; row<_previousViewportRowMax; row++)
			storeTextItem(row, col);

	viewportChangedDelayed(); //rerun to get new items
}

void DataSetView::reloadRowNumbers()
{
	//Store all current items
	for(int row=_previousViewportRowMin; row<_previousViewportRowMax; row++)
		storeRowNumber(row);

	viewportChangedDelayed(); //rerun to get new items
}

void DataSetView::reloadColumnHeaders()
{
	//Store all current items
	for(int col=_previousViewportColMin; col< _previousViewportColMax; col++)
		storeColumnHeader(col);

	viewportChangedDelayed(); //rerun to get new items
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

#ifdef ADD_LINES_PLEASE
QSGNode * DataSetView::updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *)
{
	//JASPTIMER_RESUME(DataSetView::updatePaintNode);
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
			currentNode->setMaterial(&_material);
			currentNode->setRenderOrder(100);

			justAdded = true;

			oldNode->markDirty(QSGNode::DirtyNodeAdded);
		}

		int geomSize = std::min(linesPerNode, (int)(_linesActualSize - lineIndex) / 4); //_lines is floats x, y, x, y so each set of 4 is a single line.
		geomSize *= 2;

		QSGGeometry *geometry = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), geomSize);
		geometry->setLineWidth(1); //ignored anyway
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

	//JASPTIMER_STOP(DataSetView::updatePaintNode);

	return oldNode;
}
#endif

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

QPoint DataSetView::editCoordinates() const
{
	return QPoint(_prevEditCol, _prevEditRow);
}

int DataSetView::maxColWidth() const
{
	return _maxColWidth;
}

void DataSetView::setMaxColWidth(int newMaxColWidth)
{
	if (_maxColWidth == newMaxColWidth)
		return;
	_maxColWidth = newMaxColWidth;
	emit maxColWidthChanged();
}
