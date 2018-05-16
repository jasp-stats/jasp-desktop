#include "datasetview.h"

#include <QSGFlatColorMaterial>
#include <QSGGeometry>
#include <QSGNode>
#include <queue>



DataSetView::DataSetView() : _metricsFont(_font)
{
	setFlag(QQuickItem::ItemHasContents, true);

	material.setColor(Qt::gray);

	connect(this, &DataSetView::parentChanged, this, &DataSetView::myParentChanged);

	connect(this, &DataSetView::viewportXChanged, this, &DataSetView::viewportChanged);
	connect(this, &DataSetView::viewportYChanged, this, &DataSetView::viewportChanged);
	connect(this, &DataSetView::viewportWChanged, this, &DataSetView::viewportChanged);
	connect(this, &DataSetView::viewportHChanged, this, &DataSetView::viewportChanged);

	connect(this, &DataSetView::itemDelegateChanged, this, &DataSetView::reloadTextItems);
	connect(this, &DataSetView::rowNumberDelegateChanged, this, &DataSetView::reloadRowNumbers);
	connect(this, &DataSetView::columnHeaderDelegateChanged, this, &DataSetView::reloadColumnHeaders);

	connect(this, &DataSetView::itemHorizontalPaddingChanged, this, &DataSetView::calculateCellSizes);
	connect(this, &DataSetView::itemVerticalPaddingChanged, this, &DataSetView::calculateCellSizes);

	connect(this, &DataSetView::itemSizeChanged, this, &DataSetView::reloadTextItems);
	connect(this, &DataSetView::itemSizeChanged, this, &DataSetView::reloadRowNumbers);
	connect(this, &DataSetView::itemSizeChanged, this, &DataSetView::reloadColumnHeaders);

	setZ(10);
}

DataSetView::~DataSetView()
{
	//everything is cleaned up through QObject tree I think.
}

void DataSetView::setModel(QAbstractTableModel * model)
{
	if(_model != model)
	{
		_model = model;

		connect(_model, &QAbstractTableModel::dataChanged,			this, &DataSetView::modelDataChanged);
		connect(_model, &QAbstractTableModel::headerDataChanged,	this, &DataSetView::modelHeaderDataChanged);
		connect(_model, &QAbstractTableModel::modelAboutToBeReset,	this, &DataSetView::modelAboutToBeReset);
		connect(_model, &QAbstractTableModel::modelReset,			this, &DataSetView::modelWasReset);

		setRolenames();

		QSizeF calcedSizeRowNumber = _metricsFont.size(Qt::TextSingleLine, QString::fromStdString(std::to_string(_model->rowCount())));
		_rowNumberMaxWidth = calcedSizeRowNumber.width() + 30;

		//recalculateCellSizes = true;
		calculateCellSizes();
		update();

		emit modelChanged();
	}
}

void DataSetView::setRolenames()
{
	_roleNameToRole.clear();

	if(_model == NULL) return;

	auto roleNames = _model->roleNames();

	for(auto rn : roleNames.keys())
		_roleNameToRole[roleNames[rn].toStdString()] = rn;

}

void DataSetView::calculateCellSizes()
{


	_cellSizes.clear();
	_dataColsMaxWidth.clear();

	for(auto col : _cellTextItems)
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

	if(_model == NULL) return;

	_cellSizes.resize(_model->columnCount());
	_colXPositions.resize(_model->columnCount());
	_cellTextItems.clear();

	for(int col=0; col<_model->columnCount(); col++)
	{
		QString text = _model->headerData(col, Qt::Orientation::Horizontal, _roleNameToRole["maxColString"]).toString();
		QSizeF calcedSize = _metricsFont.size(Qt::TextSingleLine, text);

		_cellSizes[col] = calcedSize;
	}

	_dataColsMaxWidth.resize(_model->columnCount());

	for(int col=0; col<_model->columnCount(); col++)
		_dataColsMaxWidth[col] = _cellSizes[col].width() + _itemHorizontalPadding * 2;

	_dataRowsMaxHeight = _model->columnCount() == 0 ? 0 : _cellSizes[0].height() + _itemVerticalPadding * 2;

	float w=_rowNumberMaxWidth;
	for(int col=0; col<_model->columnCount(); col++)
		w += _dataColsMaxWidth[col];


	float x = _rowNumberMaxWidth;

	for(int col=0; col<_model->columnCount(); col++)
	{
		_colXPositions[col] = x;
		x += _dataColsMaxWidth[col];
	}


	setWidth(w);
	setHeight( _dataRowsMaxHeight * (_model->rowCount() + 1));
	_recalculateCellSizes = false;

	emit itemSizeChanged();
}


void DataSetView::viewportChanged()
{
	if(_model == NULL || _viewportX != _viewportX || _viewportY != _viewportY || _viewportW != _viewportW || _viewportH != _viewportH ) //only possible if they are NaN
		return;

#ifdef DEBUG_VIEWPORT
	std::cout << "viewportChanged!\n" <<std::flush;
#endif
	QVector2D leftTop(_viewportX, _viewportY);
	QVector2D viewSize(_viewportW, _viewportH);
	QVector2D rightBottom(leftTop + viewSize);

	int currentViewportColMin = -1, currentViewportColMax = -1, currentViewportRowMin = -1, currentViewportRowMax = -1;

	float cumulative = 0;
	for(int col=0; col<_model->columnCount() && currentViewportColMax == -1; col++)
	{
		if(currentViewportColMax == -1 && cumulative > rightBottom.x())
			currentViewportColMax = col;

		cumulative += _dataColsMaxWidth[col];

		if(currentViewportColMin == -1 && cumulative > leftTop.x())
			currentViewportColMin = col;
	}

	if(currentViewportColMax == -1)
		currentViewportColMax = _model->columnCount();

	currentViewportColMin = std::max(0,						currentViewportColMin - _viewportMargin);
	currentViewportColMax = std::min(_model->columnCount(),	currentViewportColMax + _viewportMargin);

	currentViewportRowMin = std::max(qRound(leftTop.y()		/ _dataRowsMaxHeight) - 1,	0);
	currentViewportRowMax = std::min(qRound(rightBottom.y()	/ _dataRowsMaxHeight) + 1,	_model->rowCount());

	// remove superflouous textItems if they exist (aka store them in stack)
	int maxRows = _model->rowCount(), maxCols = _model->columnCount();
	if(
			_previousViewportRowMin >= 0		&& _previousViewportRowMax >= 0			&& _previousViewportColMin >= 0			&& _previousViewportColMax >= 0 &&
			_previousViewportRowMin < maxRows	&& _previousViewportRowMax < maxRows	&& _previousViewportColMin < maxCols	&& _previousViewportColMax < maxCols
	)
	{
		for(int col=_previousViewportColMin; col<_previousViewportColMax; col++)
		{
			for(int row=_previousViewportRowMin; row < currentViewportRowMin; row++)
				storeTextItem(row, col);

			for(int row=currentViewportRowMax; row < _previousViewportRowMax; row++)
				storeTextItem(row, col);
		}

		for(int row=_previousViewportRowMin; row<_previousViewportRowMax; row++)
		{
			for(int col=_previousViewportColMin; col < currentViewportColMin; col++)
				storeTextItem(row, col);

			for(int col=currentViewportColMax; col < _previousViewportColMax; col++)
				storeTextItem(row, col);
		}

		for(int row=_previousViewportRowMin; row < currentViewportRowMin; row++)
			storeRowNumber(row);

		for(int row=currentViewportRowMax; row < _previousViewportRowMax; row++)
			storeRowNumber(row);
	}

	_lines.clear();

	//and now we should create some new ones!

	for(int col=currentViewportColMin; col<currentViewportColMax; col++)
		for(int row=currentViewportRowMin; row<currentViewportRowMax; row++)
		{
			QVector2D pos0(_colXPositions[col],					_dataRowsMaxHeight + row * _dataRowsMaxHeight);
			QVector2D pos1(pos0.x() + _dataColsMaxWidth[col],	pos0.y()+ _dataRowsMaxHeight);

			int lineFlags = _model->data(_model->index(row, col), _roleNameToRole["lines"]).toInt();

			bool	left	= (lineFlags & 1 > 0)	&& pos0.x()  > _rowNumberMaxWidth + _viewportX,
					right	= (lineFlags & 2 > 0)	&& pos1.x()  > _rowNumberMaxWidth + _viewportX,
					up		= lineFlags & 4 > 0		&& pos0.y()  > _dataRowsMaxHeight + _viewportY,
					down	= lineFlags & 8 > 0		&& pos1.y()  > _dataRowsMaxHeight + _viewportY;

			createTextItem(row, col);


			if(left)	_lines.push_back(std::make_pair(QVector2D(pos0.x(),	pos1.y()),	pos0));
			if(up)		_lines.push_back(std::make_pair(QVector2D(pos1.x(),	pos0.y()),	pos0));
			if(right)	_lines.push_back(std::make_pair(QVector2D(pos1.x(),	pos0.y()),	pos1));
			if(down)	_lines.push_back(std::make_pair(QVector2D(pos0.x(),	pos1.y()),	pos1));

		}

	_lines.push_back(std::make_pair(QVector2D(_viewportX + 0.01f,				_viewportY),						QVector2D(_viewportX + 0.01f,				_viewportY + _viewportH)));
	_lines.push_back(std::make_pair(QVector2D(_viewportX + _rowNumberMaxWidth,	_viewportY),						QVector2D(_viewportX + _rowNumberMaxWidth,	_viewportY + _viewportH)));

	_lines.push_back(std::make_pair(QVector2D(_viewportX,						_viewportY + 0.01f),						QVector2D(_viewportX + _viewportW,			_viewportY+ 0.01f)));
	_lines.push_back(std::make_pair(QVector2D(_viewportX,						_viewportY + _dataRowsMaxHeight),	QVector2D(_viewportX + _viewportW,			_viewportY + _dataRowsMaxHeight)));


	for(int row=currentViewportRowMin; row<currentViewportRowMax; row++)
	{
		createRowNumber(row);

		QVector2D pos0(_viewportX,						(1 + row) * _dataRowsMaxHeight);
		QVector2D pos1(_viewportX + _rowNumberMaxWidth, (2 + row) * _dataRowsMaxHeight);

		if(pos0.y() > _dataRowsMaxHeight + _viewportY)
			_lines.push_back(std::make_pair(QVector2D(pos0.x(), pos0.y()), QVector2D(pos1.x(), pos0.y())));


		if(row == _model->rowCount() - 1 && pos1.y() > _dataRowsMaxHeight + _viewportY)
			_lines.push_back(std::make_pair(QVector2D(pos0.x(), pos1.y()), QVector2D(pos1.x(), pos1.y())));
	}

	for(int col=currentViewportColMin; col<currentViewportColMax; col++)
	{

		createColumnHeader(col);

		QVector2D pos0(_colXPositions[col],					_viewportY);
		QVector2D pos1(pos0.x() + _dataColsMaxWidth[col],	pos0.y() + _dataRowsMaxHeight);

		if(pos0.x()  > _rowNumberMaxWidth + _viewportX)
			_lines.push_back(std::make_pair(QVector2D(pos0.x(), pos0.y()), QVector2D(pos0.x(), pos1.y())));


		if(col == _model->columnCount() - 1 && pos1.x()  > _rowNumberMaxWidth + _viewportX)
			_lines.push_back(std::make_pair(QVector2D(pos1.x(), pos0.y()), QVector2D(pos1.x(), pos1.y())));
	}


	createleftTopCorner();


	update();

#ifdef DEBUG_VIEWPORT
	std::cout << "viewport X: " << _viewportX << " Y: " << _viewportY << " W: " << _viewportW << " H: " << _viewportH <<  std::endl << std::flush;
	std::cout << "_previousViewport ColMin: "<<_previousViewportColMin<<" ColMax: "<<_previousViewportColMax<<" RowMin: "<<_previousViewportRowMin<<" RowMax: "<<_previousViewportRowMax<<"\n";
	std::cout << "currentViewport ColMin: "<<currentViewportColMin<<" ColMax: "<<currentViewportColMax<<" RowMin: "<<currentViewportRowMin<<" RowMax: "<<currentViewportRowMax<<"\n"<<std::flush;
#endif

	_previousViewportColMin = currentViewportColMin;
	_previousViewportColMax = currentViewportColMax;
	_previousViewportRowMin = currentViewportRowMin;
	_previousViewportRowMax = currentViewportRowMax;
}

QQuickItem * DataSetView::createTextItem(int row, int col)
{
	//std::cout << "createTextItem("<<row<<", "<<col<<") called!\n" << std::flush;

	if((_cellTextItems.count(col) == 0 && _cellTextItems[col].count(row) == 0) || _cellTextItems[col][row] == NULL)
	{

		if(_itemDelegate == NULL)
		{
			_itemDelegate = new QQmlComponent(qmlEngine(this));
			_itemDelegate->setData("import QtQuick 2.10\nText { text: itemText; color: itemActive ? 'black' : 'grey' }", QUrl());
		}

		QQuickItem * textItem = NULL;
		ItemContextualized * itemCon = NULL;

		QModelIndex ind(_model->index(row, col));
		bool active = _model->data(ind, _roleNameToRole["active"]).toBool();

		if(_textItemStorage.size() > 0)
		{
#ifdef DEBUG_VIEWPORT
			std::cout << "createTextItem("<<row<<", "<<col<<") from storage!\n" << std::flush;
#endif
			itemCon = _textItemStorage.top();
			textItem = itemCon->item;
			_textItemStorage.pop();
			setStyleDataItem(itemCon->context, _model->data(ind).toString(), active, col, row);
		}
		else
		{
#ifdef DEBUG_VIEWPORT
			std::cout << "createTextItem("<<row<<", "<<col<<") ex nihilo!\n" << std::flush;
#endif
			QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
			itemCon = new ItemContextualized(setStyleDataItem(NULL, _model->data(ind).toString(), active, col, row));
			_itemDelegate->create(localIncubator, itemCon->context);
			textItem = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = textItem;

			textItem->setParent(this);
			textItem->setParentItem(this);
		}

		textItem->setX(_colXPositions[col] + _itemHorizontalPadding);
		textItem->setY(-2 + _dataRowsMaxHeight + _itemVerticalPadding + row * _dataRowsMaxHeight);
		textItem->setZ(-4);
		textItem->setVisible(true);

		_cellTextItems[col][row] = itemCon;
	}

	return _cellTextItems[col][row]->item;
}

void DataSetView::storeTextItem(int row, int col, bool cleanUp)
{
#ifdef DEBUG_VIEWPORT
	std::cout << "storeTextItem("<<row<<", "<<col<<") in storage!\n" << std::flush;
#endif
	if((_cellTextItems.count(col) == 0 && _cellTextItems[col].count(row) == 0) || _cellTextItems[col][row] == NULL) return;


	ItemContextualized * textItem = _cellTextItems[col][row];
	_cellTextItems[col][row] = NULL;

	if(cleanUp)
	{
		_cellTextItems[col].erase(row);

		if(_cellTextItems[col].size() == 0)
			_cellTextItems.erase(col);
	}

	textItem->item->setVisible(false);

	_textItemStorage.push(textItem);
}



QQuickItem * DataSetView::createRowNumber(int row)
{
	//std::cout << "createRowNumber("<<row<<") called!\n" << std::flush;



	if(_rowNumberDelegate == NULL)
	{
		_rowNumberDelegate = new QQmlComponent(qmlEngine(this));
		_rowNumberDelegate->setData("import QtQuick 2.10\nItem {\n"
			"Rectangle	{ color: \"lightGrey\";	anchors.fill: parent }\n"
			"Text		{ text: rowIndex; anchors.centerIn: parent }\n"
		"}", QUrl());
	}

	QQuickItem * rowNumber = NULL;
	ItemContextualized * itemCon = NULL;

	if(_rowNumberItems.count(row) == 0  || _rowNumberItems[row] == NULL)
	{
		if(_rowNumberStorage.size() > 0)
		{
#ifdef DEBUG_VIEWPORT
			std::cout << "createRowNumber("<<row<<") from storage!\n" << std::flush;
#endif
			 itemCon = _rowNumberStorage.top();
			_rowNumberStorage.pop();

			rowNumber = itemCon->item;
			setStyleDataRowNumber(itemCon->context, row + 1);
		}
		else
		{
#ifdef DEBUG_VIEWPORT
			std::cout << "createRowNumber("<<row<<") ex nihilo!\n" << std::flush;
#endif
			QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
			itemCon = new ItemContextualized(setStyleDataRowNumber(NULL, row + 1));

			_rowNumberDelegate->create(localIncubator, itemCon->context);
			rowNumber = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = rowNumber;

			rowNumber->setParent(this);
			rowNumber->setParentItem(this);
		}

		rowNumber->setProperty("z", 10);
		//rowNumber->setProperty("text", QString::fromStdString(std::to_string(row + 1))); //Nobody wants zero-based rows...

		rowNumber->setY(_dataRowsMaxHeight * (1 + row));
		rowNumber->setZ(-3);
		rowNumber->setHeight(_dataRowsMaxHeight);
		rowNumber->setWidth(_rowNumberMaxWidth);

		rowNumber->setVisible(true);

		_rowNumberItems[row] = itemCon;
	}
	else
		rowNumber = _rowNumberItems[row]->item;

	rowNumber->setX(_viewportX);

	return _rowNumberItems[row]->item;
}

void DataSetView::storeRowNumber(int row)
{
#ifdef DEBUG_VIEWPORT
	std::cout << "storeRowNumber("<<row<<") in storage!\n" << std::flush;
#endif

	if(_rowNumberItems.count(row) == 0  || _rowNumberItems[row] == NULL) return;

	ItemContextualized * rowNumber = _rowNumberItems[row];
	_rowNumberItems[row] = NULL;

	_rowNumberItems.erase(row);

	rowNumber->item->setVisible(false);

	_rowNumberStorage.push(rowNumber);
}


QQuickItem * DataSetView::createColumnHeader(int col)
{
	//std::cout << "createColumnHeader("<<col<<") called!\n" << std::flush;

	if(_columnHeaderDelegate == NULL)
	{
		_columnHeaderDelegate = new QQmlComponent(qmlEngine(this));
		_columnHeaderDelegate->setData("import QtQuick 2.10\nItem {\n"
			"Rectangle	{ color: \"lightGrey\";	anchors.fill: parent }\n"
			"Text		{ text: headerText; anchors.centerIn: parent }\n"
		"}", QUrl());
	}


	QQuickItem * columnHeader = NULL;
	ItemContextualized * itemCon = NULL;

	if(_columnHeaderItems.count(col) == 0  || _columnHeaderItems[col] == NULL)
	{
		if(_columnHeaderStorage.size() > 0)
		{
#ifdef DEBUG_VIEWPORT
			std::cout << "createColumnHeader("<<col<<") from storage!\n" << std::flush;
#endif
			itemCon = _columnHeaderStorage.top();
			_columnHeaderStorage.pop();
			columnHeader = itemCon->item;

			setStyleDataColumnHeader(itemCon->context, _model->headerData(col, Qt::Orientation::Horizontal).toString(), col);
		}
		else
		{
#ifdef DEBUG_VIEWPORT
			std::cout << "createColumnHeader("<<col<<") ex nihilo!\n" << std::flush;
#endif
			QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
			itemCon = new ItemContextualized(setStyleDataColumnHeader(NULL, _model->headerData(col, Qt::Orientation::Horizontal).toString(), col));
			_columnHeaderDelegate->create(localIncubator, itemCon->context);
			columnHeader = qobject_cast<QQuickItem*>(localIncubator.object());
			itemCon->item = columnHeader;

			columnHeader->setParent(this);
			columnHeader->setParentItem(this);
		}

		columnHeader->setProperty("z", 10);

		columnHeader->setZ(-3);
		columnHeader->setHeight(_dataRowsMaxHeight);
		columnHeader->setWidth(_dataColsMaxWidth[col]);

		columnHeader->setVisible(true);

		_columnHeaderItems[col] = itemCon;
	}
	else
		columnHeader = _columnHeaderItems[col]->item;

	columnHeader->setX(_colXPositions[col]);
	columnHeader->setY(_viewportY);

	return columnHeader;
}

void DataSetView::storeColumnHeader(int col)
{
#ifdef DEBUG_VIEWPORT
	std::cout << "storeColumnHeader("<<col<<") in storage!\n" << std::flush;
#endif

	if(_columnHeaderItems.count(col) == 0  || _columnHeaderItems[col] == NULL) return;

	ItemContextualized * columnHeader = _columnHeaderItems[col];
	_columnHeaderItems[col] = NULL;

	_columnHeaderItems.erase(col);

	columnHeader->item->setVisible(false);

	_columnHeaderStorage.push(columnHeader);
}

QQuickItem * DataSetView::createleftTopCorner()
{
	//std::cout << "createleftTopCorner() called!\n" << std::flush;
	if(_leftTopItem == NULL)
	{

		if(_leftTopCornerDelegate == NULL)
		{
			_leftTopCornerDelegate = new QQmlComponent(qmlEngine(this));
			_leftTopCornerDelegate->setData("import QtQuick 2.10\nItem {}", QUrl());
		}

		QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
		_leftTopCornerDelegate->create(localIncubator);
		_leftTopItem = qobject_cast<QQuickItem*>(localIncubator.object());

		_leftTopItem->setParent(this);
		_leftTopItem->setParentItem(this);

		_leftTopItem->setProperty("z", 12);

		_leftTopItem->setZ(-1);



		_leftTopItem->setVisible(true);
	}

	_leftTopItem->setHeight(_dataRowsMaxHeight);
	_leftTopItem->setWidth(_rowNumberMaxWidth);
	_leftTopItem->setX(_viewportX);
	_leftTopItem->setY(_viewportY);

	return _leftTopItem;
}

QQuickItem * DataSetView::createStyleData(QQmlContext * theContext)
{

	if(_styleDataCreator == NULL)
	{
		_styleDataCreator = new QQmlComponent(qmlEngine(this));
		_styleDataCreator->setData("import QtQuick 2.10\n Item { "
							 "property bool active: true;"
							 "property string text: \"???\";"
							 "property int columnIndex: -1;"
							 "property int rowIndex: -1; }", QUrl());
	}


	QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
	_styleDataCreator->create(localIncubator);
	QQuickItem * _styleData = qobject_cast<QQuickItem*>(localIncubator.object());

	_styleData->setParent(this);
	_styleData->setParentItem(this);
	_styleData->setVisible(false);


	return _styleData;
}

QQmlContext * DataSetView::setStyleDataItem(QQmlContext * previousContext, QString text, bool active, int column, int row)
{
	if(previousContext == NULL)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("itemText",		text);
	previousContext->setContextProperty("itemActive",	active);
	previousContext->setContextProperty("columnIndex",	column);
	previousContext->setContextProperty("rowIndex",		row);

	return previousContext;
}

QQmlContext * DataSetView::setStyleDataRowNumber(QQmlContext * previousContext, int row)
{
	if(previousContext == NULL)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("rowIndex",		row);

	return previousContext;
}

QQmlContext * DataSetView::setStyleDataColumnHeader(QQmlContext * previousContext, QString text, int column)
{
	if(previousContext == NULL)
		previousContext = new QQmlContext(qmlContext(this), this);

	previousContext->setContextProperty("headerText",	text);
	previousContext->setContextProperty("columnIndex",	column);

	return previousContext;
}

void DataSetView::setItemDelegate(QQmlComponent * newDelegate)
{
	if(newDelegate != _itemDelegate)
	{
		if(_itemDelegate != NULL)
			delete _itemDelegate;
		_itemDelegate = newDelegate;
		emit itemDelegateChanged();
	}
}

void DataSetView::setRowNumberDelegate(QQmlComponent * newDelegate)
{
	if(newDelegate != _rowNumberDelegate)
	{
		if(_rowNumberDelegate != NULL)
			delete _rowNumberDelegate;
		_rowNumberDelegate = newDelegate;
		emit rowNumberDelegateChanged();
	}
}


void DataSetView::setColumnHeaderDelegate(QQmlComponent * newDelegate)
{
	if(newDelegate != _columnHeaderDelegate)
	{
		if(_columnHeaderDelegate != NULL)
			delete _columnHeaderDelegate;
		_columnHeaderDelegate = newDelegate;
		emit columnHeaderDelegateChanged();
	}
}


void DataSetView::setLeftTopCornerItem(QQuickItem * newItem)
{
	if(newItem != _leftTopItem)
	{
		if(_leftTopItem != NULL)
			delete _leftTopItem;
		_leftTopItem = newItem;

		if(_leftTopItem != NULL)
		{

			_leftTopItem->setParent(this);
			_leftTopItem->setParentItem(this);


			_leftTopItem->setProperty("z", 12);
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


QSGNode * DataSetView::updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *)
{
	if (width() <= 0 || height() <= 0) {
		delete oldNode;
		return 0;
	}

	//if(recalculateCellSizes) calculateCellContentSizes();

	const QRectF rect = boundingRect();


	const int linesPerNode = 1000; //Or something? should be multiple of 2 though

	if(!oldNode)
		oldNode = new QSGNode();

	QSGGeometryNode * currentNode = static_cast<QSGGeometryNode*>(oldNode->firstChild());


	for(int lineIndex=0; lineIndex<_lines.size();)
	{
		if(currentNode == NULL)
		{
			currentNode = new QSGGeometryNode;

			currentNode->setFlag(QSGNode::OwnsMaterial, false);
			currentNode->setFlag(QSGNode::OwnsGeometry, true);
			currentNode->setMaterial(&material);

			oldNode->appendChildNode(currentNode);
		}

		int geomSize = std::min(linesPerNode, (int)(_lines.size() - lineIndex));
		geomSize *= 2;

		QSGGeometry *geometry = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), geomSize);
		geometry->setLineWidth(1);
		geometry->setDrawingMode(GL_LINES);
		QSGGeometry::Point2D *points = geometry->vertexDataAsPoint2D();

		for(int geomIndex=0; geomIndex<geomSize; geomIndex+=2)
		{
			points[geomIndex  ].x = _lines[lineIndex].first.x() + rect.left();
			points[geomIndex  ].y = _lines[lineIndex].first.y() + rect.top();
			points[geomIndex+1].x = _lines[lineIndex].second.x() + rect.left();
			points[geomIndex+1].y = _lines[lineIndex].second.y() + rect.top();
			lineIndex++;
		}

		currentNode->setGeometry(geometry);

		currentNode = static_cast<QSGGeometryNode*>(currentNode->nextSibling());
	}


	std::queue<QSGGeometryNode*> killThem;

	while(currentNode != NULL) //superfluous children! Lets kill em
	{
		killThem.push(currentNode);
		currentNode = static_cast<QSGGeometryNode*>(currentNode->nextSibling());
	}

	while(killThem.size() > 0)
	{
		QSGGeometryNode * childToDie = killThem.front();
		killThem.pop();

		delete childToDie;
	}


	return oldNode;
}



void DataSetView::setViewportX(float newViewportX)
{
	if(newViewportX != _viewportX)
	{
		//std::cout << "setViewPortX!\n" <<std::flush;
		_viewportX = newViewportX;
		emit viewportXChanged();
	}
}

void DataSetView::setViewportY(float newViewportY)
{
	if(newViewportY != _viewportY)
	{
		//std::cout << "setViewPortY!\n" << std::flush;
		_viewportY = newViewportY;
		emit viewportYChanged();
	}
}

void DataSetView::setViewportW(float newViewportW)
{
	if(newViewportW != _viewportW)
	{
		//std::cout << "setViewPortW!\n" << std::flush;
		_viewportW = newViewportW;
		emit viewportWChanged();
	}
}

void DataSetView::setViewportH(float newViewportH)
{
	if(newViewportH != _viewportH)
	{
		//std::cout << "setViewPortH!\n" << std::flush;
		_viewportH = newViewportH;
		emit viewportHChanged();
	}
}

void DataSetView::modelDataChanged(const QModelIndex & begin, const QModelIndex & end, const QVector<int> & roles)
{
	calculateCellSizes();
}

void DataSetView::modelHeaderDataChanged(Qt::Orientation orientation, int first, int last)
{
	calculateCellSizes();
}

void DataSetView::modelAboutToBeReset()
{

}

void DataSetView::modelWasReset()
{
	setRolenames();
	calculateCellSizes();
}
