#ifndef DATASETVIEW_H
#define DATASETVIEW_H

#include <QObject>
#include <QQuickItem>
#include <QAbstractItemModel>
#include <vector>
#include <stack>
#include <QSGFlatColorMaterial>

#include <map>
#include <QtQml>
#include "utilities/qutils.h"
#include "gui/preferencesmodel.h"


//#define DATASETVIEW_DEBUG_VIEWPORT
//#define DATASETVIEW_DEBUG_CREATION
#define DATASETVIEW_SHOW_ITEMS_PLEASE
#define DATASETVIEW_ADD_LINES_PLEASE

struct ItemContextualized
{
	ItemContextualized(QQmlContext * context = nullptr, QQuickItem * item = nullptr) : item(item), context(context) {}

	QQuickItem * item;
	QQmlContext * context;
};


/// Custom QQuickItem to render data tables witch caching and only displaying the necessary cells and lines
/// Supports scaling the data into millions of columns and rows without any noticable slowdowns (the model could slow it down though)
/// Contains custom rendering code for the lines to make sure they are always a single pixel wide.
/// Caching is a bit flawed at the moment though so when changing data in the model it is best to turn that off.
/// It also uses pools of header-, rowheader- and general-items when they go out of view to avoid the overhead of recreating them all the time.
class DataSetView : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY( QAbstractItemModel	*	model					READ model					WRITE setModel					NOTIFY modelChanged					)
	Q_PROPERTY( int						itemHorizontalPadding	READ itemHorizontalPadding	WRITE setItemHorizontalPadding	NOTIFY itemHorizontalPaddingChanged )
	Q_PROPERTY( int						itemVerticalPadding		READ itemVerticalPadding	WRITE setItemVerticalPadding	NOTIFY itemVerticalPaddingChanged	)
	Q_PROPERTY( double					viewportX				READ viewportX				WRITE setViewportX				NOTIFY viewportXChanged				)
	Q_PROPERTY( double					viewportY				READ viewportY				WRITE setViewportY				NOTIFY viewportYChanged				)
	Q_PROPERTY( double					viewportW				READ viewportW				WRITE setViewportW				NOTIFY viewportWChanged				)
	Q_PROPERTY( double					viewportH				READ viewportH				WRITE setViewportH				NOTIFY viewportHChanged				)
	Q_PROPERTY( QQmlComponent		*	itemDelegate			READ itemDelegate			WRITE setItemDelegate			NOTIFY itemDelegateChanged			)
	Q_PROPERTY( QQmlComponent		*	rowNumberDelegate		READ rowNumberDelegate		WRITE setRowNumberDelegate		NOTIFY rowNumberDelegateChanged		)
	Q_PROPERTY( QQmlComponent		*	columnHeaderDelegate	READ columnHeaderDelegate	WRITE setColumnHeaderDelegate	NOTIFY columnHeaderDelegateChanged	)
	Q_PROPERTY( QQuickItem			*	leftTopCornerItem		READ leftTopCornerItem		WRITE setLeftTopCornerItem		NOTIFY leftTopCornerItemChanged		)
	Q_PROPERTY( QQuickItem			*	extraColumnItem			READ extraColumnItem		WRITE setExtraColumnItem		NOTIFY extraColumnItemChanged		)
	Q_PROPERTY( double					headerHeight			READ headerHeight											NOTIFY headerHeightChanged			)
	Q_PROPERTY( double					rowNumberWidth			READ rowNumberWidth			WRITE setRowNumberWidth			NOTIFY rowNumberWidthChanged		)
	Q_PROPERTY( bool					cacheItems				READ cacheItems				WRITE setCacheItems				NOTIFY cacheItemsChanged			)
	Q_PROPERTY( QQuickItem			*	tableViewItem			READ tableViewItem			WRITE setTableViewItem												)

public:
	DataSetView(QQuickItem *parent = nullptr);

	static DataSetView * lastInstancedDataSetView() { return _lastInstancedDataSetView; }

	void setModel(QAbstractItemModel * model);
	QAbstractItemModel * model()			{ return _model; }

	int itemHorizontalPadding()				{ return _itemHorizontalPadding;}
	int itemVerticalPadding()				{ return _itemVerticalPadding;}

	double viewportX()						{ return _viewportX; }
	double viewportY()						{ return _viewportY; }
	double viewportW()						{ return _viewportW; }
	double viewportH()						{ return _viewportH; }

	QQmlComponent * itemDelegate()			{ return _itemDelegate; }
	QQmlComponent * rowNumberDelegate()		{ return _rowNumberDelegate; }
	QQmlComponent * columnHeaderDelegate()	{ return _columnHeaderDelegate; }

	QQuickItem * leftTopCornerItem()		{ return _leftTopItem; }
	QQuickItem * extraColumnItem()			{ return _extraColumnItem; }

	QQuickItem * tableViewItem()			{ return _tableViewItem; }

	bool cacheItems()						{ return _cacheItems; }

	GENERIC_SET_FUNCTION(CacheItems, _cacheItems, cacheItemsChanged, bool)

	GENERIC_SET_FUNCTION(ViewportX, _viewportX, viewportXChanged, double)
	GENERIC_SET_FUNCTION(ViewportY, _viewportY, viewportYChanged, double)
	GENERIC_SET_FUNCTION(ViewportW, _viewportW, viewportWChanged, double)
	GENERIC_SET_FUNCTION(ViewportH, _viewportH, viewportHChanged, double)

	void setItemHorizontalPadding(	int newHorizontalPadding)	{ if(newHorizontalPadding	!= _itemHorizontalPadding)	{ _itemHorizontalPadding	= newHorizontalPadding;		emit itemHorizontalPaddingChanged();	update(); }}
	void setItemVerticalPadding(	int newVerticalPadding)		{ if(newVerticalPadding		!= _itemVerticalPadding)	{ _itemVerticalPadding		= newVerticalPadding;		emit itemVerticalPaddingChanged();		update(); }}

	void setRowNumberDelegate(		QQmlComponent * newDelegate);
	void setColumnHeaderDelegate(	QQmlComponent * newDelegate);
	void setItemDelegate(			QQmlComponent * newDelegate);

	void setLeftTopCornerItem(		QQuickItem * newItem);
	void setExtraColumnItem(		QQuickItem * newItem);

	void setTableViewItem(			QQuickItem * tableViewItem) { _tableViewItem = tableViewItem; }

	int headerHeight()					{ return _dataRowsMaxHeight; }
	int rowNumberWidth()	{ return _rowNumberMaxWidth; }

	void resetItems();

	Q_INVOKABLE QQuickItem*	getColumnHeader(int col)	{ return _columnHeaderItems.count(col) > 0	? _columnHeaderItems[col]->item : nullptr;	}
	Q_INVOKABLE QQuickItem*	getRowHeader(int row)		{ return _rowNumberItems.count(row) > 0		? _rowNumberItems[row]->item	: nullptr;	}

	GENERIC_SET_FUNCTION(HeaderHeight,		_dataRowsMaxHeight, headerHeightChanged,		double)
	GENERIC_SET_FUNCTION(RowNumberWidth,	_rowNumberMaxWidth, rowNumberWidthChanged,		double)

signals:
	void modelChanged();
	void itemHorizontalPaddingChanged();
	void itemVerticalPaddingChanged();

	void viewportXChanged();
	void viewportYChanged();
	void viewportWChanged();
	void viewportHChanged();

	void rowNumberDelegateChanged();
	void columnHeaderDelegateChanged();
	void itemDelegateChanged();
	void leftTopCornerItemChanged();
	void extraColumnItemChanged();

	void itemSizeChanged();

	void headerHeightChanged();
	void rowNumberWidthChanged();

	void cacheItemsChanged();



public slots:
	void calculateCellSizes()	{ calculateCellSizesAndClear(false); }
	void aContentSizeChanged()	{ _recalculateCellSizes = true; }
	void viewportChanged();
	void myParentChanged(QQuickItem *);

	void reloadTextItems();
	void reloadRowNumbers();
	void reloadColumnHeaders();

	void modelDataChanged(const QModelIndex &, const QModelIndex &, const QVector<int> &);
	void modelHeaderDataChanged(Qt::Orientation, int, int);
	void modelAboutToBeReset();
	void modelWasReset();
	void setExtraColumnX();

protected:
	void calculateCellSizesAndClear(bool clearStorage);
	void setRolenames();
	void determineCurrentViewPortIndices();
	void storeOutOfViewItems();
	void buildNewLinesAndCreateNewItems();

#ifdef DATASETVIEW_ADD_LINES_PLEASE
	QSGNode *updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *data) override;
#endif
	float extraColumnWidth() { return _extraColumnItem == nullptr ? 0 : 1 + _extraColumnItem->width(); }

	QQuickItem *	createTextItem(int row, int col);
	void			storeTextItem(int row, int col, bool cleanUp = true);

	QQuickItem *	createRowNumber(int row);
	void			storeRowNumber(int row);

	QQuickItem *	createColumnHeader(int col);
	void			storeColumnHeader(int col);

	QQuickItem *	createleftTopCorner();
	void			updateExtraColumnItem();

	QQmlContext * setStyleDataItem(			QQmlContext * previousContext, bool active, size_t col, size_t row);
	QQmlContext * setStyleDataRowNumber(	QQmlContext * previousContext, QString text, int row);
	QQmlContext * setStyleDataColumnHeader(	QQmlContext * previousContext, QString text, int column, bool isComputed, bool isInvalidated, bool isFiltered,  QString computedError);

	void addLine(float x0, float y0, float x1, float y1);

	QSizeF getTextSize(const QString& text)	const;
	QSizeF getColumnSize(int col);
	QSizeF getRowHeaderSize();

protected:
	QAbstractItemModel *									_model = nullptr;

	/// @todo, Amir: these guys, if not initialized, they will crash JASP at start
	std::vector<QSizeF>										_cellSizes; //[col]
	std::vector<double>										_colXPositions; //[col][row]
	std::vector<double>										_dataColsMaxWidth;
	std::stack<ItemContextualized*>							_textItemStorage;
	bool													_cacheItems = true;
	std::stack<ItemContextualized*>							_rowNumberStorage;
	std::map<int, ItemContextualized *>						_rowNumberItems;
	std::stack<ItemContextualized*>							_columnHeaderStorage;
	std::map<int, ItemContextualized *>						_columnHeaderItems;
	std::map<int, std::map<int, ItemContextualized *>>		_cellTextItems;			//[col][row]
	std::vector<float>										_lines;
	QQuickItem											*	_leftTopItem		= nullptr,
														*	_extraColumnItem	= nullptr,
														*	_tableViewItem		= nullptr;

	bool		_recalculateCellSizes	= false,
				_ignoreViewpoint		= true;

	double		_dataRowsMaxHeight,
				_dataWidth				= -1,
				_rowNumberMaxWidth		= 0;

	int			_itemHorizontalPadding	= 8,
				_itemVerticalPadding	= 8;


	QQmlComponent	* _itemDelegate				= nullptr;
	QQmlComponent	* _rowNumberDelegate		= nullptr;
	QQmlComponent	* _columnHeaderDelegate		= nullptr;
	QQmlComponent	* _leftTopCornerDelegate	= nullptr;
	QQmlComponent	* _styleDataCreator			= nullptr;

	QSGFlatColorMaterial material;

	double _viewportX=0, _viewportY=0, _viewportW=1, _viewportH=1;
	int _previousViewportColMin = -1,
		_previousViewportColMax = -1,
		_previousViewportRowMin = -1,
		_previousViewportRowMax = -1,
		_viewportMargin			=  1,
		_currentViewportColMin	= -1,
		_currentViewportColMax	= -1,
		_currentViewportRowMin	= -1,
		_currentViewportRowMax	= -1;

	std::map<std::string, int> _roleNameToRole;

	bool	_linesWasChanged	= false;
	size_t	_linesActualSize	= 0;

	std::map<size_t, std::map<size_t, unsigned char>>	_storedLineFlags;
	std::map<size_t, std::map<size_t, QString>>			_storedDisplayText;

	static DataSetView * _lastInstancedDataSetView;
};



#endif // DATASETVIEW_H
