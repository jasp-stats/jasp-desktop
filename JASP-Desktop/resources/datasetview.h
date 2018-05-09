#ifndef DATASETVIEW_H
#define DATASETVIEW_H

#include <QObject>
#include <QQuickItem>
#include <QAbstractTableModel>
#include <vector>
#include <stack>
#include <QSGFlatColorMaterial>
#include <iostream>
#include <map>
#include <QFontMetricsF>

//#define DEBUG_VIEWPORT

class DataSetView : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY( QAbstractTableModel * model READ model					WRITE setModel					NOTIFY modelChanged )
	Q_PROPERTY( float itemHorizontalPadding READ itemHorizontalPadding	WRITE setItemHorizontalPadding	NOTIFY itemHorizontalPaddingChanged )
	Q_PROPERTY( float itemVerticalPadding	READ itemVerticalPadding	WRITE setItemVerticalPadding	NOTIFY itemVerticalPaddingChanged )

	Q_PROPERTY( float viewportX				READ viewportX				WRITE setViewportX				NOTIFY viewportXChanged )
	Q_PROPERTY( float viewportY				READ viewportY				WRITE setViewportY				NOTIFY viewportYChanged )
	Q_PROPERTY( float viewportW				READ viewportW				WRITE setViewportW				NOTIFY viewportWChanged )
	Q_PROPERTY( float viewportH				READ viewportH				WRITE setViewportH				NOTIFY viewportHChanged )

	Q_PROPERTY( QQmlComponent * itemDelegate			READ itemDelegate			WRITE setItemDelegate			NOTIFY itemDelegateChanged )
	Q_PROPERTY( QQmlComponent * rowNumberDelegate		READ rowNumberDelegate		WRITE setRowNumberDelegate		NOTIFY rowNumberDelegateChanged )
	Q_PROPERTY( QQmlComponent * columnHeaderDelegate	READ columnHeaderDelegate	WRITE setColumnHeaderDelegate	NOTIFY columnHeaderDelegateChanged )
	Q_PROPERTY( QQmlComponent * leftTopCornerDelegate	READ leftTopCornerDelegate	WRITE setLeftTopCornerDelegate	NOTIFY leftTopCornerDelegateChanged )

	Q_PROPERTY( QFont font	MEMBER _font)

public:
	DataSetView();
	~DataSetView();

	QAbstractTableModel * model() { return _model; }
	void setModel(QAbstractTableModel * model);


	float fontPixelSize()			{ return _fontPixelSize;}
	float itemHorizontalPadding()	{ return _itemHorizontalPadding;}
	float itemVerticalPadding()		{ return _itemVerticalPadding;}

	float viewportX()				{ return _viewportX; }
	float viewportY()				{ return _viewportY; }
	float viewportW()				{ return _viewportW; }
	float viewportH()				{ return _viewportH; }

	QQmlComponent * itemDelegate()			{ return _itemDelegate; }
	QQmlComponent * rowNumberDelegate()		{ return _rowNumberDelegate; }
	QQmlComponent * columnHeaderDelegate()	{ return _columnHeaderDelegate; }
	QQmlComponent * leftTopCornerDelegate()	{ return _leftTopCornerDelegate; }

	void setViewportX(float newViewportX);
	void setViewportY(float newViewportY);
	void setViewportW(float newViewportW);
	void setViewportH(float newViewportH);

	void setItemHorizontalPadding(float newHorizontalPadding)	{ if(newHorizontalPadding != _itemHorizontalPadding)	{ _itemHorizontalPadding = newHorizontalPadding;	emit itemHorizontalPaddingChanged();	update(); }}
	void setItemVerticalPadding(float newVerticalPadding)		{ if(newVerticalPadding != _itemVerticalPadding)		{ _itemVerticalPadding = newVerticalPadding;		emit itemVerticalPaddingChanged();		update(); }}

	void setRowNumberDelegate(QQmlComponent * newDelegate);
	void setColumnHeaderDelegate(QQmlComponent * newDelegate);
	void setItemDelegate(QQmlComponent * newDelegate);
	void setLeftTopCornerDelegate(QQmlComponent * newDelegate);

protected:
	QSGNode *updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *data) override;

	QAbstractTableModel * _model = NULL;

	std::vector<QSizeF>								_cellSizes; //[col]
	std::vector<float>								_colXPositions; //[col][row]
	std::vector<float>								_dataColsMaxWidth;
	std::stack<QQuickItem*>							_textItemStorage;
	std::stack<QQuickItem*>							_rowNumberStorage;
	std::map<int, QQuickItem *>						_rowNumberItems;
	std::stack<QQuickItem*>							_columnHeaderStorage;
	std::map<int, QQuickItem *>						_columnHeaderItems;
	std::map<int, std::map<int, QQuickItem *>>		_cellTextItems;			//[col][row]
	std::vector<std::pair<QVector2D, QVector2D>>	_lines;
	QQuickItem *									_leftTopItem = NULL;

	float _dataRowsMaxHeight;
	bool _recalculateCellSizes = false, _ignoreViewpoint = true;

	float _fontPixelSize			= 20;
	float _itemHorizontalPadding	= 8;
	float _itemVerticalPadding		= 8;

	QQmlComponent * _itemDelegate			= NULL;
	QQmlComponent * _rowNumberDelegate		= NULL;
	QQmlComponent * _columnHeaderDelegate	= NULL;
	QQmlComponent * _leftTopCornerDelegate	= NULL;

	QSGFlatColorMaterial material;

	QFont _font;

	float _viewportX=0, _viewportY=0, _viewportW=1, _viewportH=1;
	int _previousViewportColMin = -1, _previousViewportColMax = -1, _previousViewportRowMin = -1, _previousViewportRowMax = -1, _viewportMargin = 10;

	QQuickItem * createTextItem(int row, int col);
	void storeTextItem(int row, int col, bool cleanUp = true);

	QQuickItem * createRowNumber(int row);
	void storeRowNumber(int row);

	QQuickItem * createColumnHeader(int row);
	void storeColumnHeader(int row);

	QQuickItem * createleftTopCorner();

	QFontMetricsF _metricsFont;

	std::map<std::string, int> _roleNameToRole;

	void setRolenames();

	float _rowNumberMaxWidth = 0;

signals:
	void modelChanged();
	void fontPixelSizeChanged();
	void itemHorizontalPaddingChanged();
	void itemVerticalPaddingChanged();

	void viewportXChanged();
	void viewportYChanged();
	void viewportWChanged();
	void viewportHChanged();

	void rowNumberDelegateChanged();
	void columnHeaderDelegateChanged();
	void itemDelegateChanged();
	void leftTopCornerDelegateChanged();

	void itemSizeChanged();



public slots:
	void aContentSizeChanged() { _recalculateCellSizes = true; }
	void viewportChanged();
	void myParentChanged(QQuickItem *);

	void reloadTextItems();
	void reloadRowNumbers();
	void reloadColumnHeaders();
	void reloadLeftTopCorner();

	void calculateCellSizes();

	void modelDataChanged(const QModelIndex & begin, const QModelIndex & end, const QVector<int> & roles);
	void modelHeaderDataChanged(Qt::Orientation orientation, int first, int last);
	void modelAboutToBeReset();
	void modelWasReset();

};

#endif // DATASETVIEW_H
