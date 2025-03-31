#ifndef DATASETVIEW_H
#define DATASETVIEW_H

#include "datasetviewbase.h"
#include "data/expanddataproxymodel.h"


class DataSetView : public DataSetViewBase
{
	Q_OBJECT

	Q_PROPERTY( bool					expandDataSet			READ expandDataSet			WRITE setExpandDataSet			NOTIFY expandDataSetChanged			)
	Q_PROPERTY( bool					mainData				READ mainData				WRITE setMainData				NOTIFY mainDataChanged				)
	
public:
	friend ExpandDataProxyModel;

							DataSetView(QQuickItem *parent = nullptr);

							static DataSetView *	mainDataViewer()								{ return _mainDataSetView;}

	void					setModel(QAbstractItemModel * model)		override;

	QAbstractItemModel	*	model()								const	override	{ return _expandedModel->sourceModel();								}
	bool					mainData()							const				{ return _mainData;													}
	bool					expandDataSet()						const	override	{ return _expandedModel ? _expandedModel->expandDataSet() : false;	}

	void setExpandDataSet(bool			  expandDataSet);
	void setMainData(bool newMainData);
		
signals:
	void		expandDataSetChanged();
	void		undoChanged();
	void		mainDataChanged();
	
public slots:

	int			rowCount()								{ return _expandedModel->rowCount();	}
	int			columnCount()							{ return _expandedModel->columnCount(); }

	void		cut(	QPoint where = QPoint(-1,-1)) { _copy(where, true);  }
	void		copy(	QPoint where = QPoint(-1,-1)) { _copy(where, false); }
	void		paste(	QPoint where = QPoint(-1,-1));

	QString		columnInsertBefore(			int col = -1,	bool computed = false, bool R = false);
	QString		columnInsertAfter(			int col = -1,	bool computed = false, bool R = false);
	void		columnComputedInsertAfter(	int col = -1,	bool R=true);
	void		columnComputedInsertBefore(	int col = -1,	bool R=true);
	void		columnsDeleteSelected();
	void		columnsDelete(				int col);
	void		columnReverseValues(		int col = -1);
	void		columnautoSortByValues(		int col = -1);
	void		rowInsertBefore(			int row = -1);
	void		rowInsertAfter(				int row = -1);
	void		rowsDelete(					int row);
	void		rowsDeleteSelected();
	void		cellsClear();

	void		undo()									{ _expandedModel->undo();				}
	void		redo()									{ _expandedModel->redo();				}
	QString		undoText()								{ return _expandedModel->undoText();	}
	QString		redoText()								{ return _expandedModel->redoText();	}

	void		setColumnType(int columnIndex, int newColumnType);
	void		resizeData(int rows, int columns);
	void		currentSelectedColumnHandler(const QModelIndex &current, const QModelIndex &previous);

	bool		isVirtual(			const QPoint& point)	override;
	bool		isRowVirtual(		int row)				override;
	bool		isColumnVirtual(	int column)				override;
	bool		isFiltered(			int row, int col)		override;

protected:
	void		_copy(QPoint where, bool clear);
	void		setRolenames()								override;


	ExpandDataProxyModel								*	_expandedModel			 = nullptr;
	static DataSetView									*	_mainDataSetView;
	bool													_mainData				= false;
};



#endif // DATASETVIEW_H
