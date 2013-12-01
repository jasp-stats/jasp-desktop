#ifndef TABLEVIEW_H
#define TABLEVIEW_H

#include <QTableView>

#include "widgets/infopopup.h"

#include "datasettablemodel.h"
#include "maintablehorizontalheader.h"

class MainTableView : public QTableView
{
	Q_OBJECT
public:
	explicit MainTableView(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model) override;

protected:
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	virtual void verticalScrollbarValueChanged(int value) override;


signals:
	
public slots:

private slots:
	void badDataEnteredHandler(QModelIndex index);
	void columnTypeChanged(int columnIndex, Column::ColumnType newColumnType);

private:

	DataSetTableModel *_dataSetModel;

	bool _infoPopupVisible;
	QModelIndex _infoPopupIndex;
	InfoPopup *_infoPopup;

	void showInfoPopup(QModelIndex &index);
	void moveInfoPopup();
	void hideInfoPopup();

	MainTableHorizontalHeader *_header;
	
};

#endif // TABLEVIEW_H
