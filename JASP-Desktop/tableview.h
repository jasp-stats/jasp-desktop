#ifndef TABLEVIEW_H
#define TABLEVIEW_H

#include <QTableView>

#include "widgets/infopopup.h"

class TableView : public QTableView
{
	Q_OBJECT
public:
	explicit TableView(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model) override;

protected:
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	virtual void verticalScrollbarValueChanged(int value) override;


signals:
	
public slots:

private slots:
	void badDataEnteredHandler(QModelIndex index);

private:
	bool _infoPopupVisible;
	QModelIndex _infoPopupIndex;
	InfoPopup *_infoPopup;

	void showInfoPopup(QModelIndex &index);
	void moveInfoPopup();
	void hideInfoPopup();
	
};

#endif // TABLEVIEW_H
