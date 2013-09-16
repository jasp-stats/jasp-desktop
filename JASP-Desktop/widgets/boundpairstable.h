#ifndef BOUNDPAIRSTABLE_H
#define BOUNDPAIRSTABLE_H

#include <QTableView>
#include "bound.h"
#include "availablefieldslistview.h"
#include "assignbutton.h"
#include "tablemodelvariablesassigned.h"

class BoundPairsTable : public QTableView, public Bound
{
	Q_OBJECT
public:
	explicit BoundPairsTable(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model);
	virtual void bindTo(Option *option) override;

protected:
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	virtual void startDrag(Qt::DropActions supportedActions) override;
	virtual void focusInEvent(QFocusEvent *event) override;

	void resizeEvent(QResizeEvent *e);
	void moveEvent(QMoveEvent *e);

	void setupKey();
	void repositionKey();

signals:
	void selectionUpdated();
	void focused();

private:

	TableModelVariablesAssigned *_tableModel;
	QWidget *_variableTypeKey;

};

#endif // BOUNDPAIRSTABLE_H
