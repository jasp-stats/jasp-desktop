#ifndef BOUNDPAIRSTABLE_H
#define BOUNDPAIRSTABLE_H

#include <QTableView>
#include "bound.h"
#include "availablefieldslistview.h"
#include "assignbutton.h"
#include "tablemodelpairsassigned.h"
#include "tableview.h"

class BoundPairsTable : public TableView, public Bound
{
	Q_OBJECT
public:
	explicit BoundPairsTable(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void notifyDragWasDropped() OVERRIDE;

protected:

	void resizeEvent(QResizeEvent *e);
	void moveEvent(QMoveEvent *e);

	void setupKey();
	void repositionKey();

private:

	TableModelPairsAssigned *_tableModel;
	QWidget *_variableTypeKey;

};

#endif // BOUNDPAIRSTABLE_H
