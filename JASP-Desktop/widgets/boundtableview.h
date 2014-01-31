#ifndef BOUNDTABLEVIEW_H
#define BOUNDTABLEVIEW_H

#include "tableview.h"
#include "bound.h"
#include "boundmodel.h"

class BoundTableView : public TableView, public Bound
{
public:
	BoundTableView(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void setModel(QAbstractItemModel *model) OVERRIDE;

private:
	BoundModel *_model;
};

#endif // BOUNDTABLEVIEW_H
