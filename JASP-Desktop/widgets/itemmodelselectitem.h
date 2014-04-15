#ifndef ITEMMODELSELECTITEM_H
#define ITEMMODELSELECTITEM_H

#include <QStandardItemModel>

#include "boundmodel.h"
#include "common.h"
#include "options/optionlist.h"

class ItemModelSelectItem : public QStandardItemModel, public BoundModel
{
public:
	ItemModelSelectItem();

	virtual void bindTo(Option *option) OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	int selectedIndex();
	void setSelected(int index);


private:
	int _selectedIndex;
	OptionList *_boundTo;

};

#endif // ITEMMODELSELECTITEM_H
