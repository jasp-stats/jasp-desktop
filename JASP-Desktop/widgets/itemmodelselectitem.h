#ifndef ITEMMODELSELECTITEM_H
#define ITEMMODELSELECTITEM_H

#include <QAbstractListModel>

#include "boundmodel.h"
#include "common.h"
#include "options/optionlist.h"

class ItemModelSelectItem : public QAbstractListModel, public BoundModel
{
public:
	ItemModelSelectItem();

	virtual void bindTo(Option *option) OVERRIDE;
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;

	void setValueByIndex(int index);
	int valueIndex() const;

	void setValue(QString value);
	QString value() const;

private:
	OptionList *_boundTo;
};

#endif // ITEMMODELSELECTITEM_H
