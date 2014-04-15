#ifndef BOUNDCOMBOBOX_H
#define BOUNDCOMBOBOX_H

#include <QComboBox>
#include "bound.h"
#include "options/optionlist.h"
#include "itemmodelselectitem.h"

class BoundComboBox : public QComboBox, public Bound
{
	Q_OBJECT
public:
	explicit BoundComboBox(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	void setModel(QAbstractItemModel *newModel);

private slots:
	void changeHandler(int index);
	void updateSelection();

private:
	BoundModel *_model;
	ItemModelSelectItem _defaultModel;

};

#endif // BOUNDCOMBOBOX_H
