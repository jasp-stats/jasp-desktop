#ifndef BOUNDCOMBOBOX_H
#define BOUNDCOMBOBOX_H

#include <QComboBox>
#include "bound.h"
#include "options/optionlist.h"

class BoundComboBox : public QComboBox, public Bound
{
	Q_OBJECT
public:
	explicit BoundComboBox(QWidget *parent = 0);

	void bindTo(Option *option) OVERRIDE;

private slots:
	void changeHandler(int index);

private:
	OptionList *_boundTo;

};

#endif // BOUNDCOMBOBOX_H
