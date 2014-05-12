#include "boundgroupbox.h"

#include <QTimer>
#include <QRadioButton>
#include <QDebug>
#include <QLayout>

#include <QtAlgorithms>

#include <boost/foreach.hpp>

BoundGroupBox::BoundGroupBox(QWidget *parent) :
	QGroupBox(parent)
{	
	_timer = new QTimer(this);
	_buttonGroup = new QButtonGroup(this);
	setFlat(true);

	connect(_buttonGroup, SIGNAL(buttonClicked(QAbstractButton*)), this, SLOT(itemSelected(QAbstractButton*)));
}

void BoundGroupBox::bindTo(Option *option)
{
	_model.bindTo(option);
}

void BoundGroupBox::childEvent(QChildEvent *child)
{
	if (child->added())
		_timer->singleShot(0, this, SLOT(updateGroup()));
}

bool compareNames(QObject *left, QObject *right)
{
	return left->objectName().compare(right->objectName()) < 0;
}

void BoundGroupBox::updateGroup()
{
	BOOST_FOREACH(QAbstractButton *button, _buttonGroup->buttons())
		_buttonGroup->removeButton(button);

	QList<QRadioButton *> buttons;

	foreach (QObject *child, this->children())
	{
		QRadioButton *radio = qobject_cast<QRadioButton *>(child);
		if (radio != NULL)
			buttons.append(radio);
	}

	qSort(buttons.begin(), buttons.end(), compareNames);

	int index = 0;
	int selectedIndex = _model.selectedIndex();

	foreach (QRadioButton *button, buttons)
	{
		if (index == selectedIndex)
			button->setChecked(true);
		_buttonGroup->addButton(button, index);
		index++;
	}
}

void BoundGroupBox::itemSelected(QAbstractButton *)
{
	_model.setSelected(_buttonGroup->checkedId());
}

