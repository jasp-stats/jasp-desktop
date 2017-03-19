//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "boundgroupbox.h"

#include <QTimer>
#include <QRadioButton>
#include <QDebug>
#include <QLayout>

#include <QtAlgorithms>

#include <boost/foreach.hpp>

BoundGroupBox::BoundGroupBox(QWidget *parent) :
	GroupBox(parent)
{	
	_timer = new QTimer(this);
	_buttonGroup = new QButtonGroup(this);

	connect(_buttonGroup, SIGNAL(buttonClicked(QAbstractButton*)), this, SLOT(itemSelected(QAbstractButton*)));
}

void BoundGroupBox::bindTo(Option *option)
{
	_model.bindTo(option);

	int selectedIndex = _model.selectedIndex();
	if (selectedIndex >= 0 && selectedIndex < _buttonGroup->buttons().count())
		_buttonGroup->buttons().at(_model.selectedIndex())->setChecked(true);
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

