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

#include "expanderbutton.h"

#include <QDialog>
#include <QIcon>
#include <QImage>
#include <QDebug>

ExpanderButton::ExpanderButton(QWidget *parent) :
	QPushButton(parent)
{
    _expanded = false;
    _expandedIcon = QIcon(QString(":/images/expander-arrow-down.png"));
    _contractedIcon = QIcon(QString(":/images/expander-arrow-up.png"));

	setCheckable(true);
	setIcon(_contractedIcon);

	setFocusPolicy(Qt::NoFocus);

#ifdef __APPLE__
	setStyleSheet(QString("QPushButton { text-align: left ; }"));
#else
	setStyleSheet(QString("QPushButton { text-align: left ; padding: 3px 9px ; }"));
#endif

}

void ExpanderButton::nextCheckState()
{
	_expanded = !_expanded;
	setIcon(_expanded ? _expandedIcon : _contractedIcon);

	QObjectList siblings = this->parentWidget()->children();

	for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
		QWidget* w = dynamic_cast<QWidget*>(*itr);
        if (w != NULL && w != this)
			w->setVisible(_expanded);
	}
}

QSize ExpanderButton::sizeHint() const {
	 QSize sizeHint = QPushButton::sizeHint();

	 int width = sizeHint.width();
	QObjectList siblings = this->parentWidget()->children();
	for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
		QWidget* w = dynamic_cast<QWidget*>(*itr);
		if (w != NULL && w != this && w->sizeHint().width() > width)
			width = w->sizeHint().width();
	}

	sizeHint.setWidth(width);

	return sizeHint;
}

QSize ExpanderButton::minimumSizeHint() const {
	 QSize sizeHint = QPushButton::minimumSizeHint();

	 int width = sizeHint.width();
	QObjectList siblings = this->parentWidget()->children();
	for (QObjectList::Iterator itr = siblings.begin(); itr != siblings.end(); itr++) {
		QWidget* w = dynamic_cast<QWidget*>(*itr);
		if (w != NULL && w != this && w->minimumSizeHint().width() > width)
			width = w->minimumSizeHint().width();
	}

	sizeHint.setWidth(width);

	return sizeHint;
}
