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

#include "boundcheckbox.h"

#include <QEvent>
#include <QDebug>

BoundCheckBox::BoundCheckBox(QWidget *parent) :
    QCheckBox(parent)
{
	_boundTo = NULL;
}

void BoundCheckBox::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionBoolean *>(option);

	if (_boundTo != NULL)
		setChecked(_boundTo->value());
	else
		qDebug() << "could not bind to OptionBoolean in boundcheckbox.cpp";
}

void BoundCheckBox::nextCheckState()
{
	QCheckBox::nextCheckState();

	if (_boundTo != NULL)
		_boundTo->setValue(isChecked());
}

bool BoundCheckBox::event(QEvent *e)
{
	if (e->type() == QEvent::Wheel && this->isEnabled() == false)
		return false;

	return QCheckBox::event(e);
}
