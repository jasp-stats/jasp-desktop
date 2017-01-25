//
// Copyright (C) 2017 University of Amsterdam
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

#include "verticalscrollarea.h"

#include <QEvent>
#include <QScrollBar>
#include <QResizeEvent>

VerticalScrollArea::VerticalScrollArea(QWidget *parent) : QScrollArea(parent)
{
	setWidgetResizable(true);
	setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
	setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
}

bool VerticalScrollArea::eventFilter(QObject *object, QEvent *event)
{
	// This works because QScrollArea::setWidget installs an eventFilter on the widget

	if(object && object == widget() && event->type() == QEvent::Resize)
		setMinimumWidth(widget()->minimumSizeHint().width() + verticalScrollBar()->width());

	return QScrollArea::eventFilter(object, event);
}

