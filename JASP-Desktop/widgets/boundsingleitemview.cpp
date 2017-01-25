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

#include "boundsingleitemview.h"
#include "boundlistview.h"



boundSingleItemView::boundSingleItemView(QWidget *parent)
	: BoundListView(parent)
{
	/*int height = this->fontMetrics().height();
	this->setMaximumHeight(height);
	this->setMinimumHeight(height);*/
}

int boundSingleItemView::itemCount() const
{
	return 1;
}

QSize boundSingleItemView::sizeHint() const
{
	static int height = -1;

	if (height == -1)
		height = this->fontMetrics().height() + 10;

	QSize sizeHint = BoundListView::sizeHint();
	sizeHint.setHeight(height);

	return sizeHint;
}

QSize boundSingleItemView::minimumSizeHint() const
{
	static int height = -1;

	if (height == -1)
		height = this->fontMetrics().height() + 10;

	QSize sizeHint = BoundListView::minimumSizeHint();
	sizeHint.setHeight(height);

	return sizeHint;
}
