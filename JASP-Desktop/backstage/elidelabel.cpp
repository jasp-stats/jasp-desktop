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

#include "elidelabel.h"

ElideLabel::ElideLabel(QWidget *parent) : QLabel(parent)
{
	setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
}

QSize ElideLabel::minimumSizeHint() const
{
	return QSize(10, QLabel::minimumSizeHint().height());
}

QSize ElideLabel::sizeHint() const
{
	return QSize(10, QLabel::sizeHint().height());
}

void ElideLabel::resizeEvent(QResizeEvent *event)
{
	if (text() != _modifiedText)
		_originalText = text();

	QFontMetrics metrics(font());

	_modifiedText = metrics.elidedText(_originalText, Qt::ElideRight, width());

	setText(_modifiedText);

	QLabel::resizeEvent(event);
}

