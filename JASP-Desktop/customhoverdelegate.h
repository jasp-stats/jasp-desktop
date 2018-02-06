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

#ifndef CUSTOMHOVERDELEGATE_H
#define CUSTOMHOVERDELEGATE_H

#include <QStyledItemDelegate>
#include <QPainter>
#include <QToolTip>
#include <QColor>

class CustomHoverDelegate : public QStyledItemDelegate
{
public:
	CustomHoverDelegate(QObject *parent = 0) : QStyledItemDelegate(parent) {}
	void paint (QPainter *painter, const QStyleOptionViewItem & option, const QModelIndex & index) const
	{
		if(option.state & QStyle::State_MouseOver)
		{
			painter->fillRect(option.rect, QColor(220, 241, 251));
			QString str = index.data().toString();
			QToolTip::showText(QCursor::pos(), str);
		}
		QStyledItemDelegate::paint(painter, option, index);
	}
};

#endif // CUSTOMHOVERDELEGATE_H
