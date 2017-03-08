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

#include "tableviewmenueditor.h"

#include <QPaintEvent>
#include <QPainter>

#include <iostream>

TableViewMenuEditor::TableViewMenuEditor(QWidget *parent) :
	QWidget(parent)
{
	_menu = new QMenu(this);
}

QString TableViewMenuEditor::selected()
{
	return _selected;
}

void TableViewMenuEditor::setMenuContent(QStringList menuItems, QString selected)
{
	_menu->clear();

	QAction *active;

	foreach (QString item, menuItems)
	{
		QAction *added = _menu->addAction(item);
		if (selected == item)
			active = added;
	}

	_menu->setActiveAction(active);
	connect(_menu, SIGNAL(aboutToHide()), this, SLOT(aboutToHide()));

}

void TableViewMenuEditor::showEvent(QShowEvent *)
{
	QPoint pos(rect().left() + 20, rect().top() - 5);
	_menu->popup(mapToGlobal(pos), _menu->activeAction());
}

void TableViewMenuEditor::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);

	QPainterPath path;
	path.moveTo(5,  20 / 5);
	path.lineTo(16, 20 / 2);
	path.lineTo(5,  20 * 4 / 5);

	QBrush black(QColor(0x80, 0x80, 0x80));
	QBrush white(Qt::white);

	painter.fillRect(event->rect(), white);
	painter.fillPath(path, black);
}

void TableViewMenuEditor::aboutToHide()
{
	QAction *action = _menu->activeAction();
	if (action != NULL)
		_selected = action->text();

	emit editingFinished();
}
