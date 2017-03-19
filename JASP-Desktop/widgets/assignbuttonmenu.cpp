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

#include "assignbuttonmenu.h"

#include "droptarget.h"

#include <QMenu>
#include <QDebug>

AssignButtonMenu::AssignButtonMenu(QWidget *parent)
	: Button(parent)
{
	_source = NULL;
	_target = NULL;

	connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));

	setIcon(QIcon(":/images/arrow-right.png"));

	setEnabled(false);
}

void AssignButtonMenu::setSourceAndTarget(DropTarget *source, DropTarget *target)
{
	_source = source;
	_target = target;

	_source->selectionUpdated.connect(boost::bind(&AssignButtonMenu::sourceChanged, this));
	_source->focused.connect(boost::bind(&AssignButtonMenu::sourceChanged, this));

	_target->selectionUpdated.connect(boost::bind(&AssignButtonMenu::targetChanged, this));
	_target->focused.connect(boost::bind(&AssignButtonMenu::targetChanged, this));
}

void AssignButtonMenu::buttonClicked()
{
	QMenu *m = menu();

	if (m != NULL)
		m->show();

	if (_source == NULL || _target == NULL)
	{
		qDebug() << "AssignButtonMenu::buttonClicked() : source or target not set";
		return;
	}
}

void AssignButtonMenu::sourceChanged()
{
	if (_target == NULL)
	{
		qDebug() << "AssignButtonMenu::sourceChanged() : target not set";
		return;
	}

	if (_source->hasSelection())
	{
		QMimeData *mimeData = _source->view()->model()->mimeData(_source->view()->selectionModel()->selectedIndexes());
		bool canAssign = _target->view()->model()->canDropMimeData(mimeData, Qt::MoveAction, -1, 0, QModelIndex());
		this->setEnabled(canAssign);
	}
	else
	{
		setEnabled(false);
	}
}

void AssignButtonMenu::targetChanged()
{
	setEnabled(false);
}



