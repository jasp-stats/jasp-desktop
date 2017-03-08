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

#include "assignbutton.h"

#include <QMimeData>
#include <QDebug>

#include "draganddrop.h"

AssignButton::AssignButton(QWidget *parent) :
	Button(parent)
{
	_source = NULL;
	_target = NULL;

	connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));

	setAssignDirection(true);
}

void AssignButton::setAssignDirection(bool assign)
{
	_assignDirection = assign;

	if (assign)
		setIcon(QIcon(":/images/arrow-right.png"));
	else
		setIcon(QIcon(":/images/arrow-left.png"));
}

bool AssignButton::isAssign()
{
	return _assignDirection;
}

void AssignButton::setSourceAndTarget(DropTarget *source, DropTarget *target)
{
	this->setEnabled(false);

	_source = source;
	_target = target;

	_source->selectionUpdated.connect(boost::bind(&AssignButton::sourceChanged, this));
	_source->focused.connect(boost::bind(&AssignButton::sourceChanged, this));

	_target->selectionUpdated.connect(boost::bind(&AssignButton::targetChanged, this));
	_target->focused.connect(boost::bind(&AssignButton::targetChanged, this));
}

void AssignButton::buttonClicked()
{
	if (_source == NULL || _target == NULL)
	{
		qDebug() << "AssignButton::buttonClicked() : source and target not set";
		return;
	}

	if (_assignDirection)
		DragAndDrop::perform(_source, _target);
	else
		DragAndDrop::perform(_target, _source);
}

void AssignButton::sourceChanged()
{	
	setAssignDirection(true);

	if (_target == NULL)
	{
		qDebug() << "AssignButton::sourceChanged() : target not set";
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
		this->setEnabled(false);
	}
}

void AssignButton::targetChanged()
{	
	setAssignDirection(false);

	if (_source == NULL)
	{
		qDebug() << "AssignButton::targetChanged() : source not set";
		return;
	}

	if (_target->hasSelection())
	{
		QMimeData *mimeData = _target->view()->model()->mimeData(_target->view()->selectionModel()->selectedIndexes());
		bool canAssign = _source->view()->model()->canDropMimeData(mimeData, Qt::MoveAction, -1, 0, QModelIndex());
		this->setEnabled(canAssign);
	}
	else
	{
		this->setEnabled(false);
	}
}



