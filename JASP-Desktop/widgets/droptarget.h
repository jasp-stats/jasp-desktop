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

#ifndef DROPTARGET_H
#define DROPTARGET_H

#include <boost/signals2.hpp>
#include <QMimeData>
#include <QAbstractItemView>
#include <QAbstractItemModel>

class DropTarget
{
protected:
	DropTarget()
	{
		_view = NULL;
	}

public:
	boost::signals2::signal<void ()> focused;
	boost::signals2::signal<void ()> selectionUpdated;

	QAbstractItemView *view()
	{
		if (_view == NULL)
			_view = dynamic_cast<QAbstractItemView*>(this);
		return _view;
	}

	bool hasSelection()
	{
		QAbstractItemView *v = view();
		if (v != NULL && v->selectionModel() != NULL)
			return v->selectionModel()->selectedIndexes().length() > 0;
		else
			return false;
	}

	virtual void notifyDragWasDropped() { }

	QAbstractItemView *_view;
};

#endif // DROPTARGET_H
