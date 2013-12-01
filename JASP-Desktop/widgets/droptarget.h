#ifndef DROPTARGET_H
#define DROPTARGET_H

#include <boost/signals2.hpp>
#include <QMimeData>
#include <QAbstractItemView>
#include <QAbstractItemModel>
#include <QDebug>

class DropTarget
{
protected:
	DropTarget() { _view = NULL; }

public:
	boost::signals2::signal<void ()> focused;
	boost::signals2::signal<void ()> selectionUpdated;

	QAbstractItemView *view() { if (_view == NULL) _view = dynamic_cast<QAbstractItemView*>(this); return _view; }
	bool hasSelection() { return view()->selectionModel()->selectedIndexes().length() > 0; }

	virtual void notifyDragWasDropped() { }

	QAbstractItemView *_view;
};

#endif // DROPTARGET_H
