#ifndef ENHANCEDDROPTARGET_H
#define ENHANCEDDROPTARGET_H

#include <QMimeData>

class EnhancedDropTarget
{
public:
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int flags) = 0;
};

#endif // ENHANCEDDROPTARGET_H
