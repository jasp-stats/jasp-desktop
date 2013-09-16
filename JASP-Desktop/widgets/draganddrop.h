#ifndef DRAGANDDROP_H
#define DRAGANDDROP_H

#include <QAbstractItemView>

class DragAndDrop
{
public:
	static void perform(QAbstractItemView *source, QAbstractItemView *target);
};

#endif // DRAGANDDROP_H
