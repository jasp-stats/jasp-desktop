#ifndef DRAGANDDROP_H
#define DRAGANDDROP_H

#include <QAbstractItemView>

#include "enhanceddroptarget.h"

class DragAndDrop
{
public:
	static void perform(QAbstractItemView *source, QAbstractItemView *target, int flags = 0);
};

#endif // DRAGANDDROP_H
