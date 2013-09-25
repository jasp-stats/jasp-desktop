#ifndef DRAGANDDROP_H
#define DRAGANDDROP_H

#include <QAbstractItemView>

#include "droptarget.h"
#include "enhanceddroptarget.h"

class DragAndDrop
{
public:
	static void perform(DropTarget *source, DropTarget *target, int flags = 0);
};

#endif // DRAGANDDROP_H
