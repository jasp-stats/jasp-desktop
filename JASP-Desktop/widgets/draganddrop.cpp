
#include "draganddrop.h"

void DragAndDrop::perform(DropTarget *source, DropTarget *target, int flags)
{
	QModelIndexList indices = source->view()->selectionModel()->selectedIndexes();
	QMimeData *mimeData = source->view()->model()->mimeData(indices);
	Qt::DropAction action = Qt::CopyAction;

	if (target->view()->model()->supportedDropActions() & Qt::MoveAction)
		action = Qt::MoveAction;

	QAbstractItemModel *model = target->view()->model();
	EnhancedDropTarget *enhancedModel = dynamic_cast<EnhancedDropTarget *>(model);

	bool success;

	if (enhancedModel != NULL)
		success = enhancedModel->dropMimeData(mimeData, action, -1, 0, QModelIndex(), flags);
	else
		success = model->dropMimeData(mimeData, action, -1, 0, QModelIndex());

	if (success && action == Qt::MoveAction && source != target)
		source->notifyDragWasDropped();

}

