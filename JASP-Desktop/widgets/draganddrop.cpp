
#include "draganddrop.h"

void DragAndDrop::perform(QAbstractItemView *source, QAbstractItemView *target, int flags)
{
	QModelIndexList indices = source->selectionModel()->selectedIndexes();
	QMimeData *mimeData = source->model()->mimeData(indices);
	Qt::DropAction action = Qt::CopyAction;

	if (target->model()->supportedDropActions() & Qt::MoveAction)
		action = Qt::MoveAction;

	QAbstractItemModel *model = target->model();
	EnhancedDropTarget *enhancedModel = dynamic_cast<EnhancedDropTarget *>(model);

	bool success;

	if (enhancedModel != NULL)
		success = enhancedModel->dropMimeData(mimeData, action, -1, 0, QModelIndex(), flags);
	else
		success = model->dropMimeData(mimeData, action, -1, 0, QModelIndex());

	if (success)
	{
		if (action == Qt::MoveAction)
		{
			while (indices.length() > 0)
			{
				QModelIndex lowest;

				foreach (QModelIndex index, indices)
				{
					if (index.row() > lowest.row())
						lowest = index;
				}

				foreach (QModelIndex index, indices)
				{
					if (index.row() == lowest.row())
						indices.removeOne(index);
				}

				source->model()->removeRows(lowest.row(), 1);
			}
		}
	}
}

