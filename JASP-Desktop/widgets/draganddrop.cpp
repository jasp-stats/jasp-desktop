
#include "draganddrop.h"

void DragAndDrop::perform(QAbstractItemView *source, QAbstractItemView *target)
{
	QModelIndexList indices = source->selectionModel()->selectedIndexes();
	QMimeData *mimeData = source->model()->mimeData(indices);
	Qt::DropAction action = Qt::CopyAction;

	if (target->model()->supportedDropActions() & Qt::MoveAction)
		action = Qt::MoveAction;

	if (target->model()->dropMimeData(mimeData, action, -1, 0, QModelIndex()))
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
