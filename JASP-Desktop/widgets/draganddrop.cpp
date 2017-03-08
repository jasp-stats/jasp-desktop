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

