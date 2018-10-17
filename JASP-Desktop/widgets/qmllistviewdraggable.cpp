//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "qmllistviewdraggable.h"
#include "listmodeldraggable.h"
#include "analysis/analysisqmlform.h"
#include <QQuickItem>
#include <QQmlProperty>
#include <QTimer>

QMLListViewDraggable::QMLListViewDraggable(QQuickItem* item, AnalysisQMLForm* form)
	: QMLListView(item, form)
{
}

void QMLListViewDraggable::setUp()
{
	QMLListView::setUp();
	
	_draggableModel = dynamic_cast<ListModelDraggable*>(model());
	_draggableModel->setItemType(QQmlProperty(_item, "itemType").read().toString());
	_draggableModel->setTermsAreVariables(QQmlProperty(_item, "showVariableIcon").read().toBool());
	QString dropMode = QQmlProperty(_item, "dropMode").read().toString();
	if (dropMode.isEmpty()) dropMode = "None";
	_draggableModel->setDropMode(qmlDropModeFromQString(dropMode));
	
	QQuickItem::connect(_item, SIGNAL(itemDoubleClicked(int)), this, SLOT(itemDoubleClickedHandler(int)));	
	QQuickItem::connect(_item, SIGNAL(itemsDropped(QVariant, QVariant, int)), this, SLOT(itemsDroppedHandler(QVariant, QVariant, int)));	
}

void QMLListViewDraggable::itemDoubleClickedHandler(int index)
{
	ListModel *targetModel = _form->getRelatedModel(this);
	
	if (!targetModel)
	{
		addError(QString::fromLatin1("No related list found for VariablesList ") + name());
		return;
	}
	
	ListModelDraggable *draggableTargetModel = dynamic_cast<ListModelDraggable*>(targetModel);
	if (!draggableTargetModel)
	{
		addError(QString::fromLatin1("Wrong kind of related list (") + targetModel->name() + QString::fromLatin1(") found for VariablesList ") + name());
		return;
	}
	
	QList<int> indexes;
	indexes.push_back(index);
	_moveItems(indexes, draggableTargetModel, -1);
}

void QMLListViewDraggable::itemsDroppedHandler(QVariant vindexes, QVariant vdropList, int dropItemIndex)
{
	QQuickItem* dropList = qobject_cast<QQuickItem*>(vdropList.value<QObject*>());
	ListModelDraggable* dropModel = NULL;
	
	if (!dropList)
		dropModel = dynamic_cast<ListModelDraggable*>(_form->getRelatedModel(this));
	else
	{
		QVariant vdropModel = QQmlProperty(dropList, "model").read();
		dropModel = qobject_cast<ListModelDraggable*>(vdropModel.value<QObject*>());
	}
	
	if (!dropModel)
	{
		qDebug() << "No drop element!";
		return;
	}
	
	QList<QVariant> vvindexes = vindexes.toList();
	_tempIndexes.clear();
	for (QVariant &index : vvindexes)
		_tempIndexes.push_back(index.toInt());
	
	_tempDropModel = dropModel;
	_tempDropItemIndex = dropItemIndex;
	// We need to move the items with another thread, if not, the drag and drop in QML get confused:
	// the call to itemsDropped is called from an item that will be removed (the items of the variable list
	// will be re-created). So itemsDropped should not call _moveItems directly.
	QTimer::singleShot(0, this, SLOT(moveItemsDelayedHandler()));
}

void QMLListViewDraggable::moveItemsDelayedHandler()
{
	_moveItems(_tempIndexes, _tempDropModel, _tempDropItemIndex);
}

void QMLListViewDraggable::_moveItems(QList<int> &indexes, ListModelDraggable* targetModel, int dropItemIndex)
{
	if (targetModel && indexes.size() > 0)
	{
		Options* options = _form->getAnalysisOptions();
		if (options != NULL)
			options->blockSignals(true);
		
		ListModelDraggable* sourceModel = draggableModel();
		if (sourceModel == targetModel)
			sourceModel->moveTerms(indexes, dropItemIndex);
		else
		{
			bool success = true;		
			Terms* removedTermsWhenDropping = NULL;
			if (!sourceModel->copyTermsWhenDropped() && targetModel->removeTermsWhenDragged())
			{
				Terms* terms = sourceModel->termsFromIndexes(indexes);
				success = targetModel->canAddTerms(terms);
				if (success)
					removedTermsWhenDropping = targetModel->addTerms(terms, dropItemIndex);
				delete terms;
			}
				
			if (success && !targetModel->copyTermsWhenDropped() && sourceModel->removeTermsWhenDragged())
			{
				sourceModel->removeTerms(indexes);
				if (removedTermsWhenDropping)
				{
					if (removedTermsWhenDropping->size() > 0)
						sourceModel->addTerms(removedTermsWhenDropping);
					delete removedTermsWhenDropping;
				}
			}
			else
				sourceModel->refresh();
		}
		
		if (options != NULL)
			options->blockSignals(false);	
	}
	else
	{
		qDebug() << (!targetModel ? "no dropModel" : "no indexes");
	}
}

void QMLListViewDraggable::setDropMode(qmlDropMode dropMode)
{
	dynamic_cast<ListModelDraggable*>(model())->setDropMode(dropMode);
	QString dropModeStr = qmlDropModeToQString(dropMode);
	QQmlProperty::write(_item, "dropMode",dropModeStr);			
}
