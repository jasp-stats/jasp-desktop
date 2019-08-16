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
#include "listmodelassignedinterface.h"
#include "../analysis/analysisform.h"
#include <QQuickItem>
#include <QQmlProperty>
#include <QTimer>
#include "log.h"

QMLListViewDraggable::QMLListViewDraggable(QQuickItem* item, AnalysisForm* form)
	: QMLListView(item, form)
{
}

void QMLListViewDraggable::setUp()
{
	QMLListView::setUp();
	
	_draggableModel = dynamic_cast<ListModelDraggable*>(model());
	_draggableModel->setItemType(QQmlProperty(_item, "itemType").read().toString());
	_draggableModel->setTermsAreVariables(QQmlProperty(_item, "showVariableTypeIcon").read().toBool());
	QString dropMode = QQmlProperty(_item, "dropMode").read().toString();
	if (dropMode.isEmpty()) dropMode = "None";
	_draggableModel->setDropMode(qmlDropModeFromQString(dropMode));
	
	QQuickItem::connect(_item, SIGNAL(itemDoubleClicked(int)), this, SLOT(itemDoubleClickedHandler(int)));	
	QQuickItem::connect(_item, SIGNAL(itemsDropped(QVariant, QVariant, int, QString)), this, SLOT(itemsDroppedHandler(QVariant, QVariant, int, QString)));	
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
	moveItems(indexes, draggableTargetModel);
}

void QMLListViewDraggable::itemsDroppedHandler(QVariant vindexes, QVariant vdropList, int dropItemIndex, QString assignOption)
{
	QQuickItem* dropList = qobject_cast<QQuickItem*>(vdropList.value<QObject*>());
	ListModelDraggable* dropModel = nullptr;
	
	if (!dropList)
		dropModel = dynamic_cast<ListModelDraggable*>(_form->getRelatedModel(this));
	else
	{
		QVariant vdropModel = QQmlProperty(dropList, "model").read();
		dropModel = qobject_cast<ListModelDraggable*>(vdropModel.value<QObject*>());
	}
	
	if (!dropModel)
	{
		Log::log()  << "No drop element!" << std::endl;
		return;
	}
	
	QList<QVariant> vvindexes = vindexes.toList();
	_tempIndexes.clear();
	for (QVariant &index : vvindexes)
		_tempIndexes.push_back(index.toInt());
	
	_tempDropModel = dropModel;
	_tempDropItemIndex = dropItemIndex;
	_tempAssignOption = assignOption;
	// the call to itemsDropped is called from an item that will be removed (the items of the variable list
	// will be re-created). So itemsDropped should not call _moveItems directly.
	QTimer::singleShot(0, this, SLOT(moveItemsDelayedHandler()));
}

void QMLListViewDraggable::moveItemsDelayedHandler()
{
	moveItems(_tempIndexes, _tempDropModel, _tempDropItemIndex, _tempAssignOption);
}

void QMLListViewDraggable::moveItems(QList<int> &indexes, ListModelDraggable* targetModel, int dropItemIndex, const QString& assignOption)
{
	if (targetModel && indexes.size() > 0)
	{
		std::sort(indexes.begin(), indexes.end());
		Options* options = _form->getAnalysisOptions();
		if (options != nullptr)
			options->blockSignals(true);
		
		ListModelDraggable* sourceModel = draggableModel();
		if (sourceModel == targetModel)
			sourceModel->moveTerms(indexes, dropItemIndex);
		else
		{
			bool success = true;		
			Terms* removedTermsWhenDropping = nullptr;
			if (!sourceModel->copyTermsWhenDropped() && targetModel->removeTermsWhenDragged())
			{
				Terms* terms = sourceModel->termsFromIndexes(indexes);
				if (terms == nullptr || terms->size() == 0)
				{
					Log::log() << "No terms found when trying to moving them" << std::endl;
					if (terms)
						delete terms;
					return;
				}
				success = targetModel->canAddTerms(terms);
				if (success)
					removedTermsWhenDropping = targetModel->addTerms(terms, dropItemIndex, assignOption);
				delete terms;
			}
				
			if (success && !targetModel->copyTermsWhenDropped() && sourceModel->removeTermsWhenDragged())
			{
				sourceModel->removeTerms(indexes);
				if (removedTermsWhenDropping && removedTermsWhenDropping->size() > 0)
				{
					if (sourceModel->canAddTerms(removedTermsWhenDropping))
						sourceModel->addTerms(removedTermsWhenDropping);
					else
					{
						// Strange situation: the target has added the terms, but the source cannot add the terms sent back by the target.
						// We try to find the available model and to add these terms there.
						ListModelAssignedInterface* assignedModel = dynamic_cast<ListModelAssignedInterface*>(targetModel);
						if (assignedModel)
							assignedModel->source()->addTerms(removedTermsWhenDropping);
					}

					delete removedTermsWhenDropping;
				}
			}
			else
				sourceModel->refresh();
		}
		
		if (options != nullptr)
			options->blockSignals(false);	
	}
	else
	{
		Log::log()  << (!targetModel ? "no dropModel" : "no indexes") << std::endl;
	}
}

void QMLListViewDraggable::setDropMode(qmlDropMode dropMode)
{
	dynamic_cast<ListModelDraggable*>(model())->setDropMode(dropMode);
	QString dropModeStr = qmlDropModeToQString(dropMode);
	QQmlProperty::write(_item, "dropMode",dropModeStr);			
}
