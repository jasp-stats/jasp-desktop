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

#include "variableslistbase.h"
#include "listmodeltermsavailable.h"
#include "listmodelinteractionavailable.h"
#include "listmodeltermsassigned.h"
#include "listmodelmeasurescellsassigned.h"
#include "listmodelinteractionassigned.h"
#include "listmodellayersassigned.h"
#include "listmodelmultitermsassigned.h"
#include "boundcontrolmeasurescells.h"
#include "boundcontrollayers.h"
#include "boundcontrolterms.h"
#include "boundcontrolmultiterms.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrol.h"
#include "sourceitem.h"
#include <QTimer>
#include <QQmlProperty>
#include "log.h"

VariablesListBase::VariablesListBase(QQuickItem* parent)
	: JASPListControl(parent)
{
	_controlType			= ControlType::VariablesListView;
	_useControlMouseArea	= false;
}

void VariablesListBase::setUp()
{
	JASPListControl::setUp();

	if (listViewType() == ListViewType::RepeatedMeasures)
	{
		for (SourceItem* sourceItem : _sourceItems)
		{
			ListModelRepeatedMeasuresFactors* factorsModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(sourceItem->model());
			if (!factorsModel)
				addControlError(tr("Source model of %1 must be from a Factor List").arg(name()));
			addDependency(factorsModel->listView());
			BoundControlMeasuresCells* measuresCellsControl = dynamic_cast<BoundControlMeasuresCells*>(_boundControl);
			measuresCellsControl->addFactorModel(factorsModel);
		}
	}

	ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(_draggableModel);

	if (assignedModel)
	{
		ListModel* relatedModel = form()->getRelatedModel(this);

		if (!relatedModel)
		{
			if (sourceItems().empty() && !property("debug").toBool())
				addControlError(tr("Cannot find source for VariableList %1").arg(name()));
		}
		else
		{
			ListModelAvailableInterface* availableModel = dynamic_cast<ListModelAvailableInterface*>(relatedModel);
			if (!availableModel)
				addControlError(tr("Wrong kind of source for VariableList %1").arg(name()));
			else
			{
				assignedModel->setAvailableModel(availableModel);
				availableModel->addAssignedModel(assignedModel);
				addDependency(availableModel->listView());
				connect(availableModel, &ListModelAvailableInterface::allAvailableTermsChanged, assignedModel, &ListModelAssignedInterface::availableTermsChanged);
			}
		}
	}
	else
	{
		SortMenuModel* sortedMenuModel = new SortMenuModel(_draggableModel, {Sortable::None, Sortable::SortByName, Sortable::SortByType});
		setProperty("sortMenuModel", QVariant::fromValue(sortedMenuModel));
	}

	_draggableModel->setItemType(property("itemType").toString());
	_draggableModel->setTermsAreVariables(property("showVariableTypeIcon").toBool());
	JASPControl::DropMode dropMode = JASPControl::DropMode(property("dropMode").toInt());
	_draggableModel->setDropMode(dropMode);
	
	QQuickItem::connect(this, SIGNAL(itemDoubleClicked(int)),							this, SLOT(itemDoubleClickedHandler(int)));
	QQuickItem::connect(this, SIGNAL(itemsDropped(QVariant, QVariant, int, int)),		this, SLOT(itemsDroppedHandler(QVariant, QVariant, int, int)));
}

void VariablesListBase::setUpModel()
{
	switch (_listViewType)
	{
	case ListViewType::AvailableVariables:
	{
		_isBound = false;
		_draggableModel = new ListModelTermsAvailable(this);
		break;
	}
	case ListViewType::AvailableInteraction:
	{
		_isBound = false;
		_draggableModel = new ListModelInteractionAvailable(this);
		break;
	}
	case ListViewType::Layers:
	{
		ListModelLayersAssigned* layersModel = new ListModelLayersAssigned(this);
		_boundControl = new BoundControlLayers(layersModel);
		_draggableModel = layersModel;
		break;
	}
	case ListViewType::RepeatedMeasures:
	{
		 ListModelMeasuresCellsAssigned* measuresCellsModel = new ListModelMeasuresCellsAssigned(this);
		_boundControl	= new BoundControlMeasuresCells(measuresCellsModel);
		_draggableModel = measuresCellsModel;
		break;
	}
	case ListViewType::AssignedVariables:
	{
		ListModelAssignedInterface* termsModel = nullptr;

		if (columns() > 1)
		{
			ListModelMultiTermsAssigned* multiTermsModel = new ListModelMultiTermsAssigned(this, columns());
			_boundControl = new BoundControlMultiTerms(multiTermsModel);
			_draggableModel = multiTermsModel;
		}
		else
		{
			int maxRows = property("maxRows").toInt();

			termsModel = new ListModelTermsAssigned(this, maxRows);
			_boundControl	= new BoundControlTerms(termsModel, maxRows == 1);
			_draggableModel = termsModel;
		}
		break;
	}
	case ListViewType::Interaction:
	{
		bool interactionContainLowerTerms	= property("interactionContainLowerTerms").toBool();
		bool addInteractionsByDefault		= property("addInteractionsByDefault").toBool();

		ListModelInteractionAssigned* termsModel = new ListModelInteractionAssigned(this, interactionContainLowerTerms, addInteractionsByDefault);
		_boundControl	= new BoundControlTerms(termsModel);
		_draggableModel = termsModel;
		break;
	}
	}

	JASPListControl::setUpModel();
}

void VariablesListBase::itemDoubleClickedHandler(int index)
{
	ListModel *targetModel = form()->getRelatedModel(this);
	
	if (!targetModel)
	{
		addControlError(tr("No related list found for VariablesList %1").arg(name()));
		return;
	}
	
	ListModelDraggable *draggableTargetModel = dynamic_cast<ListModelDraggable*>(targetModel);
	if (!draggableTargetModel)
	{
		addControlError(tr("Wrong kind of related list (%1) found for VariablesList %2").arg(targetModel->name()).arg(name()));
		return;
	}
	
	QList<int> indexes;
	indexes.push_back(index);
	moveItems(indexes, draggableTargetModel);
}

void VariablesListBase::itemsDroppedHandler(QVariant vindexes, QVariant vdropList, int dropItemIndex, int assignOption)
{
	QQuickItem* dropList = qobject_cast<QQuickItem*>(vdropList.value<QObject*>());
	ListModelDraggable* dropModel = nullptr;
	
	if (!dropList)
		dropModel = dynamic_cast<ListModelDraggable*>(form()->getRelatedModel(this));
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
	if (!vvindexes.empty())
	{
		_tempIndexes.clear();
		for (QVariant &index : vvindexes)
			_tempIndexes.push_back(index.toInt());
	}
	else
		_tempIndexes = vindexes.value<QList<int> >();
	
	_tempDropModel = dropModel;
	_tempDropItemIndex = dropItemIndex;
	_tempAssignOption = JASPControl::AssignType(assignOption);
	// the call to itemsDropped is called from an item that will be removed (the items of the variable list
	// will be re-created). So itemsDropped should not call _moveItems directly.
	QTimer::singleShot(0, this, SLOT(moveItemsDelayedHandler()));
}

void VariablesListBase::moveItemsDelayedHandler()
{
	moveItems(_tempIndexes, _tempDropModel, _tempDropItemIndex, _tempAssignOption);
}

void VariablesListBase::moveItems(QList<int> &indexes, ListModelDraggable* targetModel, int dropItemIndex, JASPControl::AssignType assignOption)
{
	if (targetModel && indexes.size() > 0)
	{
		std::sort(indexes.begin(), indexes.end());
		Options* options = form()->getAnalysisOptions();
		if (options != nullptr)
			options->blockSignals(true);
		
		ListModelDraggable* sourceModel = _draggableModel;
		if (sourceModel == targetModel)
			sourceModel->moveTerms(indexes, dropItemIndex);
		else
		{
			bool refreshSource = false;
			Terms termsAdded;
			Terms removedTermsWhenAdding;
			QList<int> indexAdded = indexes;

			if (!sourceModel->copyTermsWhenDropped())
			{
				Terms terms = sourceModel->termsFromIndexes(indexes);
				if (terms.size() == 0)
					Log::log() << "No terms found when trying to move them" << std::endl;

				termsAdded = targetModel->canAddTerms(terms);

				if (termsAdded.size() > 0)
					removedTermsWhenAdding = targetModel->addTerms(termsAdded, dropItemIndex, assignOption);

				if (termsAdded.size() != terms.size())
				{
					indexAdded.clear();
					for (int i = 0; i < indexes.size(); i++)
					{
						int index = indexes[i];
						if (i < int(terms.size()))
						{
							const Term& term = terms[size_t(i)];
							if (termsAdded.contains(term))
								indexAdded.append(index);
						}
					}
					refreshSource = true;
				}
			}
				
			if (!targetModel->copyTermsWhenDropped())
			{
				if (indexAdded.size() > 0)
				{
					sourceModel->removeTerms(indexAdded);
					refreshSource = false;
				}
				if (removedTermsWhenAdding.size() > 0)
				{
					sourceModel->addTerms(removedTermsWhenAdding);
					refreshSource = false;
				}
			}

			if (refreshSource)
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

void VariablesListBase::termsChangedHandler()
{
	if (_boundControl)	_boundControl->updateOption();
	else JASPListControl::termsChangedHandler();
}
