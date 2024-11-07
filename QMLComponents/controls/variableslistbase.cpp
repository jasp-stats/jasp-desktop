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
#include "checkboxbase.h"
#include "models/listmodeltermsavailable.h"
#include "models/listmodelinteractionavailable.h"
#include "models/listmodeltermsassigned.h"
#include "models/listmodelmeasurescellsassigned.h"
#include "models/listmodelinteractionassigned.h"
#include "models/listmodellayersassigned.h"
#include "models/listmodelmultitermsassigned.h"
#include "boundcontrols/boundcontrolmeasurescells.h"
#include "boundcontrols/boundcontrollayers.h"
#include "boundcontrols/boundcontrolterms.h"
#include "boundcontrols/boundcontrolmultiterms.h"
#include "utilities/desktopcommunicator.h"
#include "rowcontrols.h"
#include "analysisform.h"
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
			ListModelFactorLevels* factorsModel = dynamic_cast<ListModelFactorLevels*>(sourceItem->sourceListModel());
			if (!factorsModel)
				addControlError(tr("Source model of %1 must be from a Factor List").arg(name()));
			else
			{
				addDependency(factorsModel->listView());
				BoundControlMeasuresCells* measuresCellsControl = dynamic_cast<BoundControlMeasuresCells*>(_boundControl);
				measuresCellsControl->addFactorModel(factorsModel);
			}
		}
	}

	_setRelations();

	ListModelAvailableInterface* availableModel = qobject_cast<ListModelAvailableInterface*>(_draggableModel);

	if (availableModel)
	{
		SortMenuModel* sortedMenuModel = new SortMenuModel(_draggableModel, {Sortable::None, Sortable::SortByName, Sortable::SortByType});
		setProperty("sortMenuModel", QVariant::fromValue(sortedMenuModel));
	}

	_draggableModel->setItemType(property("itemType").toString());
	JASPControl::DropMode dropMode = JASPControl::DropMode(property("dropMode").toInt());
	_draggableModel->setDropMode(dropMode);

	_mayUseFormula = (_columns == 1);
	
	//We use macros here because the signals come from QML
	QQuickItem::connect(this,	SIGNAL(itemDoubleClicked(int)),						this, SLOT(itemDoubleClickedHandler(int))					);
	QQuickItem::connect(this,	SIGNAL(itemsDropped(QVariant, QVariant, int)),		this, SLOT(itemsDroppedHandler(QVariant, QVariant, int))	);
	connect(this,				&VariablesListBase::columnsChanged,					this, [&]() { _mayUseFormula = (_columns == 1); }				);
}

void VariablesListBase::_setInitialized(const Json::Value &value)
{
	if (value == Json::nullValue && addAvailableVariablesToAssigned())
	{
		// If addAvailableVariablesToAssigned is true and this is initialized without value,
		// maybe the availableAssignedList has some default values that must be assigned to this VariablesList
		ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(_draggableModel);
		if (assignedModel && assignedModel->availableModel())
			assignedModel->initTerms(assignedModel->availableModel()->terms());
	}

	JASPListControl::_setInitialized(value);
}


ListModel *VariablesListBase::model() const
{
	return _draggableModel;
}

void VariablesListBase::setUpModel()
{
	switch (_listViewType)
	{
	case ListViewType::AvailableVariables:
		_isBound		= false;
		_draggableModel = new ListModelTermsAvailable(this);
		break;

	case ListViewType::AvailableInteraction:
		_isBound				= false;
		_termsAreInteractions	= true;
		_draggableModel			= new ListModelInteractionAvailable(this);
		break;

	case ListViewType::Layers:
	{
		auto *	layersModel		= new ListModelLayersAssigned(this);
				_boundControl	= new BoundControlLayers(layersModel);
				_draggableModel = layersModel;
				_useTermsInRSyntax = false;
		break;
	}
		
	case ListViewType::RepeatedMeasures:
	{
		 auto * measuresCellsModel	= new ListModelMeasuresCellsAssigned(this);
				_boundControl		= new BoundControlMeasuresCells(measuresCellsModel);
				_draggableModel		= measuresCellsModel;
		break;
	}
		
	case ListViewType::AssignedVariables:
	{
		ListModelAssignedInterface* termsModel = nullptr;

		if (columns() > 1)
		{
			auto *	multiTermsModel = new ListModelMultiTermsAssigned(this, columns());
					_boundControl	= new BoundControlMultiTerms(multiTermsModel);
					_draggableModel = multiTermsModel;
		}
		else
		{
			termsModel		= new ListModelTermsAssigned(this);
			_boundControl	= new BoundControlTerms(termsModel, _maxRows == 1);
			_draggableModel = termsModel;
		}
		break;
	}
		
	case ListViewType::Interaction:
	{
		_termsAreInteractions = true;

		bool	interactionContainLowerTerms	= property("interactionContainLowerTerms").toBool(),
				addInteractionsByDefault		= property("addInteractionsByDefault").toBool();

		auto *	termsModel		= new ListModelInteractionAssigned(this, interactionContainLowerTerms, addInteractionsByDefault);
				_boundControl	= new BoundControlTerms(termsModel);
				_draggableModel = termsModel;
		break;
	}
		
	}

	JASPListControl::setUpModel();
}

bool VariablesListBase::addRowControl(const QString &key, JASPControl *control)
{
	bool result = JASPListControl::addRowControl(key, control);

	if (result && !_interactionHighOrderCheckBox.isEmpty() && _interactionHighOrderCheckBox == control->name())
		connect(control, &JASPControl::boundValueChanged, this, &VariablesListBase::interactionHighOrderHandler);

	return result;
}

void VariablesListBase::itemDoubleClickedHandler(int index)
{
	ListModel *targetModel = getRelatedModel();
	
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

void VariablesListBase::itemsDroppedHandler(QVariant vindexes, QVariant vdropList, int dropItemIndex)
{
	JASPListControl		* dropList  = qobject_cast<JASPListControl*>(vdropList.value<QObject*>());
	ListModelDraggable	* dropModel = !dropList	? qobject_cast<ListModelDraggable*>(getRelatedModel())
												: qobject_cast<ListModelDraggable*>(dropList->model());

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
	
	_tempDropModel		= dropModel;
	_tempDropItemIndex	= dropItemIndex;
	// the call to itemsDropped is called from an item that will be removed (the items of the variable list
	// will be re-created). So itemsDropped should not call _moveItems directly.
	QTimer::singleShot(0, this, SLOT(moveItemsDelayedHandler()));
}

void VariablesListBase::moveItemsDelayedHandler()
{
	moveItems(_tempIndexes, _tempDropModel, _tempDropItemIndex);
}

void VariablesListBase::moveItems(QList<int> &indexes, ListModelDraggable* targetModel, int dropItemIndex)
{
	if (!targetModel || !indexes.size()) return;

	if (form()) form()->blockValueChangeSignal(true);

	std::sort(indexes.begin(), indexes.end());
	ListModelDraggable* sourceModel = _draggableModel;
	if (sourceModel == targetModel)
		sourceModel->moveTerms(indexes, dropItemIndex);
	else
	{
		Terms	termsToAdd	= sourceModel->termsFromIndexes(indexes),
				termsAllowedToBeAdded = termsToAdd,
				termsRejected;

		//if a model keeps terms we dont need to bother adding or removing anything
		if (!targetModel->keepTerms())
		{
			termsAllowedToBeAdded	=	targetModel->canAddTerms(	termsToAdd															);	// Check which terms can be added in the target model.
			if (termsAllowedToBeAdded != termsToAdd) indexes	=	sourceModel->indexesFromTerms(termsAllowedToBeAdded					);	// If not all terms can be added, recompute the indexes, but keep the original indexes otherwise: when the same term can exist several times in the sourceModel, the original indexes will give the right term to move.
		}
		if (!sourceModel->keepTerms())								sourceModel->removeTerms(	indexes									);	// Then remove the terms in the source model. This must be done before adding them in the target model: for nested FactorsForm, it is important that the term is first removed from the source and afterwards added to the target.
		if (!targetModel->keepTerms())	termsRejected			=	targetModel->addTerms(		termsAllowedToBeAdded, dropItemIndex	);	// Add the terms in the target model
		if (!sourceModel->keepTerms())								sourceModel->addTerms(		termsRejected							);	// Any possible overflow (such as for single-variable-list) gets returned to the source
		
	}

	if (form()) form()->blockValueChangeSignal(false);
}

void VariablesListBase::setDropKeys(const QStringList &dropKeys)
{
	Log::log() << "LOG setDropKeys " << name() << ": " << dropKeys.join('/') << std::endl;
	if (dropKeys != _dropKeys)
	{
		_dropKeys = dropKeys;
		_setRelations();
		emit dropKeysChanged();
	}

}

ListModel *VariablesListBase::getRelatedModel()
{
	ListModel* result = nullptr;
	if (dropKeys().count() > 0)
	{
		QString relatedName = dropKeys()[0]; // The first key gives the default drop item.
		if (_parentListView)
		{
			JASPListControl* relatedControl = qobject_cast<JASPListControl*>(_parentListView->model()->getRowControl(_parentListViewKey, relatedName));
			if (relatedControl)
				result = relatedControl->model();
		}
		if (!result && form())	result = form()->getModel(relatedName);
	}

	return result;
}

void VariablesListBase::setVariableType(int index, int type)
{
	model()->setVariableType(index, columnType(type));
}


void VariablesListBase::termsChangedHandler()
{
	JASPListControl::termsChangedHandler();

	if (_boundControl)	_boundControl->resetBoundValue();
}

void VariablesListBase::_setRelations()
{
	ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(_draggableModel);
	if (assignedModel)
	{
		ListModel* relatedModel = getRelatedModel();
		if (relatedModel)
		{
			ListModelAvailableInterface* availableModel = dynamic_cast<ListModelAvailableInterface*>(relatedModel);
			if (!availableModel)
				addControlError(tr("Wrong kind of source for VariableList %1").arg(name()));
			else
			{
				assignedModel->setAvailableModel(availableModel);
				availableModel->addAssignedModel(assignedModel);
				addDependency(availableModel->listView());
				setContainsVariables();
				setContainsInteractions();

				// When the assigned model is of type interaction or it has multiple columns, then the available model should keep its terms when they are moved to the assigned model
				if (_listViewType == ListViewType::Interaction || (columns() > 1 && _listViewType != ListViewType::RepeatedMeasures))
				{
					VariablesListBase* listControl = qobject_cast<VariablesListBase*>(availableModel->listView());
					if (listControl)
						listControl->setKeepVariablesWhenMoved(true);
				}
			}
		}
	}
}

void VariablesListBase::interactionHighOrderHandler(JASPControl* checkBoxControl)
{
	CheckBoxBase* checkBox = qobject_cast<CheckBoxBase*>(checkBoxControl);
	if (checkBox == nullptr)
	{
		Log::log() << "interactionHighOrderHandler is called with a control that is not a CheckBox!" << std::endl;
		return;
	}

	bool checked = checkBox->checked();
	if (form()) form()->blockValueChangeSignal(true);

	// if a higher order interaction is specified as nuisance, then all lower order terms should be changed to nuisance as well
	Term keyTerm = Term::readTerm(checkBoxControl->parentListViewKey());
	for (const Term& otherTerm : _draggableModel->terms())
	{
		if (otherTerm == keyTerm)
			continue;

		RowControls* rowControls = _draggableModel->getRowControls(otherTerm.asQString());
		if (!rowControls) continue; // Apparently the controls are not created yet for this row. Does not matter: this function will be called when they are created
		CheckBoxBase* otherCheckBox = qobject_cast<CheckBoxBase*>(rowControls->getJASPControl(_interactionHighOrderCheckBox));
		bool otherChecked = otherCheckBox->checked();

		if (checked)
		{
			if (keyTerm.containsAll(otherTerm) && !otherChecked)
			{
				otherCheckBox->setChecked(true);
				otherCheckBox->setBoundValue(Json::Value(true));
			}
		}
		else
		{
			if (otherTerm.containsAll(keyTerm) && otherChecked)
			{
				otherCheckBox->setChecked(false);
				otherCheckBox->setBoundValue(Json::Value(false));
			}
		}
	}

	if (form()) form()->blockValueChangeSignal(false);
}


