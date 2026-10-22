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
#include "models/listmodeltermsassigned.h"
#include "models/listmodelmeasurescellsassigned.h"
#include "models/listmodelinteractionassigned.h"
#include "models/listmodellayersassigned.h"
#include "models/listmodelmultitermsassigned.h"
#include "boundcontrols/boundcontrolmeasurescells.h"
#include "boundcontrols/boundcontrollayers.h"
#include "boundcontrols/boundcontrolterms.h"
#include "boundcontrols/boundcontrolmultiterms.h"
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

	//Per-list dataset/filter selection: the analysis (and with it the selection)
	//can only become known after construction; formIsKnown tells us when that happened (dynamic
	//controls), setUp is the hook the static ones get - see handleFormIsKnown.
	connect(this, &JASPControl::formIsKnown, this, &VariablesListBase::handleFormIsKnown);

	connect(&_hostSelection, &HostFilterSelection::selectedFilterIdChanged,		this, &VariablesListBase::selectedFilterIdChanged);
	connect(&_hostSelection, &HostFilterSelection::selectionAvailableChanged,	this, &VariablesListBase::selectionAvailableChanged);
	connect(&_hostSelection, &HostFilterSelection::filterSelectionValuesChanged, this, &VariablesListBase::filterSelectionValuesChanged);
}

void VariablesListBase::setUp()
{
	JASPListControl::setUp();

	_setRelations();

	//AnalysisForm::setAnalysisUp() -> _setUp() is the hook static controls get (formIsKnown is only
	//emitted for dynamically created ones, see JASPControl's constructor), and by then the analysis
	//is attached. Reuse handleFormIsKnown: its connections are UniqueConnections, so getting here a
	//second time (dynamic controls also emit formIsKnown) is harmless.
	handleFormIsKnown(form());

	ListModelTermsAvailable* availableModel = qobject_cast<ListModelTermsAvailable*>(_draggableModel);

	if (availableModel)
	{
		SortMenuModel* sortedMenuModel = new SortMenuModel(_draggableModel, {Sortable::None, Sortable::SortByName, Sortable::SortByType});
		setProperty("sortMenuModel", QVariant::fromValue(sortedMenuModel));
	}

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
	if (value == Json::nullValue && isBound() && addAvailableVariablesToAssigned())
	{
		// A bound control should have a value when it is initialized from a JASP file. So here is the case when a new analysis is created.
		// If it is an Assigned VariablesList with addAvailableVariablesToAssigned set to true, if there is some terms in the correspondig Available VariablesList,
		// these terms must be used to initialize this VariablesList
		ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(_draggableModel);
		if (assignedModel && assignedModel->availableModel())
			assignedModel->initTerms(assignedModel->availableModel()->terms());
	}

	if (listViewType() == ListViewType::RepeatedMeasures)
	{
		for (SourceItem* sourceItem : _sourceItems)
		{
			ListModelFactorLevels* factorsModel = dynamic_cast<ListModelFactorLevels*>(sourceItem->sourceListModel());
			if (!factorsModel)
				addControlError(tr("Source model of %1 must be from a Factor List").arg(name()));
			else
			{
				BoundControlMeasuresCells* measuresCellsControl = dynamic_cast<BoundControlMeasuresCells*>(_boundControl);
				measuresCellsControl->addFactorModel(factorsModel);
			}
		}
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

	case ListViewType::Layers:
	{
		auto *	layersModel		= new ListModelLayersAssigned(this);
		if (isBound())
			_boundControl		= new BoundControlLayers(layersModel);
		_draggableModel			= layersModel;
		_useTermsInRSyntax		= false;

		break;
	}
		
	case ListViewType::RepeatedMeasures:
	{
		 auto * measuresCellsModel	= new ListModelMeasuresCellsAssigned(this);
		if (isBound())
			_boundControl			= new BoundControlMeasuresCells(measuresCellsModel);
		_draggableModel				= measuresCellsModel;
		break;
	}
		
	case ListViewType::AssignedVariables:
	{
		ListModelAssignedInterface* termsModel = nullptr;

		if (columns() > 1)
		{
			auto *	multiTermsModel = new ListModelMultiTermsAssigned(this, columns());
			if (isBound())
				_boundControl		= new BoundControlMultiTerms(multiTermsModel);
			_draggableModel			= multiTermsModel;
		}
		else
		{
			termsModel			= new ListModelTermsAssigned(this);
			if (isBound())
				_boundControl	= new BoundControlTerms(termsModel, _maxRows == 1);
			_draggableModel		= termsModel;
		}
		break;
	}
		
	case ListViewType::Interaction:
	{
		auto *	termsModel		= new ListModelInteractionAssigned(this);
		if (isBound())
			_boundControl		= new BoundControlTerms(termsModel);
		_draggableModel			= termsModel;
		break;
	}
		
	}

	JASPListControl::setUpModel();
}

bool VariablesListBase::addRowControl(const TermRowKey &key, JASPControl *control)
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

bool VariablesListBase::containsInteractions() const
{
	if (_listViewType == ListViewType::Interaction)
		return true;

	return JASPListControl::containsInteractions();
}

// (found by the component zoo): declaring dataSetSelectionOption on a list IS the
//declaration that its values are columns of that namespace (C10 list-as-host) - no available-list
//or dataset-source relation required for that. JASPListControl::containsVariables() only knows the
//RELATION routes, so a standalone host-assigned list (the second-source pattern) came out false,
//which turned encodeValue() off: its values were never encoded and never provenanced into the
//sidecar - the list selected a dataset its values could not reach. Available lists are unbound
//(_isBound = false), so for them this only changes the icon/role reads they already had.
bool VariablesListBase::containsVariables() const
{
	if (_hostSelection.selectionAvailable())
		return true;

	return JASPListControl::containsVariables();
}

void VariablesListBase::setDropKeys(const QStringList &dropKeys)
{
	if (dropKeys != _dropKeys)
	{
		// The first key gives the related list: when only the other keys change (e.g. when a list becomes invisible),
		// the relations must not be set again, otherwise this list would be added once more to its available model.
		bool relatedListChanged = dropKeys.value(0) != _dropKeys.value(0);
		_dropKeys = dropKeys;
		if (relatedListChanged)
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
			// this list's own row identity selects the RowControls - a bare name
			//would land on the first twin's row, not necessarily this one's.
			JASPListControl* relatedControl = qobject_cast<JASPListControl*>(_parentListView->model()->getRowControl(parentRowKey(), relatedName));
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
			ListModelTermsAvailable* availableModel = dynamic_cast<ListModelTermsAvailable*>(relatedModel);
			if (!availableModel)
				addControlError(tr("Wrong kind of source for VariableList %1").arg(name()));
			else
			{
				assignedModel->setAvailableModel(availableModel);
				availableModel->addAssignedModel(assignedModel);
				addDependency(availableModel->listView());
				emit containsVariablesChanged();
				emit containsInteractionsChanged();

				//The namespace inheritance (relatedNamespaceList -> related available list, resolved
				//by JASPListControl::resolvedSelectionHost) only exists from here on, and setUpModel
				//(which first wired the model's VariableInfo) ran BEFORE the relations: re-push the
				//now-resolved varInfo so an assigned list under a selecting available list resolves
				//its column info (types, previews, names) through that host.
				if (ListModel * m = model())
					if (VariableInfo * info = resolvedVarInfo())
						m->setVarInfo(info);

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

JASPListControl * VariablesListBase::relatedNamespaceList()
{
	//Semantic main path first: an explicit `source:` names where this list's variables come from.
	if (JASPListControl * explicitSource = JASPListControl::relatedNamespaceList())
		return explicitSource;

	// with selection on the LIST, the available list a VariablesForm hands to its
	//assigned lists (dropKeys[0], wired into availableModel() by _setRelations) is where those
	//lists' items come from - so their namespace follows it, exactly like an explicit source.
	//A list that declares its own dataSetSelectionOption serves itself instead: own the edge,
	//do not inherit one (C13: the host-bound fallback applies to the list that HAS the host).
	if (_hostSelection.selectionAvailable())
		return nullptr;

	ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(_draggableModel);
	ListModelTermsAvailable* availableModel	= assignedModel ? assignedModel->availableModel() : nullptr;

	if (!availableModel)
		return nullptr;

	return availableModel->listView();
}

void VariablesListBase::handleFormIsKnown(AnalysisForm * form)
{
	if(!form)
		return;

	//AnalysisForm::setAnalysisUp() re-runs whenever the analysis or its filter changes; keep the
	//per-list selection in sync (it follows the analysis filter only until the user picks one).
	connect(form, &AnalysisForm::analysisChanged,	this, &VariablesListBase::handleAnalysisChanged,			Qt::UniqueConnection);
	connect(form, &AnalysisForm::filterChanged,		this, &VariablesListBase::handleAnalysisFilterChanged,	Qt::UniqueConnection);

	//The analysis is usually already attached when we get here - analysisChanged may well have fired
	//before this connection existed. Pull the current analysis in explicitly, otherwise the
	//selection never gets its default and QML keeps stale values.
	handleAnalysisChanged();
}

void VariablesListBase::handleAnalysisChanged()
{
	//With the analysis known, the selection (if this list declares one) marks the analysis
	//multiDataSetAware, hooks the workspace removal signals and gets its default: the analysis'
	//current filter. All QML bindings of the selection widget hang off these signals:
	_hostSelection.analysisBecameKnown();

	//The relations (and with them the inherited namespace) may only now exist too:
	if (ListModel * m = model())
		if (VariableInfo * info = resolvedVarInfo())
			m->setVarInfo(info);

	emit selectedFilterIdChanged();
	emit filterSelectionValuesChanged();
	emit selectionAvailableChanged();
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
		//the self-skip compares row VALUES on purpose: keyTerm is parsed from
		//the checkbox's rowValue string and is unqualified, so comparing Term objects would use
		//the strict identity incl. filterId (C9) and the skip would silently stop firing. (The
		//row CONTROLS lookup below is exact-row-keyed since .)
		if (otherTerm.value() == keyTerm.value())
			continue;

		// the row controls map keys on row identity, so the lookup uses the model
		//term's own TermRowKey.
		RowControls* rowControls = _draggableModel->getRowControls(TermRowKey::ofTerm(otherTerm));
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


