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
			ListModelFactorLevels* factorsModel = dynamic_cast<ListModelFactorLevels*>(sourceItem->listModel());
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

	_setAllowedVariables();

	connect(DesktopCommunicator::singleton(), &DesktopCommunicator::currentJaspThemeChanged, this, &VariablesListBase::_setAllowedVariables);

	_draggableModel->setItemType(property("itemType").toString());
	JASPControl::DropMode dropMode = JASPControl::DropMode(property("dropMode").toInt());
	_draggableModel->setDropMode(dropMode);
	
	//We use macros here because the signals come from QML
	QQuickItem::connect(this, SIGNAL(itemDoubleClicked(int)),						this, SLOT(itemDoubleClickedHandler(int)));
	QQuickItem::connect(this, SIGNAL(itemsDropped(QVariant, QVariant, int)),		this, SLOT(itemsDroppedHandler(QVariant, QVariant, int)));
	connect(this,	&VariablesListBase::allowedColumnsChanged,						this, &VariablesListBase::_setAllowedVariables);
	connect(this,	&VariablesListBase::suggestedColumnsChanged,					this, &VariablesListBase::_setAllowedVariables);
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
	if (targetModel && indexes.size() > 0)
	{
		std::sort(indexes.begin(), indexes.end());
		if (form()) form()->blockValueChangeSignal(true);

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
					removedTermsWhenAdding = targetModel->addTerms(termsAdded, dropItemIndex);

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
		
		if (form()) form()->blockValueChangeSignal(false);
	}
	else
	{
		Log::log()  << (!targetModel ? "no dropModel" : "no indexes") << std::endl;
	}
}

void VariablesListBase::setDropKeys(const QStringList &dropKeys)
{
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

void VariablesListBase::termsChangedHandler()
{
	setColumnsTypes(model()->termsTypes());
	setColumnsNames(model()->terms().asQList());

	if (_boundControl)	_boundControl->resetBoundValue();
	else JASPListControl::termsChangedHandler();
}

void VariablesListBase::_setAllowedVariables()
{
	QSet<QString> implicitAllowedTypes;

	// The implicitAllowedTypes is either the allowedColumns if they are explicitely defined
	// or the suggestedColumns with extra permitted types, with these rules:
	// . if suggestedType contains the scale type, then nomincal & ordinal types are then also allowed.
	// . if suggestedType contains the nomincal type, then nominalText & ordinal types are also allowed.

	auto listToSet = [](QStringList l) { return QSet<QString> (l.constBegin(), l.constEnd()); };
	if (!allowedColumns().empty())
		implicitAllowedTypes = listToSet(allowedColumns());
	else if (!suggestedColumns().empty())
	{
		implicitAllowedTypes = listToSet(suggestedColumns());
		if (suggestedColumns().contains("scale"))
		{
			implicitAllowedTypes.insert("nominal");
			implicitAllowedTypes.insert("ordinal");
		}
		if (suggestedColumns().contains("nominal"))
		{
			implicitAllowedTypes.insert("nominalText");
			implicitAllowedTypes.insert("ordinal");
		}
	}

	_variableTypesAllowed.clear();
	for (const QString& typeStr: implicitAllowedTypes)
		_variableTypesAllowed.insert(columnTypeFromString(fq(typeStr), columnType::unknown));

	// The suggectedColumnsIcons indicates which columns are allowed in the VariableList view.
	// It shows per default the suggested columns list, but if empty, it shows the alloaed columns list.
	QStringList iconTypeList,
				columnTypes = suggestedColumns().isEmpty() ?  allowedColumns() : suggestedColumns();
	for (const QString& columnTypeStr : columnTypes)
	{
		columnType type = columnTypeFromString(fq(columnTypeStr), columnType::unknown);
		if (type != columnType::unknown)
			iconTypeList.push_back(VariableInfo::getIconFile(type, VariableInfo::InactiveIconType));
	}
	setSuggestedColumnsIcons(iconTypeList);

	if (form() && form()->initialized())
		// If the allowed columns have changed, then refresh the model so that columns that are not allowed anymore are removed.
		model()->refresh();
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


