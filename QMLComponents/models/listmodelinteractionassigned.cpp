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

#include "listmodelinteractionassigned.h"
#include "qutils.h"
#include "listmodeltermsavailable.h"
#include "listmodeltermsassigned.h"
#include "controls/jasplistcontrol.h"
#include "analysisform.h"
#include "variableslistbase.h"
#include "sourceitem.h"

using namespace std;

ListModelInteractionAssigned::ListModelInteractionAssigned(JASPListControl* listView)
	: ListModelAssignedInterface(listView)
{
	_variablesList = qobject_cast<VariablesListBase*>(listView);
}

void ListModelInteractionAssigned::initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap)
{
	// Initialization of the terms can be a re-initialization: in this case the interaction terms can be lost
	// So the interaction terms must be kept, and if their components are in the new terms, then add this interaction.
	Terms newTerms = terms;

    Terms oldInteractions = this->terms();
    for (const Term& oldInteraction : oldInteractions)
    {
        if (oldInteraction.size() > 1)
        {
            bool add = true;
            for (const QString& comp : oldInteraction.components())
            {
                if (!terms.containsValue(comp))
                    add = false;
            }
            if (add)
                newTerms.add(oldInteraction);
        }
    }
	_setTerms(newTerms);
    ListModelAssignedInterface::initTerms(this->terms(), allValuesMap);
}

void ListModelInteractionAssigned::removeTerms(const QList<int> &indices)
{
	if(!indices.count())
		return;
	
	Terms toRemove;

	for (int i : indices)
	{
		int index = i;
		if (index < rowCount())
			toRemove.add(terms().at(size_t(index)));
	}

	_removeTerms(toRemove);
}

void ListModelInteractionAssigned::_removeTerms(const Terms & termsToRemove)
{
	if (termsToRemove.size() == 0)
		return;

	beginResetModel();

	Terms existingTerms = terms();
	if (_variablesList->interactionContainLowerTerms())
		existingTerms.discardWhatDoesContainTheseTerms(termsToRemove);
	else
	{
		existingTerms.remove(termsToRemove);
		if (availableModel())
			existingTerms.discardWhatDoesNotContainTheseComponents(availableModel()->allTerms());
	}

	_setTerms(existingTerms);

	endResetModel();
}


void ListModelInteractionAssigned::availableTermsResetHandler(Terms termsAdded, Terms termsRemoved)
{
	if (listView()->form() && !listView()->form()->initialized())
		return;

	auto getControls = [this](QVariant source) -> JASPListControls
	{
		JASPListControls listControls;
		QList<QVariant> rawSources = SourceItem::getListVariant(source);

		for (const QVariant& rawSource : rawSources)
		{
			JASPListControl* listControl = rawSource.value<JASPListControl*>();
			if (!listControl && rawSource.typeId() == QMetaType::QString)
			{
				JASPControl* control = listView()->form()->getControl(rawSource.toString());
				listControl = qobject_cast<JASPListControl*>(control);
			}
			if (listControl)
				listControls.push_back(listControl);
		}

		return listControls;
	};

	if (termsAdded.size() > 0 && listView()->addAvailableVariablesToAssigned())
	{
		beginResetModel();

		// When new terms are added in the Available VariablesList (via a source), these terms must be also added in this Assigned VariablesList
		// if addInteractionsByDefault is true, also the interactions between this terms must be added.
		// But if the new terms come from certain sources (sourceWithoutDefaultInteraction, this is per default the randomFactors and covariates sources), then these terms
		// should just be added without interaction with the other terms.
		if (_variablesList->addInteractionsByDefault())
		{
			Terms termsWithoutDefaultInteraction;
			JASPListControls sourcesWithoutDefaultInteraction = getControls(_variablesList->sourceWithoutDefaultInteraction());
			for (JASPListControl* source : sourcesWithoutDefaultInteraction)
				termsWithoutDefaultInteraction.add(source->model()->terms());

			Terms	existingTermsWithoutDefaultInteraction = terms(),
					existingTermsWithDefaultInteraction = terms(),
					newTermsWithoutDefaultInteraction = termsAdded,
					newTermsWithDefaultInteraction = termsAdded;
			newTermsWithDefaultInteraction.discardWhatDoesContainTheseTerms(termsWithoutDefaultInteraction);
			newTermsWithoutDefaultInteraction.discardWhatIsntTheseTerms(termsWithoutDefaultInteraction);
			existingTermsWithDefaultInteraction.discardWhatDoesContainTheseTerms(termsWithoutDefaultInteraction);
			existingTermsWithoutDefaultInteraction.discardWhatIsntTheseTerms(termsWithoutDefaultInteraction);

			Terms newTerms = existingTermsWithDefaultInteraction.ffCombinations(newTermsWithDefaultInteraction);
			newTerms.add(existingTermsWithoutDefaultInteraction);
			newTerms.add(newTermsWithoutDefaultInteraction);

			_setTerms(newTerms);
		}
		else
			_addTerms(termsAdded);

		endResetModel();

	}
	
	_removeTerms(termsRemoved);
}

Terms ListModelInteractionAssigned::addTerms(const Terms& terms, int , const Terms::RelatedValuesPerTerm&)
{
	if (terms.size() == 0)
		return Terms();
	
	Terms dropped;
	if (availableModel())
		dropped.setSortParent(availableModel()->allTerms());
	dropped.set(checkTermsTypes(terms));

	Terms newTerms = dropped.combineTerms(JASPControl::CombinationType::CombinationCross);

	beginResetModel();
	_addTerms(newTerms);
	endResetModel();

	return Terms();
}

void ListModelInteractionAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	JASPControl::DropMode _dropMode = dropMode();
	if (indexes.length() == 0 || _dropMode == JASPControl::DropMode::DropNone)
		return;

	beginResetModel();

	Terms termsToMove = termsFromIndexes(indexes);
	if (dropItemIndex == -1)
		dropItemIndex = int(terms().size());
	for (int index : indexes)
	{
		if (index < dropItemIndex)
			dropItemIndex--;
	}

	Terms newTerms = this->terms();
	newTerms.remove(termsToMove);
	newTerms.insert(dropItemIndex, termsToMove);

	_setTerms(newTerms);

	endResetModel();

}
