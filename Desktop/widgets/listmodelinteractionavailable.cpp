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

#include "listmodelinteractionavailable.h"
#include "listmodeltermsassigned.h"
#include "../analysis/analysisform.h"
#include "jasplistcontrol.h"
#include "sourceitem.h"

ListModelInteractionAvailable::ListModelInteractionAvailable(JASPListControl* listView)
	: ListModelAvailableInterface(listView), InteractionModel ()
{
	setTermsAreInteractions(true);
}

void ListModelInteractionAvailable::resetTermsFromSources(bool updateAssigned)
{	
	beginResetModel();
	Terms termsAvailable;
	clearInteractions();
	Terms fixedFactors;
	Terms randomFactors;
	Terms covariates;

	listView()->applyToAllSources([&](SourceItem *sourceItem, const Terms& terms)
	{
		ListModel* sourceModel = sourceItem->listModel();
		for (const Term& term : terms)
		{
			QString itemType = sourceModel ? sourceModel->getItemType(term) : "";
			if (itemType.isEmpty() || itemType == "variables")
				itemType = sourceModel->name();

			if (itemType == "fixedFactors")
				fixedFactors.add(term);
			else if (itemType == "randomFactors")
				randomFactors.add(term);
			else if (itemType == "covariates")
				covariates.add(term);
		}
	});
		
	if (fixedFactors.size() > 0)
		addFixedFactors(fixedFactors);
	
	if (randomFactors.size() > 0)
		addRandomFactors(randomFactors);
	
	if (covariates.size() > 0)
		addCovariates(covariates);
	
	const Terms& interactions = interactionTerms();
	Terms removedTerms, addedTerms;

	for (const Term& term : _allTerms)
		if (!interactions.contains(term))
			removedTerms.add(term);

	for (const Term& term : interactions)
		if (!_allTerms.contains(term))
			addedTerms.add(term);
	
	_allTerms.set(interactions);
	_setTerms(interactions, _allTerms);
	
	removeTermsInAssignedList();
	
	endResetModel();

	if (updateAssigned)
		emit availableTermsReset(addedTerms, removedTerms);

}

