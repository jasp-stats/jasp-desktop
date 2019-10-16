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

ListModelInteractionAvailable::ListModelInteractionAvailable(QMLListView* listView)
	: ListModelAvailableInterface(listView), InteractionModel ()
{
	_areTermsInteractions = true;
}

void ListModelInteractionAvailable::resetTermsFromSourceModels(bool updateAssigned)
{
	const QList<QMLListView::SourceType*>& sourceItems = listView()->sourceModels();
	if (sourceItems.size() == 0)
		return;
	
	beginResetModel();
	Terms termsAvailable;
	clearInteractions();
	Terms fixedFactors;
	Terms randomFactors;
	Terms covariates;

	QMap<ListModel*, Terms> sourceTerms = getSourceTermsPerModel();
	QMapIterator<ListModel*, Terms> it(sourceTerms);
	while (it.hasNext())
	{
		it.next();
		ListModel* sourceModel = it.key();
		const Terms& terms = it.value();
		for (const Term& term : terms)
		{
			QString itemType = sourceModel->getItemType(term);
			if (itemType.isEmpty() || itemType == "variables")
				itemType = sourceModel->name();

			if (itemType == "fixedFactors")
				fixedFactors.add(term);
			else if (itemType == "randomFactors")
				randomFactors.add(term);
			else if (itemType == "covariates")
				covariates.add(term);
		}
	}
		
	if (fixedFactors.size() > 0)
		addFixedFactors(fixedFactors);
	
	if (randomFactors.size() > 0)
		addRandomFactors(randomFactors);
	
	if (covariates.size() > 0)
		addCovariates(covariates);
	
	const Terms& interactions = interactionTerms();
	setChangedTerms(interactions);
	
	_allTerms.set(interactions);
	_terms.set(interactions);
	_terms.setSortParent(_allTerms);
	
	removeTermsInAssignedList();
	
	endResetModel();

	if (updateAssigned)
		emit allAvailableTermsChanged(&_tempAddedTerms, &_tempRemovedTerms);

}

