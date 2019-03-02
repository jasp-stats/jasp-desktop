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
#include "utilities/qutils.h"
#include "listmodeltermsavailable.h"
#include "listmodeltermsassigned.h"
#include "analysis/boundqmlitem.h"
#include "analysis/options/optionboolean.h"

using namespace std;

ListModelInteractionAssigned::ListModelInteractionAssigned(QMLListView* listView)
	: ListModelAssignedInterface(listView), InteractionModel ()
{
	_areTermsInteractions = true;
	_copyTermsWhenDropped = true;
	_addNewAvailableTermsToAssignedModel = true;
}

void ListModelInteractionAssigned::initTerms(const Terms &terms)
{
	_addTerms(terms, false);
	
	beginResetModel();
	_terms.set(interactionTerms());
	endResetModel();	
}

void ListModelInteractionAssigned::setAvailableModel(ListModelAvailableInterface *source)
{
	ListModelAssignedInterface::setAvailableModel(source);
	_terms.setSortParent(source->allTerms());
}

void ListModelInteractionAssigned::removeTerms(const QList<int> &indices)
{
	Terms toRemove;

	for (size_t index : indices)
	{
		if (index < _terms.size())
			toRemove.add(_terms.at(index));
	}

	removeInteractionTerms(toRemove);
	
	setTerms();
}

Terms *ListModelInteractionAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms;
	for (size_t index : indexes)
	{
		if (index < _terms.size())
			terms->add(_terms.at(index));
	}
	
	return terms;
}

void ListModelInteractionAssigned::_addTerms(const Terms& terms, bool combineWithExistingTerms)
{
	Terms fixedFactors;
	Terms randomFactors;
	Terms covariates;
	Terms others;
	for (const Term& term : terms)
	{
		QString itemType = getItemType(term);
		if (itemType == "fixedFactors")
			fixedFactors.add(term);
		else if (itemType == "randomFactors")
			randomFactors.add(term);
		else if (itemType == "covariates")
			covariates.add(term);
		else
			others.add(term);
	}
			
	if (fixedFactors.size() > 0)
		addFixedFactors(fixedFactors, combineWithExistingTerms);
	
	if (randomFactors.size() > 0)
		addRandomFactors(randomFactors);
	
	if (covariates.size() > 0)
		addCovariates(covariates);
	
	if (others.size() > 0)
		addInteractionTerms(others);
	
}

void ListModelInteractionAssigned::availableTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	if (termsAdded && termsAdded->size() > 0 && _addNewAvailableTermsToAssignedModel)
	{
		_addTerms(*termsAdded, true);
		setTerms();
	}
	
	if (termsRemoved && termsRemoved->size() > 0)
	{
		removeFactors(*termsRemoved);
		setTerms();
	}
}

QString ListModelInteractionAssigned::getItemType(const Term &term) const
{
	QString type;
	ListModelTermsAvailable* _source = dynamic_cast<ListModelTermsAvailable*>(source());	
	if (_source)
	{
		ListModel* model = _source->getSourceModelOfTerm(term);
		if (model)
		{
			type = model->getItemType(term);
			if (type.isEmpty() || type == "variables")
				type = model->name();
		}
	}
	
	return type;
}

bool ListModelInteractionAssigned::canAddTerms(Terms *terms) const
{
	Q_UNUSED(terms);

	return true;
}

void ListModelInteractionAssigned::addCombinedTerms(const Terms& terms, int assignType)
{
	Terms dropped;
	dropped.setSortParent(source()->allTerms());
	dropped.set(terms);

	Terms newTerms;

	switch (assignType)
	{
	case Cross:
		newTerms = dropped.crossCombinations();
		break;
	case Interaction:
		newTerms = dropped.wayCombinations(dropped.size());
		break;
	case MainEffects:
		newTerms = dropped.wayCombinations(1);
		break;
	case All2Way:
		newTerms = dropped.wayCombinations(2);
		break;
	case All3Way:
		newTerms = dropped.wayCombinations(3);
		break;
	case All4Way:
		newTerms = dropped.wayCombinations(4);
		break;
	case All5Way:
		newTerms = dropped.wayCombinations(5);
		break;
	default:
		(void)newTerms;
	}

	_addTerms(newTerms, false);	
	setTerms();
}

Terms* ListModelInteractionAssigned::addTerms(Terms *terms, int dropItemIndex)
{
	Q_UNUSED(dropItemIndex);
	
	addCombinedTerms(*terms, Cross);
	
	return nullptr;
}

void ListModelInteractionAssigned::setTerms()
{	
	beginResetModel();
	
	_terms.set(interactionTerms());
	
	endResetModel();
	
	emit modelChanged();
}
