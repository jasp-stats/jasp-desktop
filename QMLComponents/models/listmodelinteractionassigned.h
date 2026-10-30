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

#ifndef LISTMODELINTERACTIONASSIGNED_H
#define LISTMODELINTERACTIONASSIGNED_H

#include "listmodelassignedinterface.h"

class VariablesListBase;

class ListModelInteractionAssigned : public ListModelAssignedInterface
{
	Q_OBJECT
	
public:
	ListModelInteractionAssigned(JASPListControl* listView);

    void			initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& = {})          override;
	Terms			addTerms(const Terms& terms, int dropItemIndex = -1, const Terms::RelatedValuesPerTerm& rowValues = {})	override;
	void			moveTerms(const QList<int>& indexes, int dropItemIndex = -1)					override;
	void			removeTerms(const QList<int> &indices)											override;
		
public slots:
	void			availableTermsResetHandler(Terms termsToAdd, Terms termsToRemove, int filterIdToUpdate)			override;
	
protected:
	void			_removeTerms(const Terms & termsToRemove);
	///< (ORIGIN-AUTHORITATIVE): the component version of the reconcile -
	///  interaction terms are multi-component references (their stamp was settled from the
	///  components' stamps, Terms::crossCombinations/ffCombinations), so "the provider still
	///  serves this reference" means EVERY component is still in the delivered universe (the
	///  same staleness rule _removeTerms applies through
	///  discardWhatDoesNotContainTheseComponents). A survivor's stamp is re-settled from the
	///  delivered universe terms of its components (the same carry/settle rule that created
	///  the interaction - there is no universe term of the interaction itself to be replaced
	///  by); value, components and per-component types are unchanged (column type changes flow
	///  through their own signal path). Vanished interactions are returned for the row
	///  machinery.
	Terms			reconcileProviderChange(int filterIdToUpdate, const Terms &newUniverse)			override;

	VariablesListBase* _variablesList = nullptr;

};


#endif // LISTMODELINTERACTIONASSIGNED_H
