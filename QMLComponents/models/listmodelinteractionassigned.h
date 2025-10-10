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
	void			availableTermsResetHandler(Terms termsToAdd, Terms termsToRemove)				override;
	
protected:
	void			_removeTerms(const Terms & termsToRemove);

	VariablesListBase* _variablesList = nullptr;

};


#endif // LISTMODELINTERACTIONASSIGNED_H
