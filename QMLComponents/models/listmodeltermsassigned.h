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

#ifndef LISTMODELTERMSASSIGNED_H
#define LISTMODELTERMSASSIGNED_H

#include "listmodelassignedinterface.h"


class ListModelTermsAssigned : public ListModelAssignedInterface
{
	Q_OBJECT
	
public:
	ListModelTermsAssigned(JASPListControl* listView);
	
	void			initTerms(const Terms &terms, const RowControlsValues& allValuesMap = RowControlsValues())			override;
	Terms			canAddTerms(const Terms& terms)																const	override;
	Terms			addTerms(const Terms& termsToAdd, int dropItemIndex = -1, const RowControlsValues& rowValues = RowControlsValues())	override;
	void			removeTerm(int index);

	virtual void	changeTerm(int index, const QString& name);

public slots:
	void availableTermsResetHandler(Terms termsToAdd, Terms termsToRemove)							override;

};

#endif // LISTMODELTERMSASSIGNED_H
