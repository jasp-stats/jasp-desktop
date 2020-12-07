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
	ListModelTermsAssigned(JASPListControl* listView, int maxRows = -1);
	
	void			initTerms(const Terms &terms, const RowControlsOptions& allOptionsMap = RowControlsOptions())			override;
	Terms			canAddTerms(const Terms& terms)																	const	override;
	Terms			addTerms(const Terms& termsToAdd, int dropItemIndex = -1, JASPControl::AssignType assignOption = JASPControl::AssignType::AssignDefault)	override;
	Terms			termsEx(const QString& what)																			override;
	void			removeTerm(int index);

	virtual void	changeTerm(int index, const QString& name);

public slots:
	void availableTermsResetHandler(Terms termsToAdd, Terms termsToRemove)							override;

	
private:
	int		_maxRows = -1;
	Terms	_tempTermsToSendBack;

};

#endif // LISTMODELTERMSASSIGNED_H
