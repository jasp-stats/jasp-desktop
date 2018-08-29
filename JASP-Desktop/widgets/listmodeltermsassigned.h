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

#include "listmodeltermsassignedinterface.h"


class ListModelTermsAssigned : public ListModelTermsAssignedInterface
{
	Q_OBJECT
	
public:
	explicit ListModelTermsAssigned(AnalysisQMLForm *form, QQuickItem* item, bool onlyOneTerm = false);
	
	virtual void initTerms(const std::vector<std::vector<std::string> >& terms);
	
	virtual bool canAddTerms(Terms *terms) const OVERRIDE;
	virtual Terms* addTerms(Terms *terms, int dropItemIndex = -1) OVERRIDE;
	virtual void removeTerms(const QList<int> &indices) OVERRIDE;

public slots:
	virtual void availableTermsChanged(Terms* termsToAdd, Terms* termsToRemove) OVERRIDE;
	
private:	
	bool _onlyOneTerm;

	Terms _tempTermsToRemove;

};

#endif // LISTMODELTERMSASSIGNED_H
