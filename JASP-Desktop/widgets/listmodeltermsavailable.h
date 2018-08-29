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

#ifndef LISTMODELTERMSAVAILABLE_H
#define LISTMODELTERMSAVAILABLE_H

#include "listmodeltermsavailableinterface.h"
#include "common.h"

class ListModelTermsAssigned;

class ListModelTermsAvailable : public ListModelTermsAvailableInterface
{
	Q_OBJECT	
public:
	explicit ListModelTermsAvailable(AnalysisQMLForm *form, QQuickItem* item);
	
	virtual void setUp() OVERRIDE;
	
	virtual void initTerms(const Terms &terms);
	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const OVERRIDE;
	
	virtual void resetTermsFromSyncModels();
	virtual ListModel* getSyncModelOfTerm(const Term& term);

public slots:
	virtual void syncTermsChanged(Terms* termsAdded, Terms* termsRemoved) OVERRIDE;
	
private:
	void _setChangedTerms(const Terms& newTerms);
	Terms _tempRemovedTerms;
	Terms _tempAddedTerms;
		
};

#endif // LISTMODELTERMSAVAILABLE_H
