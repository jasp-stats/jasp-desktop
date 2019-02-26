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

#ifndef LISTMODELAVAILABLEINTERFACE_H
#define LISTMODELAVAILABLEINTERFACE_H

#include "listmodeldraggable.h"
#include "analysis/options/terms.h"
#include "analysis/options/variableinfo.h"

class ListModelAssignedInterface;

class ListModelAvailableInterface: public ListModelDraggable, public VariableInfoProvider
{
	Q_OBJECT
public:
	ListModelAvailableInterface(QMLListView* listView) : ListModelDraggable(listView) {}
	
	virtual const Terms& allTerms() const { return _allTerms; }
	virtual void removeAssignedTerms(const Terms& terms);
	virtual void resetTermsFromSourceModels() = 0;	
	
	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const OVERRIDE;	
	
public slots:
	virtual void sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved) OVERRIDE;

protected:
	Terms _allTerms;
	
	Terms _tempRemovedTerms;
	Terms _tempAddedTerms;	
	
	void setChangedTerms(const Terms &newTerms);
	void removeTermsInAssignedList();
};

#endif // LISTMODELTERMSAVAILABLEINTERFACE_H
