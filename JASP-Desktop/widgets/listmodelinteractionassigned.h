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
#include "listmodelavailableinterface.h"
#include "interactionmodel.h"
#include "analysis/options/options.h"
#include "analysis/options/optionterm.h"
#include "analysis/analysisqmldefines.h"

class ListModelInteractionAssigned : public ListModelAssignedInterface, public InteractionModel
{
	Q_OBJECT
	
public:
	ListModelInteractionAssigned(QMLListView* listView, bool addAvailableTermsToAssigned);

	virtual void initTerms(const Terms &terms) OVERRIDE;
	
	virtual void setAvailableModel(ListModelAvailableInterface *source) OVERRIDE;
	
	virtual Terms *termsFromIndexes(const QList<int> &indexes) const OVERRIDE;
	virtual bool canAddTerms(Terms *terms) const OVERRIDE;
	virtual Terms* addTerms(Terms *terms, int dropItemIndex = -1, const QString& assignOption = "") OVERRIDE;
	virtual void removeTerms(const QList<int> &indices) OVERRIDE;
	
	virtual QString getItemType(const Term &term) const OVERRIDE;
	
		
public slots:
	virtual void availableTermsChanged(Terms* termsToAdd, Terms* termsToRemove) OVERRIDE;
	
protected:
	void addCombinedTerms(const Terms& terms, qmlAssignType assignType);
	void _addTerms(const Terms& terms, bool combineWithExistingTerms);
	
	void setTerms();
};


#endif // LISTMODELINTERACTIONASSIGNED_H
