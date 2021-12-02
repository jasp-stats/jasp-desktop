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

class ListModelInteractionAssigned : public ListModelAssignedInterface, public InteractionModel
{
	Q_OBJECT
	
public:
	ListModelInteractionAssigned(JASPListControl* listView, bool mustContainLowerTerms, bool addInteractionsByDefault);

	void			initTerms(const Terms &terms, const RowControlsValues& = RowControlsValues())	override;
	Terms			termsFromIndexes(const QList<int> &indexes)								const	override;
	Terms			canAddTerms(const Terms& terms) const override;
	Terms			addTerms(const Terms& terms, int dropItemIndex = -1, const RowControlsValues& rowValues = RowControlsValues())	override;
	void			moveTerms(const QList<int>& indexes, int dropItemIndex = -1)					override;
	void			removeTerms(const QList<int> &indices)											override;
	QString			getItemType(const Term &term)											const	override;
	Terms			filterTerms(const Terms& terms, const QStringList& filters)						override;
		
public slots:
	void availableTermsResetHandler(Terms termsToAdd, Terms termsToRemove)							override;
	void sourceNamesChanged(QMap<QString, QString> map)												override;
	
protected:
	void addCombinedTerms(const Terms& terms, JASPControl::AssignType assignType);
	void _addTerms(const Terms& terms, bool combineWithExistingTerms);
	
	void setTerms();

	bool _addInteractionsByDefault;
};


#endif // LISTMODELINTERACTIONASSIGNED_H
