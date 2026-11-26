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

#ifndef LISTMODELASSIGNEDINTERFACE_H
#define LISTMODELASSIGNEDINTERFACE_H

#include "listmodeldraggable.h"
#include "listmodeltermsavailable.h"

class ListModelAssignedInterface : public ListModelDraggable
{
	Q_OBJECT
public:
	ListModelAssignedInterface(JASPListControl* listView);
	
	void							refresh()														override;

	virtual void					setAvailableModel(ListModelTermsAvailable *availableModel);
	ListModelTermsAvailable*		availableModel() const													{ return _availableModel; }

	///< Sets the terms of the value the control is bound to. That value is complete, so the list gets exactly these terms,
	///  whereas initTerms may re-initialize the list with what survives of its terms (e.g. keeping their interactions).
	virtual void					bindTerms(const Terms& terms, const Terms::RelatedValuesPerTerm& allValuesMap = {})	{ initTerms(terms, allValuesMap); }

public slots:
	virtual void availableTermsResetHandler(Terms termsAdded, Terms termsRemoved)				{}
			bool sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)		override;
			bool sourceLabelsReordered(QString columnName)											override;

protected:
	ListModelTermsAvailable*			_availableModel = nullptr;
};

#endif // LISTMODELASSIGNEDINTERFACE_H
