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
#include "listmodelavailableinterface.h"

class ListModelAssignedInterface : public ListModelDraggable
{
	Q_OBJECT
public:
	ListModelAssignedInterface(JASPListControl* listView);
	
	void							refresh()														override;

	virtual void					setAvailableModel(ListModelAvailableInterface *availableModel);
	ListModelAvailableInterface*	availableModel() const													{ return _availableModel; }
	bool							checkAllowedTerms(Terms& terms);

public slots:
	virtual void availableTermsResetHandler(Terms termsAdded, Terms termsRemoved)				{}
			int  sourceColumnTypeChanged(QString name)												override;
			bool sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)		override;
			bool sourceLabelsReordered(QString columnName)											override;

protected:
	ListModelAvailableInterface*			_availableModel = nullptr;
};

#endif // LISTMODELASSIGNEDINTERFACE_H
