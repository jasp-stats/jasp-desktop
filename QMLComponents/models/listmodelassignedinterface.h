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

public slots:
	///< mds-plan post (ORIGIN-AUTHORITATIVE): the pool's update (added/removed diff plus the
	///  participation key `filterIdToUpdate` - the filterId the affected terms currently
	///  carry). Every family reconciles its references into the changed provider here: terms
	///  stamped `filterIdToUpdate` follow (survivors are REPLACED BY the delivered universe
	///  term, vanished names die), foreign stamps and -1 terms under a qualified provider are
	///  other providers' references and never move - see ListModel::termFollowsProviderChange.
	virtual void availableTermsResetHandler(Terms termsAdded, Terms termsRemoved, int filterIdToUpdate)	{}
			bool sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)		override;
			bool sourceLabelsReordered(QString columnName)											override;

protected:
	ListModelTermsAvailable*			_availableModel = nullptr;
};

#endif // LISTMODELASSIGNEDINTERFACE_H
