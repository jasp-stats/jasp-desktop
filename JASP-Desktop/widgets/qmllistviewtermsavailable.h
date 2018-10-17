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

#ifndef QMLLISTVIEWTERMSAVAILABLE_H
#define QMLLISTVIEWTERMSAVAILABLE_H

#include "qmllistviewdraggable.h"
#include "listmodeltermsavailable.h"

class ListModelAssignedInterface;

class QMLListViewTermsAvailable : public QMLListViewDraggable
{
Q_OBJECT
public:
	QMLListViewTermsAvailable(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual ListModel* model() OVERRIDE	{ return _availableModel; }
	virtual void setTermsAreNotVariables() OVERRIDE;

	void addAssignedModel(ListModelAssignedInterface* model);
	
	const QList<ListModelAssignedInterface*>& assignedModel() const { return _assignedModels; }
	
	
protected:
	ListModelTermsAvailable* _availableModel;
	QList<ListModelAssignedInterface*> _assignedModels;
};

#endif // QMLLISTVIEWTERMSAVAILABLE_H
