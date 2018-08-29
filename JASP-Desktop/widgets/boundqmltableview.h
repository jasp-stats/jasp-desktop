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

#ifndef BOUNDQMLTABLEVIEW_H
#define BOUNDQMLTABLEVIEW_H

#include "analysis/boundqmlitem.h"
#include "listmodel.h"

class BoundQMLTableView : public BoundQMLItem
{
	Q_OBJECT
	
public:
	explicit BoundQMLTableView(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual void setUp() OVERRIDE;	
	
	ListModel* model() { return _model; }

protected:
	virtual void resetTermsFromSyncModels() {}
	
	ListModel* _model;
	std::vector<ListModel*> _syncModels;
	bool _needsSyncModels;

private slots:	
	virtual void syncTermsChanged(Terms* termsAdded, Terms* termsRemoved);

};

#endif // BOUNDQMLTABLEVIEW_H
