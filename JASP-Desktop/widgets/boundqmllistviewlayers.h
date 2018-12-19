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

#ifndef BOUNDQMLLISTVIEWLAYERS_H
#define BOUNDQMLLISTVIEWLAYERS_H

#include "boundqmllistviewdraggable.h"
#include "listmodellayersassigned.h"
#include "analysis/options/optionstable.h"

class BoundQMLListViewLayers : public BoundQMLListViewDraggable
{
	Q_OBJECT
	
public:
	BoundQMLListViewLayers(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual ListModel* model() OVERRIDE	{ return _layersModel; }
	virtual Option* boundTo() OVERRIDE	{ return _boundTo; }
	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual Option* createOption() OVERRIDE;

protected slots:
	virtual void modelChangedHandler() OVERRIDE;
	
private:
	OptionsTable* _boundTo;
	ListModelLayersAssigned* _layersModel;
	
};

#endif // BOUNDQMLLISTVIEWLAYERS_H
