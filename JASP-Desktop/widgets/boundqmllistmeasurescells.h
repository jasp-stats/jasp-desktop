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

#ifndef BOUNDQMLLISTMEASURESCELLS_H
#define BOUNDQMLLISTMEASURESCELLS_H

#include "boundqmldraggablelistview.h"
#include "listmodelmeasurescells.h"
#include "listmodelfactors.h"
#include "analysis/options/optionvariables.h"

class BoundQMLListMeasuresCells : public BoundQMLDraggableListView
{
	Q_OBJECT
	
public:
	explicit BoundQMLListMeasuresCells(QQuickItem* item, AnalysisQMLForm* form);
	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	virtual Option* createOption() OVERRIDE;
	virtual void setUp() OVERRIDE;
	
private slots:
	void modelChangedHandler();
	
protected:
	OptionVariables* _boundTo;
	ListModelMeasuresCells* _measuresCellsModel;
	std::vector<ListModelFactors*> _syncFactorsModels;
	
	
	virtual void resetTermsFromSyncModels() OVERRIDE;	

private:
	Terms _tempTerms;
	
};

#endif // BOUNDQMLLISTMEASURESCELLS_H
