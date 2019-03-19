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

#ifndef BOUNDQMLFACTORSFORM_H
#define BOUNDQMLFACTORSFORM_H

#include "analysis/boundqmlitem.h"
#include "listmodelfactorsform.h"
#include "qmllistview.h"
#include "analysis/options/optionstable.h"




class BoundQMLFactorsForm :  public QMLListView, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLFactorsForm(QQuickItem* item, AnalysisForm* form);	

	virtual ListModel* model() OVERRIDE		{ return _factorsModel; }
	virtual Option* boundTo() OVERRIDE		{ return _boundTo; }
	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual Option* createOption() OVERRIDE;
	virtual bool isOptionValid(Option* option) OVERRIDE;
	virtual bool isJsonValid(const Json::Value& optionValue) OVERRIDE;

protected slots:
	virtual void modelChangedHandler() OVERRIDE;
	
	void addListViewSlot(BoundQMLListViewTerms* listView);
	
private:
	ListModelFactorsForm* _factorsModel;
	OptionsTable* _boundTo;
	QString _availableVariablesListName;
	QQuickItem* _availableVariablesListItem;
	int _initNumberFactors;
	
};

#endif // BOUNDQMLFACTORSFORM_H
