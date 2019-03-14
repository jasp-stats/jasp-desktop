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

#ifndef BOUNDQMLLISTVIEWTERMS_H
#define BOUNDQMLLISTVIEWTERMS_H

#include "boundqmllistviewdraggable.h"
#include "listmodelassignedinterface.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optionstable.h"

class BoundQMLListViewTerms : public BoundQMLListViewDraggable
{
	Q_OBJECT
	
public:
	BoundQMLListViewTerms(QQuickItem* item, AnalysisForm* form, bool interaction = false);
	
	virtual ListModel* model() OVERRIDE	{ return _termsModel; }
	virtual Option* boundTo() OVERRIDE
	{
		if (_hasExtraControls || _termsModel->areTermsInteractions())
			return _optionsTable;
		else
			return _optionVariables; 
	}	
	
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	
	virtual Option* createOption() OVERRIDE;
	virtual bool isOptionValid(Option* option) OVERRIDE;
	virtual bool isJsonValid(const Json::Value& optionValue) OVERRIDE;
	
	virtual void setTermsAreInteractions() OVERRIDE;	

protected:
	virtual void initExtraControlOptions(const QString &colName, Options *options);
	
protected slots:
	virtual void modelChangedHandler() OVERRIDE;
	void bindExtraControlOptions();
	
private:
	OptionVariables* _optionVariables;
	OptionsTable* _optionsTable;
	ListModelAssignedInterface* _termsModel;
	bool _singleItem		= false;
	
	void extraOptionsChangedSlot(Option *option);
	void updateNuisances(bool checked);
};

#endif // BOUNDQMLLISTVIEWTERMS_H
