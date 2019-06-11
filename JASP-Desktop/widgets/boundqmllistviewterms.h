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
	
	ListModel*	model()										override { return _termsModel; }
	Option*		boundTo()									override
	{
		if (_hasExtraControls || _termsModel->areTermsInteractions())
			return _optionsTable;
		else
			return _optionVariables; 
	}	
	
	void		bindTo(Option *option)						override;
	void		unbind()									override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;
	void		setTermsAreInteractions()					override;	

protected:
	virtual void initExtraControlOptions(const QString &colName, Options *options);
	
protected slots:
	void modelChangedHandler() override;
	void bindExtraControlOptions();
	
private:
	OptionVariables*				_optionVariables;
	OptionsTable*					_optionsTable;
	ListModelAssignedInterface*		_termsModel;
	bool							_singleItem		= false;
	
	void extraOptionsChangedSlot(Option *option);
	void updateNuisances(bool checked);

	void _fillOptionsMap(QMap<std::string, Options*>& optionsMap);
};

#endif // BOUNDQMLLISTVIEWTERMS_H
