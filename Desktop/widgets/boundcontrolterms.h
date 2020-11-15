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

#include "analysis/options/boundcontrol.h"
#include "listmodelassignedinterface.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optionstable.h"

class CheckBoxBase;
class VariablesListBase;

class BoundControlTerms : public BoundControl
{
	
public:
	BoundControlTerms(ListModelAssignedInterface* listmodel, bool isSingleRow = false);
	
	Option*		boundTo()									override;
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;
	void		modelChanged()								override;

private:
	void		interactionHighOrderHandler(Option* option);
	
private:
	OptionVariables*				_optionVariables		= nullptr;
	OptionsTable*					_optionsTable			= nullptr;
	ListModelAssignedInterface*		_termsModel				= nullptr;
	JASPListControl*					_listView				= nullptr;
	bool							_isSingleRow			= false;
	QString							_interactionHighOrderCheckBoxName;
	std::string						_tempOptionKey;
};

#endif // BOUNDQMLLISTVIEWTERMS_H
