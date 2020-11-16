//
// Copyright (C) 2013-2020 University of Amsterdam
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

#ifndef BOUNDCONTROLMULTITERMS_H
#define BOUNDCONTROLMULTITERMS_H

#include "analysis/options/boundcontrol.h"
#include "listmodelassignedinterface.h"
#include "analysis/options/optionvariablesgroups.h"
#include "listmodelmultitermsassigned.h"


class BoundControlMultiTerms : public BoundControl
{
public:
	BoundControlMultiTerms(ListModelMultiTermsAssigned* listModel);

	Option*		boundTo()									override	{ return _optionVariablesGroups; }
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;
	void		updateOption()								override;


private:
	OptionVariablesGroups*			_optionVariablesGroups	= nullptr;
	ListModelMultiTermsAssigned*	_listModel;
};

#endif // BOUNDCONTROLMULTITERMS_H
