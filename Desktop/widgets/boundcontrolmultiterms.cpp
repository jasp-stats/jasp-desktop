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

#include "boundcontrolmultiterms.h"

BoundControlMultiTerms::BoundControlMultiTerms(ListModelMultiTermsAssigned* listModel)
{
	_listModel = listModel;
}

void BoundControlMultiTerms::bindTo(Option *option)
{
	_optionVariablesGroups = dynamic_cast<OptionVariablesGroups *>(option);
	_listModel->initTerms(_optionVariablesGroups->value());
}

Option* BoundControlMultiTerms::createOption()
{
	return new OptionVariablesGroups();
}

bool BoundControlMultiTerms::isOptionValid(Option *option)
{
	return dynamic_cast<OptionVariablesGroups*>(option) != nullptr;
}

bool BoundControlMultiTerms::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}

void BoundControlMultiTerms::updateOption()
{
	const QList<Terms>& tuples = _listModel->tuples();
	std::vector<std::vector<std::string> > values;
	for (const Terms& terms : tuples)
		values.push_back(terms.asVector());
	_optionVariablesGroups->setValue(values);
}
