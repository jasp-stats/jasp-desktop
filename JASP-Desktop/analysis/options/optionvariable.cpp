//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "optionvariable.h"

using namespace std;

void OptionVariable::set(const Json::Value &value)
{
	vector<string> v;

	if (value.isString())
	{
		string asString = value.asString();
		if (asString != "")
			v.push_back(asString);
	}

	setValue(v);
}

Json::Value OptionVariable::asJSON() const
{
	if (_value.size() > 0 && _value.front().size() > 0)
		return Json::Value(_value.front().front());

	return Json::Value("");
}

Json::Value OptionVariable::asMetaJSON() const
{
	return defaultMetaEntryContainingColumn();
}

Option *OptionVariable::clone() const
{
	OptionVariable *c = new OptionVariable();
	c->setValue(this->value());
	return c;
}

string OptionVariable::variable() const
{
	if (_value.size() > 0 && _value.front().size() > 0)
		return _value.front().front();
	else
		return "";
}

void  OptionVariable::removeUsedVariable(const std::string & var)
{
	bool iContainVar = false;
	for(std::string v : variables())
		if(v == var)
		{
			iContainVar = true;
			break;
		}

	if(iContainVar)
	{
		Json::Value nuller(Json::nullValue);
		set(nuller);
	}

}
