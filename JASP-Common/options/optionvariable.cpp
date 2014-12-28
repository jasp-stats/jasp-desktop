
#include "optionvariable.h"

using namespace std;

OptionVariable::OptionVariable()
	: OptionVariables(true)
{
}

void OptionVariable::set(Json::Value &value)
{
	string asString = value.asString();

	if (asString == "")
	{
		vector<string> v;
		setValue(v);
	}
	else
	{
		vector<string> v;
		v.push_back(asString);
		setValue(v);
	}
}

Json::Value OptionVariable::asJSON() const
{
	if (_value.size() > 0 && _value.front().size() > 0)
		return Json::Value(_value.front().front());

	return Json::Value("");
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
