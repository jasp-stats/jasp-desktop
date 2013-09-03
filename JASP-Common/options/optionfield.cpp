#include "optionfield.h"

using namespace std;

OptionField::OptionField(string name)
	: OptionFields(name)
{
}

void OptionField::set(Json::Value &value)
{
	vector<string> v;
	v.push_back(value.asString());
	setValue(v);
}

void OptionField::setValue(std::vector<string> value)
{
	if (value.size() > 0)
	{
		vector<string> onlyOne;
		onlyOne.push_back(value.at(0));
		OptionFields::setValue(onlyOne);
	}
	else
	{
		OptionFields::setValue(value);
	}
}

Json::Value OptionField::asJSON() const
{
	if (_value.size() > 0)
		return Json::Value(_value.at(0));

	return Json::Value();
}
