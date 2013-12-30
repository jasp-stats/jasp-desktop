#include "optionsrow.h"

#include "optionfield.h"
#include "boost/foreach.hpp"

using namespace std;

OptionsRow::OptionsRow(std::string name)
{
	_field = new OptionField();
	_field->setValue(name);

	add("variable", _field);
}

std::string OptionsRow::variable() const
{
	return _field->value()[0];
}

void OptionsRow::setVariable(string name)
{
	_field->setValue(name);
}

Option *OptionsRow::clone() const
{
	OptionsRow *row = new OptionsRow(variable());

	BOOST_FOREACH(const string& name, this->names)
	{
		if (name == "variable")
			continue;

		Option *oc = get(name)->clone();
		row->add(name, oc);
	}

	return row;
}
