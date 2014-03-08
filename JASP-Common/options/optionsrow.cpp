#include "optionsrow.h"

#include "optionfield.h"
#include "boost/foreach.hpp"

using namespace std;

OptionsRow::OptionsRow()
{

}

Option *OptionsRow::clone() const
{
	OptionsRow *row = new OptionsRow();

	BOOST_FOREACH(const string& name, this->names)
	{
		Option *oc = get(name)->clone();
		row->add(name, oc);
	}

	return row;
}
