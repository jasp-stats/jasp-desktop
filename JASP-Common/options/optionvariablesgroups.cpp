
#include "optionvariablesgroups.h"

#include <boost/foreach.hpp>

using namespace std;

OptionVariablesGroups::OptionVariablesGroups()
{
}

Json::Value OptionVariablesGroups::asJSON() const
{
	Json::Value v = Json::arrayValue;

	BOOST_FOREACH(const vector<string> &group, _value)
	{
		Json::Value u = Json::arrayValue;

		BOOST_FOREACH(const string &variable, group)
			u.append(variable);

		v.append(u);
	}

	return v;
}

void OptionVariablesGroups::set(Json::Value &value)
{
	vector<vector<string> > groups;

	for (Json::ValueIterator itr = value.begin(); itr != value.end(); itr++)
	{
		vector<string> group;

		Json::Value &g = *itr;

		for (Json::ValueIterator gtr = g.begin(); gtr != g.end(); gtr++)
		{
			Json::Value &v = *gtr;
			string variable = v.asString();
			group.push_back(variable);
		}

		groups.push_back(group);
	}

	setValue(groups);
}

Option *OptionVariablesGroups::clone() const
{
	OptionVariablesGroups *c = new OptionVariablesGroups();
	c->setValue(value());
	return c;
}
