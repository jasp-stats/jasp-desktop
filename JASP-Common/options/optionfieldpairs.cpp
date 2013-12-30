#include "optionfieldpairs.h"

#include <boost/foreach.hpp>

using namespace Json;
using namespace std;

OptionFieldPairs::OptionFieldPairs()
{
}

Value OptionFieldPairs::asJSON() const
{
	Value v = arrayValue;

	pair<string, string> p;

	BOOST_FOREACH(p, _value)
	{
		Value pa = arrayValue;
		pa[(UInt)0] = p.first;
		pa[1] = p.second;

		v.append(pa);
	}

	return v;
}

void OptionFieldPairs::set(Value &value)
{
	vector<pair<string, string> > vec;

	for (Value::iterator itr = value.begin(); itr != value.end(); itr++)
	{
		Value v = *itr;

		pair<string, string> p;
		p.first = v[(UInt)0].asString();
		p.second = v[(UInt)1].asString();

		vec.push_back(p);
	}

	setValue(vec);
}

Option *OptionFieldPairs::clone() const
{
	OptionFieldPairs *c = new OptionFieldPairs();
	c->setValue(_value);
	return c;
}
