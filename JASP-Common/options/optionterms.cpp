
#include "optionterms.h"
#include <boost/foreach.hpp>

using namespace std;

OptionTerms::OptionTerms()
{
	_onlyOneComponent = false;
	_onlyOneTerm = false;
}

OptionTerms::OptionTerms(bool onlyOneComponent, bool onlyOneTerm)
{
	_onlyOneComponent = onlyOneComponent;
	_onlyOneTerm = onlyOneTerm;
}

void OptionTerms::loadData(Json::Value data)
{
	_onlyOneComponent = data["onlyOneComponent"].asBool();
	_onlyOneTerm = data["onlyOneTerm"].asBool();
}

void OptionTerms::set(Json::Value &value)
{

}

Json::Value OptionTerms::asJSON() const
{
	Json::Value top;

	BOOST_FOREACH(const vector<string> &term, _value)
	{
		Json::Value row(Json::arrayValue);

		BOOST_FOREACH(const string &component, term)
		{
			row.append(component);
		}

		top.append(row);
	}

	return top;
}

Option *OptionTerms::clone() const
{
	OptionTerms *c = new OptionTerms(_onlyOneComponent, _onlyOneTerm);
	c->setValue(value());
	return c;
}

void OptionTerms::setValue(vector<vector<string> > value)
{
	if (_onlyOneComponent || _onlyOneTerm)
	{
		if (value.size() > 1)
		{
			value.erase(++value.begin(), value.end());

			if (_onlyOneComponent && _onlyOneTerm && value.front().size() > 1)
			{
				vector<string> &term = value.front();
				term.erase(++term.begin(), term.end());
			}
		}
	}

	OptionI::setValue(value);
}

void OptionTerms::setValue(vector<string> value)
{
	vector<vector<string> > terms;
	terms.push_back(value);
	setValue(terms);
}

void OptionTerms::setValue(string value)
{
	vector<string> term;
	vector<vector<string> > terms;

	term.push_back(value);
	terms.push_back(term);

	setValue(terms);
}

bool OptionTerms::onlyOneTerm() const
{
	return _onlyOneTerm;
}

bool OptionTerms::onlyOneComponent() const
{
	return _onlyOneComponent;
}
