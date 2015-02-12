
#include "term.h"
#include "qutils.h"

#include <sstream>
#include <boost/foreach.hpp>

using namespace std;

Term::Term(const std::vector<string> components)
{
	bool first = true;

	foreach (const string &component, components)
	{
		if (first)
			first = false;
		else
			_asString.append(" \xE2\x9C\xBB ");  // star spoked asterisk

		_asString.append(component);
		_components.append(tq(component));
		_scomponents.push_back(component);
	}

	_asQString = tq(_asString);
}

Term::Term(const string component)
{
	_components.append(tq(component));
	_scomponents.push_back(component);
	_asString = component;
	_asQString = tq(component);
}

Term::Term(const QStringList components)
{
	bool first = true;

	foreach (const QString &component, components)
	{
		if (first)
			first = false;
		else
			_asQString += " : ";

		_asQString += component;
		_components.append(component);
		_scomponents.push_back(fq(component));
	}

	_asString = fq(_asQString);
}

Term::Term(const QString component)
{
	_components.append(component);
	_scomponents.push_back(fq(component));
	_asQString = component;
	_asString = fq(component);
}

const QStringList &Term::components() const
{
	return _components;
}

const std::vector<string> &Term::scomponents() const
{
	return _scomponents;
}

const string &Term::asString() const
{
	return _asString;
}

bool Term::contains(const string &component) const
{
	BOOST_FOREACH(const string &termComponent, _scomponents)
	{
		if (component == termComponent)
			return true;
	}

	return false;
}

const QString &Term::asQString() const
{
	return _asQString;
}

Term::iterator Term::begin()
{
	return _components.begin();
}

Term::iterator Term::end()
{
	return _components.end();
}

const QString &Term::at(int index) const
{
	return _components.at(index);
}

bool Term::operator==(const Term &other) const
{
	if (this == &other)
		return true;

	return components() == other.components();
}

size_t Term::size() const
{
	return _components.size();
}
