//
// Copyright (C) 2013-2017 University of Amsterdam
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
#ifdef __WIN32__
			_asString.append(" * ");
#else
			_asString.append(" \xE2\x88\x97 ");
#endif

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
#ifdef __WIN32__
			_asQString.append(" * ");
#else
			_asQString.append(" \xE2\x88\x97 ");
#endif

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

bool Term::containsAll(const Term &term) const
{
	BOOST_FOREACH(const string &termComponent, term._scomponents)
	{
		if ( ! contains(termComponent))
			return false;
	}

	return true;
}

bool Term::containsAny(const Term &term) const
{
	BOOST_FOREACH(const string &termComponent, _scomponents)
	{
		if (term.contains(termComponent))
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

bool Term::operator!=(const Term &other) const
{
	return this->operator==(other) == false;
}

size_t Term::size() const
{
	return _components.size();
}
