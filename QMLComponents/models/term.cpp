//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include "utilities/qutils.h"
#include <sstream>

const char * Term::separator =
#ifdef _WIN32
		" * ";
#else
		" \xEF\xB9\xA1 ";
#endif


Term::Term(const std::vector<std::string>	components)	{ initFrom(tq(components));		}
Term::Term(const std::string				component)	{ initFrom(tq(component));		}
Term::Term(const QStringList				components)	{ initFrom(components);			}
Term::Term(const QString					component)	{ initFrom(component);			}

void Term::initFrom(const QStringList components)
{
	_asQString	= components.join(separator);
	_components = components;
}

void Term::initFrom(const QString component)
{
	_components.append(component);
	_asQString = component;
}

const QStringList &Term::components() const
{
	return _components;
}

std::vector<std::string> Term::scomponents() const
{
	return fq(_components);
}

std::string Term::asString() const
{
	return fq(_asQString);
}

bool Term::contains(const QString &component) const
{
	for(const QString &termComponent : _components)
		if (component == termComponent)
			return true;

	return false;
}

bool Term::containsAll(const Term &term) const
{
	for(const QString &termComponent : term._components)
		if ( ! contains(termComponent))
			return false;

	return true;
}

bool Term::containsAny(const Term &term) const
{
	for(const QString &termComponent : _components)
		if (term.contains(termComponent))
			return true;

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

	return (other.size() == size()) && containsAll(other);
}

bool Term::operator!=(const Term &other) const
{
	return this->operator==(other) == false;
}

size_t Term::size() const
{
	return _components.size();
}

bool Term::replaceVariableName(const std::string & oldName, const std::string & newName)
{
	bool changed = false;
	for(int i=0; i<_components.size(); i++)
		if(_components[i] == tq(oldName))
		{
			_components[i] = tq(newName);
			changed = true;
		}

	initFrom(_components);

	return changed;
}

Term Term::readTerm(std::string str)
{
	return readTerm(tq(str));
}

Term Term::readTerm(QString str)
{
	return Term(str.split(separator));
}
