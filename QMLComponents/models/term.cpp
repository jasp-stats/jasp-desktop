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


Term::Term(const std::vector<std::string>	components, const columnTypeVec&	types)	{ initFrom(tq(components),	types);		}
Term::Term(const std::string				component,	columnType				type)	{ initFrom(tq(component),	type);		}
Term::Term(const QStringList				components, const columnTypeVec&	types)	{ initFrom(components,		types);		}
Term::Term(const QString					component,	columnType				type)	{ initFrom(component,		type);		}

void Term::initFrom(const QStringList components, const columnTypeVec& types)
{
	_asQString	= components.join(separator);
	_components = components;
	_types = types;
}

void Term::initFrom(const QString component, columnType type)
{
	_components.append(component);
	_asQString = component;
	_types = {type};
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

bool Term::operator<(const Term &other) const
{
	return asQString() < other.asQString();
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

	initFrom(_components, _types);

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

Term Term::readTerm(const Json::Value &json, columnType defaultType)
{
	Json::Value jsonValue = json;
	std::vector<std::string> components;
	columnTypeVec types;

	if (json.isObject() && json.isMember("value") && json.isMember("types"))
	{
		jsonValue = json["value"];
		Json::Value jsonType = json["types"];

		if (jsonType.isArray())
		{
			for (const Json::Value& type : jsonType)
				types.push_back(columnTypeFromString(type.asString(), columnType::unknown));
		}
		else if (jsonType.isString())
			types.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));
	}

	if (jsonValue.isArray())
	{
		for (const Json::Value& component : jsonValue)
			components.push_back(component.asString());
	}
	else if (jsonValue.isString())
		components.push_back(jsonValue.asString());

	while (types.size() < components.size())
		types.push_back(defaultType);

	return Term(components, types);
}

Json::Value Term::toJson(bool useArray, bool useValueAndType) const
{
	useArray = useArray || _components.size();
	Json::Value result, value, types;

	if (useArray)
	{
		for (const QString& component : _components)
			value.append(fq(component));
		for (columnType type : _types)
			types.append(columnTypeToString(type));
	}
	else
	{
		value = asString();
		types = columnTypeToString(type());
	}

	if (useValueAndType)
	{
		result["value"] = value;
		result["types"] = types;
	}
	else
		result = value;

	return result;
}
