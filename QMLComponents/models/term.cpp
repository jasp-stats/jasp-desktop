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

const char * Term::separator =
#ifdef _WIN32
		" * ";
#else
		" \xEF\xB9\xA1 ";
#endif

Term::Term(const std::vector<std::string>	& components,	const columnTypeVec	&	types)	{ initFrom(tq(components),	"", types);		}
Term::Term(const std::string				& value,		const columnType		type)	{ initFrom(tq(value),		"", {type});	}
Term::Term(const std::string				& value,		const columnTypeVec	&	type)	{ initFrom(tq(value),		"", type);		}
Term::Term(const QStringList				& components,	const columnTypeVec	&	types)	{ initFrom(components,		"", types);		}
Term::Term(const QString					& value,		const columnType		type)	{ initFrom(value,			"", {type});	}
Term::Term(const QString					& value,		const QString		&	label,	const QString	& info, const columnType type) { initFrom(value, label, {type}, info); }

Term::Term(const Json::Value &json, const std::string& keyValue, const std::string& keyLabel, const columnTypeVec& types)
{
	if (!json.isMember(keyValue))
	{
		initFrom("", "", {columnType::unknown});
		return;
	}

	Json::Value jsonValue = json[keyValue];
	QString label = (!keyLabel.empty() && json.isMember(keyLabel) && json[keyValue].isString()) ? tq(json[keyValue].asString()) : "";
	QStringList components;
	columnTypeVec realTypes = types;

	if (jsonValue.isObject() && jsonValue.isMember("value") && jsonValue.isMember("types"))
	{
		Json::Value jsonType = jsonValue["types"];
		jsonValue = jsonValue["value"];
		realTypes = {};

		if (jsonType.isArray())
		{
			for (const Json::Value& type : jsonType)
				realTypes.push_back(columnTypeFromString(type.asString(), columnType::unknown));
		}
		else if (jsonType.isString())
			realTypes.push_back(columnTypeFromString(jsonType.asString(), columnType::unknown));

	}

	if (jsonValue.isArray())
	{
		for (const Json::Value& component : jsonValue)
			components.push_back(tq(component.asString()));
	}
	else if (jsonValue.isString())
		components.push_back(tq(jsonValue.asString()));

	while (realTypes.size() < components.size())
		realTypes.push_back(columnType::unknown);

	initFrom(components, label, realTypes);
}

void Term::initFrom(const QStringList	& components,	const QString	& label, const columnTypeVec& types, const QString	& info)
{
	_value		= components.join(separator);
	_label		= label.isEmpty() ? _value : label;
	_components = components;
	_types		= types;
	_info		= info;
}

void Term::initFrom(const QString& value, const QString& label, const columnTypeVec& types, const QString& info)
{
	_components.append(value);
	_value		= value;
	_label		= label.isEmpty() ? _value : label;
	_types		= types.size() > 0 ? types : columnTypeVec{columnType::unknown};
	_info		= info;
}

const QStringList &Term::components() const
{
	return _components;
}

std::vector<std::string> Term::scomponents() const
{
	return fq(_components);
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
	return value() < other.value();
}


size_t Term::size() const
{
	return _components.size();
}

bool Term::replaceVariableName(const std::string & oldValue, const std::string & newValue)
{
	bool changed = false;
	for(int i=0; i<_components.size(); i++)
		if(_components[i] == tq(oldValue))
		{
			_components[i] = tq(newValue);
			changed = true;
		}

	initFrom(_components, "", _types);

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
		value = fq(this->value());
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
