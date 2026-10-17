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
#include "qutils.h"

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
	//isMember/get throw on non-objects (e.g. a fuzzed non-object element inside an
	//options array), so guard the type first.
	if (!json.isObject() || !json.isMember(keyValue))
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

	//mds-plan D5.1: Storage row entry `{keyValue: {name, types, filterId?}}` - the per-term object
	//of the final file format nested under the row's optionKey (optionKey discipline kept, row
	//children stay siblings and are collected by the Terms(json) applier exactly like today). The
	//term's OWN types win over the positional fallback; its filterId rides the typed Term (C9).
	if (jsonValue.isObject() && isStorageTermObject(jsonValue))
	{
		Term parsed = fromStorageTermObject(jsonValue, types);
		initFrom(parsed.components(), label, parsed.types());
		_filterId = parsed.filterId();
		return;
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

//Identity (mds-plan C9): same components (as a set, order-free — always has been) AND the same
//source filter. -1 (unqualified) is a value like any other here: it is NOT a wildcard, a term
//that knows its filter and one that does not are different facts about the world and must not
//compare equal (that is what lets Terms keep both apart once the stamp arrives). Wildcard-ish
//matching lives in the LOOKUP helpers (Terms::containsValue/indexOfValue), not in equality.
bool Term::operator==(const Term &other) const
{
	if (this == &other)
		return true;

	return _filterId == other._filterId
		&& (other.size() == size()) && containsAll(other);
}

bool Term::operator!=(const Term &other) const
{
	return this->operator==(other) == false;
}

bool Term::operator<(const Term &other) const
{
	if (value() != other.value())
		return value() < other.value();

	//Same name, different filters: keep a strict total order so std::map<Term, ...> (e.g.
	//Terms::indexesFromTerms) does not collapse same-name-different-filter terms into one key.
	return _filterId < other._filterId;
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

//mds-plan D5.1: the Storage term object {name, filterId?, types} - see the header for the shape
//contract and the collision rules. This is the FILE serialization only (analysis.cpp's save and
//the duplicate-me parking); the Engine generator (toJson above) is today's frozen R contract
//and stays byte-identical forever.
Json::Value Term::toJsonStorage() const
{
	Json::Value result(Json::objectValue);

	if (_components.size() == 1)
		result["name"] = fq(_components[0]);
	else
	{
		Json::Value components(Json::arrayValue);
		for (const QString& component : _components)
			components.append(fq(component));
		result["name"] = components;
	}

	Json::Value types(Json::arrayValue);
	for (columnType type : _types)
		types.append(columnTypeToString(type));
	result["types"] = types;

	if (_filterId >= 0)
		result["filterId"] = _filterId;

	return result;
}

bool Term::isStorageTermObject(const Json::Value & json)
{
	if (!json.isObject() || !json.isMember("name"))
		return false;

	const Json::Value & name = json["name"];
	if (!name.isString() && !name.isArray())
		return false;
	if (name.isArray())
	{
		for (const Json::Value & component : name)
		{
			if (!component.isString())
				return false;
		}
	}

	bool hasTypes = false;
	if (json.isMember("types"))
	{
		hasTypes = true;
		const Json::Value & types = json["types"];
		if (types.isString())
		{
			//fine
		}
		else if (types.isArray())
		{
			for (const Json::Value & type : types)
			{
				if (!type.isString())
					return false;
			}
		}
		else
		{
			return false;
		}
	}

	//filterId: absent or explicit null == unqualified (null-tolerant because the QML->Json bridge
	//used to turn every JS number into null; integral reals are accepted for the same reason),
	//any other non-integral value disqualifies the shape.
	const bool hasFidMember = json.isMember("filterId");
	if (hasFidMember && !json["filterId"].isNull() && !json["filterId"].isIntegral())
		return false;
	else if (!hasFidMember && !hasTypes)
		//{name} alone is NOT claimed as a term object: factor-level rows (keyValue "name", no
		//levels yet) and other name-keyed rows keep parsing through today's row path untouched.
		//A generated Storage term object always carries `types`, so this never hides a real one.
		return false;

	for (const std::string & member : json.getMemberNames())
	{
		if (member != "name" && member != "types" && member != "filterId")
			return false;
	}

	return true;
}

Term Term::fromStorageTermObject(const Json::Value & json, const columnTypeVec & fallbackTypes)
{
	QStringList components;

	const Json::Value & name = json["name"];
	if (name.isString())
		components.push_back(tq(name.asString()));
	else
		for (const Json::Value & component : name)
			components.push_back(tq(component.asString()));

	columnTypeVec types = fallbackTypes;

	if (json.isMember("types"))
	{
		types = {};
		const Json::Value & jsonTypes = json["types"];
		if (jsonTypes.isString())
			types.push_back(columnTypeFromString(jsonTypes.asString(), columnType::unknown));
		else
			for (const Json::Value & type : jsonTypes)
				types.push_back(columnTypeFromString(type.asString(), columnType::unknown));
	}

	if (types.empty())
		types.push_back(columnType::unknown);
	while (types.size() < components.size())
		types.push_back(columnType::unknown);

	Term term(components, types);

	if (json.isMember("filterId") && !json["filterId"].isNull())
		term.setFilterId(json["filterId"].isInt() ? json["filterId"].asInt() : static_cast<int>(json["filterId"].asDouble()));

	return term;
}
