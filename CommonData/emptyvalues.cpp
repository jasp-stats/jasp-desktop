#include "emptyvalues.h"
#include "columnutils.h"
#include "log.h"
#include "jsonutilities.h"

std::string		EmptyValues::_displayString			= "";
const int		EmptyValues::missingValueInteger	= std::numeric_limits<int>::lowest();
const double	EmptyValues::missingValueDouble		= NAN;

EmptyValues::EmptyValues(EmptyValues * parent) : _parent(parent)
{
	//Log::log() << "EmptyValues::EmptyValues(" << (parent ? "a parent" : "null") << ");" << std::endl;
}

void EmptyValues::resetEmptyValues()
{
	_emptyStrings.clear();
	_emptyDoubles.clear();
}

EmptyValues::~EmptyValues()
{

}

Json::Value EmptyValues::toJson() const
{
	Json::Value		emptyVals				= Json::objectValue;
					emptyVals["strings"]	= JsonUtilities::setToJsonArray(_emptyStrings);
					emptyVals["custom"]		= _hasEmptyValues;
	return			emptyVals;
}

void EmptyValues::fromJson(const Json::Value & json)
{
	resetEmptyValues();
	
	if (!json.isObject())
		return;
	
	setEmptyValues(	JsonUtilities::jsonStringArrayToSet(json["strings"]			), 
														json["custom"].asBool()	);
}


const stringset& EmptyValues::emptyStrings() const
{
	return _emptyStrings;
}

const stringset &EmptyValues::emptyStringsColumnModel() const
{
	return !_parent ? _emptyStrings : !_hasEmptyValues ? _parent->_emptyStrings : _emptyStrings;
}

const doubleset & EmptyValues::emptyDoubles() const
{
    return _emptyDoubles;
}

void EmptyValues::setEmptyValues(const stringset &values)
{
	setEmptyValues(values, values.size());
}

void EmptyValues::setEmptyValues(const stringset& values, bool custom)
{
	_emptyStrings	= values;
	_emptyDoubles	= ColumnUtils::getDoubleValues(values);
	_hasEmptyValues	= custom;
}

bool EmptyValues::hasEmptyValues() const
{
	return !_parent || _hasEmptyValues;
}

void EmptyValues::setHasCustomEmptyValues(bool hasThem)
{
	_hasEmptyValues = hasThem;
	
	if(_parent)
		setEmptyValues(!_hasEmptyValues ? stringset() : _parent->_emptyStrings, _hasEmptyValues);
	
}

bool EmptyValues::isEmptyValue(const std::string& val) const
{
	return hasEmptyValues() ? _emptyStrings.count(val) : ( _parent && _parent->isEmptyValue(val));
}

bool EmptyValues::isEmptyValue(const double val) const
{
	return hasEmptyValues() ? std::isnan(val) || _emptyDoubles.count(val) : ( _parent && _parent->isEmptyValue(val));
}
