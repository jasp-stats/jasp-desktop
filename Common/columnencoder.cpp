//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "columnencoder.h"
#include <regex>
#include "log.h"

ColumnEncoder				*	ColumnEncoder::_columnEncoder				= nullptr;
std::set<ColumnEncoder*>	*	ColumnEncoder::_otherEncoders				= nullptr;
bool							ColumnEncoder::_encodingMapInvalidated		= true;
bool							ColumnEncoder::_decodingMapInvalidated		= true;
bool							ColumnEncoder::_originalNamesInvalidated	= true;
bool							ColumnEncoder::_encodedNamesInvalidated		= true;


ColumnEncoder * ColumnEncoder::columnEncoder()
{
	if(!_columnEncoder)
		_columnEncoder = new ColumnEncoder();

	return _columnEncoder;
}

void ColumnEncoder::invalidateAll()
{
	_encodingMapInvalidated		= true;
	_decodingMapInvalidated		= true;
	_originalNamesInvalidated	= true;
	_encodedNamesInvalidated	= true;
}

ColumnEncoder::ColumnEncoder(std::string prefix, std::string postfix)
	: _encodePrefix(prefix), _encodePostfix(postfix)
{
	if(!_otherEncoders)
	{
		_otherEncoders = new ColumnEncoder::ColumnEncoders();
		invalidateAll();
	}

	_otherEncoders->insert(this);
}

ColumnEncoder::ColumnEncoder(const std::map<std::string, std::string> & decodeDifferently)
	: _encodePrefix("JASPColumn_."), _encodePostfix("._For_Replacement")
{

	std::vector<std::string> originalNames;
	originalNames.reserve(decodeDifferently.size());

	for(const auto & oriNew : decodeDifferently)
		originalNames.push_back(oriNew.first);

	setCurrentNames(originalNames);

	for(const std::string & encodedName : _encodedNames)
		if(decodeDifferently.count(_decodingMap[encodedName]) > 0)
			_decodingMap[encodedName] = decodeDifferently.at(_decodingMap[encodedName]);
}

ColumnEncoder::~ColumnEncoder()
{
	if(this != _columnEncoder)
	{
		if(_otherEncoders && _otherEncoders->count(this) > 0) //The special "replacer-encoder" doesn't add itself to otherEncoders.
			_otherEncoders->erase(this);
	}
	else
	{
		_columnEncoder = nullptr;

		ColumnEncoders others = *_otherEncoders;

		for(ColumnEncoder * colEnc : others)
			delete colEnc;

		if(_otherEncoders->size() > 0)
			Log::log() << "Something went wrong removing other ColumnEncoders..." << std::endl;

		delete _otherEncoders;
		_otherEncoders = nullptr;

		invalidateAll();
	}
}

std::string ColumnEncoder::encode(const std::string &in)
{
	if(in == "") return "";

	if(_encodingMap.count(in) == 0)
		throw std::runtime_error("Trying to encode columnName but '" + in + "' is not a columnName!");

	return _encodingMap[in];
}

std::string ColumnEncoder::decode(const std::string &in)
{
	if(in == "") return "";

	if(_decodingMap.count(in) == 0)
		throw std::runtime_error("Trying to decode columnName but '" + in + "' is not an encoded columnName!");

	return _decodingMap[in];
}

void ColumnEncoder::setCurrentNames(const std::vector<std::string> & names)
{
	_encodingMap.clear();
	_decodingMap.clear();

	_encodedNames.clear();
	_encodedNames.reserve(names.size());

	for(size_t col = 0; col < names.size(); col++)
	{
		std::string newName			= _encodePrefix + std::to_string(col) + _encodePostfix; //Slightly weird (but R-syntactically valid) name to avoid collisions with user stuff.
		_encodingMap[names[col]]	= newName;
		_decodingMap[newName]		= names[col];

		_encodedNames.push_back(newName);
	}

	_originalNames = names;
	sortVectorBigToSmall(_originalNames);
	invalidateAll();
}

void ColumnEncoder::sortVectorBigToSmall(std::vector<std::string> & vec)
{
	std::sort(vec.begin(), vec.end(), [](std::string & a, std::string & b) { return a.size() > b.size(); }); //We need this to make sure smaller columnNames do not bite chunks off of larger ones
}

const ColumnEncoder::colMap	&	ColumnEncoder::encodingMap()
{
	static ColumnEncoder::colMap map;

	if(_encodingMapInvalidated)
	{
		map = _columnEncoder->_encodingMap;

		if(_otherEncoders)
			for(const ColumnEncoder * other : *_otherEncoders)
				for(const auto & keyVal : other->_encodingMap)
					if(map.count(keyVal.first) == 0)
						map[keyVal.first] = keyVal.second;

		_encodingMapInvalidated = false;
	}

	return map;
}

const ColumnEncoder::colMap	&	ColumnEncoder::decodingMap()
{
	static ColumnEncoder::colMap map;

	if(_decodingMapInvalidated)
	{
		map = _columnEncoder->_decodingMap;

		if(_otherEncoders)
			for(const ColumnEncoder * other : *_otherEncoders)
				for(const auto & keyVal : other->_decodingMap)
					if(map.count(keyVal.first) == 0)
						map[keyVal.first] = keyVal.second;

		_decodingMapInvalidated = false;
	}

	return map;
}

const ColumnEncoder::colVec	&	ColumnEncoder::originalNames()
{
	static ColumnEncoder::colVec vec;

	if(_originalNamesInvalidated)
	{
		vec = _columnEncoder->_originalNames;

		if(_otherEncoders)
			for(const ColumnEncoder * other : *_otherEncoders)
				for(const std::string name : other->_originalNames)
					vec.push_back(name);

		_originalNamesInvalidated = false;
	}

	sortVectorBigToSmall(vec);

	return vec;
}

const ColumnEncoder::colVec	&	ColumnEncoder::encodedNames()
{
	static ColumnEncoder::colVec vec;

	if(_encodedNamesInvalidated)
	{
		vec = _columnEncoder->_encodedNames;

		if(_otherEncoders)
			for(const ColumnEncoder * other : *_otherEncoders)
				for(const std::string name : other->_encodedNames)
					vec.push_back(name);

		_encodedNamesInvalidated = false;
	}

	sortVectorBigToSmall(vec);

	return vec;
}

bool ColumnEncoder::shouldEncode(const std::string & in)
{
	return _encodingMap.count(in) > 0;
}

bool ColumnEncoder::shouldDecode(const std::string & in)
{
	return _decodingMap.count(in) > 0;
}

std::string	ColumnEncoder::replaceAll(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names)
{		
	size_t foundPos = 0;

	while(foundPos < std::string::npos)
	{
		size_t firstFoundPos	= std::string::npos;

		std::string replaceThis;

		//First we find the first occurence of a replaceable text.
		for(const std::string & replaceMe : names) //We follow names instead of keyvals from map because they ought to be sorted from largest to smallest string (_originalNames) to not make sub-replacements
		{
			size_t pos = text.find(replaceMe, foundPos);
			if(pos < firstFoundPos)
			{
				firstFoundPos = pos;
				replaceThis = replaceMe;
			}
		}

		//We found something to replace and this will be the first occurence of anything like that. Replace it!
		if(firstFoundPos != std::string::npos)
		{
			foundPos = firstFoundPos;
			const std::string & replacement = map.at(replaceThis);
			text.replace(foundPos, replaceThis.length(), replacement);
			foundPos += replacement.length(); //Let's make sure we start replacing from after where we just replaced
		}
		else
			foundPos = std::string::npos;
	}

	return text;
}

std::string ColumnEncoder::encodeRScript(std::string text, std::set<std::string> * columnNamesFound)
{
	return encodeRScript(text, encodingMap(), originalNames(), columnNamesFound);
}

std::string ColumnEncoder::encodeRScript(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, std::set<std::string> * columnNamesFound)
{
	if(columnNamesFound)
		columnNamesFound->clear();

	static std::regex nonNameChar("[^\\.A-Za-z0-9_]");

	//for now we simply replace any found columnname by its Base64 variant if found
	for(const std::string & oldCol : names)
	{
		std::string	newCol	= map.at(oldCol);

		std::vector<size_t> foundColPositions = getPositionsColumnNameMatches(text, oldCol);
		std::reverse(foundColPositions.begin(), foundColPositions.end());

		for (size_t foundPos : foundColPositions)
		{
			size_t foundPosEnd = foundPos + oldCol.length();

			//First check if it is a "free columnname" aka is there some space or a kind in front of it. We would not want to replace a part of another term (Imagine what happens when you use a columname such as "E" and a filter that includes the term TRUE, it does not end well..)
			bool startIsFree	= foundPos == 0					|| std::regex_match(text.substr(foundPos - 1, 1),	nonNameChar);
			bool endIsFree		= foundPosEnd == text.length()	|| std::regex_match(text.substr(foundPosEnd, 1),	nonNameChar);

			//Check for "(" as well because maybe someone has a columnname such as rep or if or something weird like that. This might however have some whitespace in between...
			bool keepGoing = true;

			for(size_t bracePos = foundPosEnd; bracePos < text.size() && endIsFree && keepGoing; bracePos++)
				if(text[bracePos] == '(')
					endIsFree = false;
				else if(text[bracePos] != '\t' && text[bracePos] != ' ')
					keepGoing = false; //Aka something else than whitespace or a brace and that means that we can replace it!

			if(startIsFree && endIsFree)
			{
				text.replace(foundPos, oldCol.length(), newCol);

				if(columnNamesFound)
					columnNamesFound->insert(oldCol);
			}
		}
	}

	return text;
}

std::vector<size_t> ColumnEncoder::getPositionsColumnNameMatches(const std::string & text, const std::string & columnName)
{
	std::vector<size_t> positions;

	bool inString	= false;
	char delim		= '?';

	for (std::string::size_type pos = 0; pos < text.length(); ++pos)
		if (!inString && text.substr(pos, columnName.length()) == columnName)
			positions.push_back(int(pos));
		else if (text[pos] == '"' || text[pos] == '\'') //string starts or ends. This does not take into account escape characters though...
		{
			if (!inString)
			{
				delim		= text[pos];
				inString	= true;
			}
			else if(text[pos] == delim)
				inString = false;
		}

	return positions;
}

void ColumnEncoder::encodeJson(Json::Value & json, bool replaceNames)
{
	//std::cout << "Json before encoding:\n" << json.toStyledString();
	replaceAll(json, encodingMap(), originalNames(), replaceNames);
	//std::cout << "Json after encoding:\n" << json.toStyledString() << std::endl;
}

void ColumnEncoder::decodeJson(Json::Value & json, bool replaceNames)
{
	//std::cout << "Json before encoding:\n" << json.toStyledString();
	replaceAll(json, decodingMap(), encodedNames(), replaceNames);
	//std::cout << "Json after encoding:\n" << json.toStyledString() << std::endl;
}


void ColumnEncoder::replaceAll(Json::Value & json, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, bool replaceNames)
{
	switch(json.type())
	{
	case Json::arrayValue:
		for(Json::Value & option : json)
			replaceAll(option, map, names, replaceNames);
		return;

	case Json::objectValue:
	{
		std::map<std::string, std::string> changedMembers;

		for(const std::string & optionName : json.getMemberNames())
		{
			replaceAll(json[optionName], map, names, replaceNames);

			if(replaceNames)
			{
				std::string replacedName = replaceAll(optionName, map, names);

				if(replacedName != optionName)
					changedMembers[optionName] = replacedName;
			}
		}

		for(const auto & origNew : changedMembers) //map is empty if !replaceNames
		{
			json[origNew.second] = json[origNew.first];
			json.removeMember(origNew.first);
		}

		return;
	}

	case Json::stringValue:
			json = replaceAll(json.asString(), map, names);
		return;

	default:
		return;
	}
}

void ColumnEncoder::setCurrentNamesFromOptionsMeta(const std::string & optionsStr)
{
	Json::Value options;
	Json::Reader().parse(optionsStr, options);

	std::vector<std::string> namesFound;

	if(options.isMember(".meta"))
		collectExtraEncodingsFromMetaJson(options[".meta"], namesFound);

	setCurrentNames(namesFound);
}

void ColumnEncoder::collectExtraEncodingsFromMetaJson(const Json::Value & json, std::vector<std::string> & namesCollected) const
{
	switch(json.type())
	{
	case Json::arrayValue:
		for(const Json::Value & option : json)
			collectExtraEncodingsFromMetaJson(option, namesCollected);
		return;

	case Json::objectValue:
		if(json.isMember("encodeThis"))
		{
			if(json["encodeThis"].isString())		namesCollected.push_back(json["encodeThis"].asString());
			else if(json["encodeThis"].isArray())
				for(const Json::Value & enc : json["encodeThis"])
					namesCollected.push_back(enc.asString());
		}
		else
			for(const std::string & optionName : json.getMemberNames())
			collectExtraEncodingsFromMetaJson(json[optionName], namesCollected);
		return;

	default:
		return;
	}
}

std::string ColumnEncoder::removeColumnNamesFromRScript(const std::string & rCode, const std::vector<std::string> & colsToRemove)
{
	std::map<std::string, std::string> replaceBy;

	for(const std::string & col : colsToRemove)
		replaceBy[col] = "stop('column " + col + " was removed from this RScript')";

	return replaceColumnNamesInRScript(rCode, replaceBy);
}

std::string ColumnEncoder::replaceColumnNamesInRScript(const std::string & rCode, const std::map<std::string, std::string> & changedNames)
{
	//Ok the trick here is to reuse the encoding code, we will first encode the original names and then change the encodings to point back to the replaced names.
	ColumnEncoder tempEncoder(changedNames);

	return
		tempEncoder.replaceAll(
			tempEncoder.encodeRScript(
				rCode,
				tempEncoder._encodingMap,
				tempEncoder._originalNames
			),
			tempEncoder._decodingMap,
			tempEncoder._encodedNames
		);
}

ColumnEncoder::colVec ColumnEncoder::columnNames()
{
	return _columnEncoder ? _columnEncoder->_originalNames : colVec();
}

ColumnEncoder::colVec ColumnEncoder::columnNamesEncoded()
{
	return _columnEncoder ? _columnEncoder->_encodedNames : colVec();
}
