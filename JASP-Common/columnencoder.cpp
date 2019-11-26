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

std::map<std::string, std::string>	ColumnEncoder::_encodingMap;
std::map<std::string, std::string>	ColumnEncoder::_decodingMap;
std::vector<std::string>			ColumnEncoder::_originalNames;
std::vector<std::string>			ColumnEncoder::_encodedNames;

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

void ColumnEncoder::setCurrentColumnNames(const std::vector<std::string> & names)
{
	_encodingMap.clear();
	_decodingMap.clear();

	_encodedNames.clear();
	_encodedNames.reserve(names.size());

	for(size_t col = 0; col < names.size(); col++)
	{
		std::string newName			= "JaspColumn_." + std::to_string(col) + "._Encoded"; //Slightly weird (but R-syntactically valid) name to avoid collisions with user stuff.
		_encodingMap[names[col]]	= newName;
		_decodingMap[newName]		= names[col];

		_encodedNames.push_back(newName);
	}

	_originalNames = names;
	std::sort(_originalNames.begin(), _originalNames.end(), [](std::string & a, std::string & b) { return a.size() > b.size(); }); //We need this to make sure smaller columnNames do not bite chunks off of larger ones
}

bool ColumnEncoder::isColumnName(const std::string & in)
{
	return _encodingMap.count(in) > 0;
}

bool ColumnEncoder::isEncodedColumnName(const std::string & in)
{
	return _decodingMap.count(in) > 0;
}

std::string	ColumnEncoder::replaceAll(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names)
{
	for(std::string replaceMe : names) //We follow names instead of keyvals from map because they might be sorted from largest to smallest string (_originalNames) to no make sub-replacements
	{
		size_t		foundPos	= 0;
		std::string replacement = map.at(replaceMe);

		while((foundPos = text.find(replaceMe, 0)) != std::string::npos)
			text.replace(foundPos, replaceMe.length(), replacement);
	}

	return text;
}

std::string ColumnEncoder::encodeRScript(std::string text, std::set<std::string> * columnNamesFound)
{
	if(columnNamesFound)
		columnNamesFound->clear();

	static std::regex nonNameChar("[^\\.A-Za-z0-9_]");

	//for now we simply replace any found columnname by its Base64 variant if found
	for(const std::string & oldCol : _originalNames)
	{
		std::string	newCol	= _encodingMap.at(oldCol);

		std::vector<size_t> foundColPositions = getPositionsColumnNameMatches(text, oldCol);
		std::reverse(foundColPositions.begin(), foundColPositions.end());

		for (size_t foundPos : foundColPositions)
		{
			size_t foundPosEnd = foundPos + oldCol.length();

			//First check if it is a "free columnname" aka is there some space or a kind in front of it. We would not want to replace a part of another term (Imagine what happens when you use a columname such as "E" and a filter that includes the term TRUE, it does not end well..)
			bool startIsFree	= foundPos == 0					|| std::regex_match(text.substr(foundPos - 1, 1),	nonNameChar);
			bool endIsFree		= foundPosEnd == text.length()	|| (std::regex_match(text.substr(foundPosEnd, 1),	nonNameChar) && text[foundPosEnd] != '('); //Check for "(" as well because maybe someone has a columnname such as rep or if or something weird like that

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
	replaceAll(json, _encodingMap, _originalNames, replaceNames);
	//std::cout << "Json after encoding:\n" << json.toStyledString() << std::endl;
}

void ColumnEncoder::decodeJson(Json::Value & json, bool replaceNames)
{
	//std::cout << "Json before encoding:\n" << json.toStyledString();
	replaceAll(json, _decodingMap, _encodedNames, replaceNames);
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



