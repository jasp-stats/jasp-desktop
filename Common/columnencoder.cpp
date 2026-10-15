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
#include "stringutils.h"
#include <algorithm>
#include <regex>
#ifdef BUILDING_JASP
#include "log.h"
#define LOGGER Log::log()
#else
#include <Rcpp.h>
#define LOGGER Rcpp::Rcout
#endif



ColumnEncoder				*	ColumnEncoder::_columnEncoder				= nullptr;
ColumnEncoder				*	ColumnEncoder::_currentEncoder				= nullptr;
std::set<ColumnEncoder*>	*	ColumnEncoder::_otherEncoders				= nullptr;

/// The ONE process-global strict decode table (C4): full encoded string -> (originalName, type).
/// Contains one block per live filter of every loaded dataset, plus the extras encoder's block
/// and the process-global fallback encoder's block. Single writer: rebuildBlock(), which erases
/// the block's bounded key space (its prefix x mint indices) before writing, so rename re-mints,
/// column-count shrinks and filter drops all close over every token the block could ever have
/// minted. That makes stale tokens impossible without tombstones (C16: deleted-filter tokens go
/// raw because their block is erased).
std::map<std::string, ColumnEncoder::DecodeEntry> & ColumnEncoder::_decodeTable()
{
	static std::map<std::string, DecodeEntry> table;
	return table;
}


ColumnEncoder * ColumnEncoder::columnEncoder()
{
	//This static getter is the ENGINE-ONLY entry point for the R bridge encoder context, set by
	//setCurrentEncoder() in provideAndUpdateDataSet - after mds-plan Phase 1 that is a VIEW (the
	//shown dataset's shown filter's handle), never a canonical. Desktop code must use
	//provider->columnEncoder() (the filter's own view) which reads from the instance directly.
	if(!_currentEncoder)
	{
		if(!_columnEncoder)
			_columnEncoder = new ColumnEncoder();
		_currentEncoder = _columnEncoder;
	}
	return _currentEncoder;
}

ColumnEncoder * ColumnEncoder::currentEncoder()
{
	return _currentEncoder;
}

ColumnEncoder * ColumnEncoder::fallbackEncoder()
{
	if(!_columnEncoder)
		_columnEncoder = new ColumnEncoder();

	return _columnEncoder;
}

ColumnEncoder * ColumnEncoder::createCanonical()
{
	return new ColumnEncoder(CanonicalTag{});
}

ColumnEncoder * ColumnEncoder::createView(ColumnEncoder * canonical, const std::string & prefix, const std::string & postfix)
{
	return new ColumnEncoder(canonical, prefix, postfix);
}

void ColumnEncoder::setCurrentEncoder(ColumnEncoder * encoder)
{
	_currentEncoder = encoder;
}

ColumnEncoder::ColumnEncoder(std::string prefix, std::string postfix)
	: _encodePrefix(prefix), _encodePostfix(postfix)
{
	if(!_otherEncoders)
		_otherEncoders = new ColumnEncoder::ColumnEncoders();

	_otherEncoders->insert(this);
}

ColumnEncoder::ColumnEncoder(CanonicalTag)
	: _mode(Mode::canonical), _encodePrefix(""), _writesBlock(false)
{
}

ColumnEncoder::ColumnEncoder(ColumnEncoder * canonical, std::string prefix, std::string postfix)
	: _mode(Mode::view), _canonical(canonical), _encodePrefix(prefix), _encodePostfix(postfix)
{
	if(_canonical)
		_canonical->_views.insert(this);

	rebuildBlock();		// mint the block from whatever the canonical holds right now
}

ColumnEncoder::ColumnEncoder(const std::map<std::string, std::string> & decodeDifferently)
	: _encodePrefix("JASPColumn_"), _encodePostfix("_For_Replacement"), _writesBlock(false)
{
	// The replacer-encoder (C5): registers nowhere - not in the decode table and not in
	// _otherEncoders - it only exists inside replaceColumnNamesInRScript() and its private maps
	// are hacked to point back at the replaced names, so it must not leak into anything global.

	colTypeMap originalNames;

	for(const auto & oriNew : decodeDifferently)
		originalNames[oriNew.first] = columnType::unknown;

	setCurrentNames(originalNames);

	for(const std::string & encodedName : _encodedNames)
		if(decodeDifferently.count(_decodingMap[encodedName]) > 0)
			_decodingMap[encodedName] = decodeDifferently.at(_decodingMap[encodedName]);
}

ColumnEncoder::~ColumnEncoder()
{
	//Whatever the current indirection points at, if it was us, clear it BEFORE the block
	//teardown below: otherwise a later columnEncoder()/isColumnName() use dereferences a
	//destroyed encoder. (Needed for non-default encoders too, e.g. a per-dataset view destroyed
	//while current; DataSet also clears explicitly, but this covers every other owner.)
	if(_currentEncoder == this)
		_currentEncoder = nullptr;

	//A dying encoder first erases and detaches every live view (their tokens go raw, C16) so the
	//views' destructors never touch freed memory - DataSet deletes its canonical (or, during the
	//Phase-1 transition, its still-FULL encoder) before its filters die as QObject children.
	orphanViews();

	if(_mode == Mode::view && _canonical)
		_canonical->_views.erase(this);

	eraseBlock();

	if(this != _columnEncoder)
	{
		if(_otherEncoders && _otherEncoders->count(this) > 0) //The special "replacer-encoder" doesn't add itself to otherEncoders.
			_otherEncoders->erase(this);
	}
	else
	{
		_columnEncoder = nullptr;

		if(_otherEncoders)
		{
			ColumnEncoders others = *_otherEncoders;

			for(ColumnEncoder * colEnc : others)
				delete colEnc;

			if(_otherEncoders->size() > 0)
				LOGGER << "Something went wrong removing other ColumnEncoders..." << std::endl;

			delete _otherEncoders;
			_otherEncoders = nullptr;
		}
	}
}

void ColumnEncoder::orphanViews()
{
	ColumnEncoders views = _views;
	_views.clear();

	for(ColumnEncoder * view : views)
	{
		view->eraseBlock();
		view->_canonical = nullptr;
	}
}

void ColumnEncoder::rebuildBlock()
{
	// THE block writer (C4): erase the bounded key space of the previous write, then write.
	// The key space is enumerable from the mint: prefix + index + postfix for every index the
	// counter ever handed out (dense [0, mintSize)), so erase-then-write is closed under
	// rename re-mints, shrinking column counts and prefix reforges.
	if(!_writesBlock)
		return;

	eraseBlock();

	const std::vector<std::pair<std::string, columnType>> & reverse = mintReverse();

	for(size_t i = 0; i < reverse.size(); ++i)
		_decodeTable()[_encodePrefix + std::to_string(i) + _encodePostfix] = DecodeEntry{reverse[i].first, reverse[i].second};

	_blockCount		= reverse.size();
	_blockPrefix	= _encodePrefix;
	_blockPostfix	= _encodePostfix;
}

void ColumnEncoder::eraseBlock()
{
	if(!_writesBlock)
		return;

	for(size_t i = 0; i < _blockCount; ++i)
		_decodeTable().erase(_blockPrefix + std::to_string(i) + _blockPostfix);

	_blockCount = 0;
}

std::string ColumnEncoder::decodeAll(const std::string & text)
{
	colMap map;
	colVec names;

	for(const auto & tokenEntry : _decodeTable())
	{
		map [tokenEntry.first] = tokenEntry.second.name;
		names.push_back(tokenEntry.first);
	}

	sortVectorBigToSmall(names);
	return replaceAll(text, map, names);
}

void ColumnEncoder::decodeJson(Json::Value & json, bool replaceNames, bool safeHtml)
{
	colMap map;
	colVec names;

	for(const auto & tokenEntry : _decodeTable())
	{
		map [tokenEntry.first] = safeHtml ? stringUtils::escapeHtmlStuff(tokenEntry.second.name, true) : tokenEntry.second.name;	// escape square brackets for https://github.com/jasp-stats/jasp-issues/issues/2625
		names.push_back(tokenEntry.first);
	}

	sortVectorBigToSmall(names);
	replaceAll(json, map, names, replaceNames, false);
}

void ColumnEncoder::resetForTests()
{
	//Forget every block. Live instances still believe they wrote one (_blockCount), so their next
	//eraseBlock/rebuildBlock removes or rewrites whatever occupies their namespace - use this only
	//between fully independent test scenarios, before the instances re-mint.
	_decodeTable().clear();
}

std::string ColumnEncoder::encode(const std::string &in)
{
	if(in == "") return "";

	if(_mode == Mode::canonical)
		throw std::runtime_error("ColumnEncoder::encode called on a canonical mint, the canonical answers no full-string encode (mds-plan C2)!");

	const auto & forward = mintForward();
	auto found = forward.find(in);

	if(found == forward.end())
		throw std::runtime_error("Trying to encode columnName but '" + in + "' is not a columnName!");

	return _encodePrefix + std::to_string(found->second) + _encodePostfix;
}

void ColumnEncoder::setCurrentNames(const std::vector<std::string> & names, bool generateTypesEncoding)
{
	//This will not give the desired result for data from the dataset, but setCurrentColumnTypePerName will fix it!
	//This function is only meant to make sure old versions of jaspBase still work

	colTypeMap map;

	for(auto & name : names)
		map[name] = columnType::unknown;

	setCurrentNames(map);
}

void ColumnEncoder::setCurrentColumnTypePerName(const colTypeMap &theMap)
{
	setCurrentNames(theMap);
}

void ColumnEncoder::buildMint(const colTypeMap & namesWithTypes)
{
	_dataSetTypes	= namesWithTypes;
	_mintForward.clear();
	_mintReverse.clear();
	_mintReverse.reserve(namesWithTypes.size() * 3);

	size_t runningCounter = 0;

	for (const auto & nameType : namesWithTypes)
	{
		//namesWithTypes either comes from the data and has a columnType specified and otherwise it is a level or something. With unknown columnType.

		if(nameType.second == columnType::unknown)
		{
			_mintForward[nameType.first]	= runningCounter;
			_mintReverse.push_back({nameType.first, columnType::unknown});
			++runningCounter;
		}
		else //It is a column from (the) data
			for(columnType colType : { columnType::scale, columnType::ordinal, columnType::nominal })
			{
				std::string qualifiedName	= nameType.first + "." + columnTypeToString(colType);
				_mintForward[qualifiedName]	= runningCounter;
				_mintReverse.push_back({nameType.first, colType});	//Decoding is back to the actual name in the data!
				++runningCounter;

				if (colType == nameType.second) //This is the type as specified in the data, so the name without specification of type gets encoded to its default type
					_mintForward[nameType.first] = runningCounter - 1;
			}
	}
}

void ColumnEncoder::materializeMaps()
{
	//FULL only: derive the legacy full-string maps from the mint + own prefix. Byte-identical to
	//what the pre-Phase-1 setCurrentNames() used to build directly.
	_originalNames.clear();
	_encodingMap.clear();
	_decodingMap.clear();
	_decodingTypes.clear();
	_encodedNames.clear();
	_encodedNames.reserve(_mintReverse.size());

	for(const auto & nameIndex : _mintForward)
	{
		_originalNames.push_back(nameIndex.first);
		_encodingMap[nameIndex.first] = _encodePrefix + std::to_string(nameIndex.second) + _encodePostfix;
	}

	for(size_t i = 0; i < _mintReverse.size(); ++i)
	{
		std::string encodedName = _encodePrefix + std::to_string(i) + _encodePostfix;

		_decodingMap[encodedName]	= _mintReverse[i].first;
		_encodedNames.push_back(encodedName);

		if(_mintReverse[i].second != columnType::unknown)
			_decodingTypes[encodedName] = _mintReverse[i].second;
	}

	sortVectorBigToSmall(_originalNames);
	sortVectorBigToSmall(_encodedNames);
}

void ColumnEncoder::setCurrentNames(const colTypeMap & namesWithTypes)
{
	switch(_mode)
	{
	case Mode::view:
		//A view has no mint of its own: the legacy ColumnEncoderContext replay path (and anything
		//else that re-points the current encoder at a view and renames through it) funnels into
		//the canonical - exactly the blast radius the dataset's own encoder used to have.
		if(_canonical)
			_canonical->setCurrentNames(namesWithTypes);
		return;

	case Mode::canonical:
		buildMint(namesWithTypes);
		for(ColumnEncoder * view : _views)			//every live view's block is a consequence of this mint (D1.4 funnel)
			view->rebuildBlock();
		return;

	case Mode::full:
		buildMint(namesWithTypes);
		materializeMaps();
		rebuildBlock();
		for(ColumnEncoder * view : _views)		//a FULL instance may still carry views (transitional or ad-hoc setups): their blocks follow the mint too
			view->rebuildBlock();
		return;
	}
}

void ColumnEncoder::setEncodePrefix(const std::string & prefix)
{
	switch(_mode)
	{
	case Mode::canonical:
		return;	//the canonical owns no namespace; reforging it is a no-op by construction

	case Mode::view:
		_encodePrefix = prefix;
		rebuildBlock();		//erase-then-write moves the whole block into the new namespace
		return;

	case Mode::full:
		_encodePrefix = prefix;
		setCurrentNames(currentNames());	//same contract as before Phase 1: reforge = remint
		return;
	}
}

void ColumnEncoder::updateColumnTypesOnly(const colTypeMap &namesWithTypes)
{
	if(_mode == Mode::view)
	{
		if(_canonical)
			_canonical->updateColumnTypesOnly(namesWithTypes);
		return;
	}

	//updateColumnTypesOnly never mints new tokens, so no view block needs a rewrite: the
	//index -> (name, type) space that the blocks are built from is untouched.
	_dataSetTypes = namesWithTypes;

	//The reverse mint (and therefore every decode block) is untouched: a type-only change never
	//mints new tokens, it only re-points the bare-name default of the encode side.
	for (const auto & nameType : namesWithTypes)
		if(nameType.second != columnType::unknown)
		{
			std::string qualifiedName		= nameType.first + "." + columnTypeToString(nameType.second);

			auto qualified = _mintForward.find(qualifiedName);
			if(qualified == _mintForward.end())
				continue;	//never minted (e.g. names-only remint via the deprecated overload): skip instead of the old behaviour of mapping the bare name to ""

			_mintForward[nameType.first] = qualified->second;

			if(_mode == Mode::full)
				_encodingMap[nameType.first] = _encodePrefix + std::to_string(qualified->second) + _encodePostfix;
		}
}

void ColumnEncoder::sortVectorBigToSmall(std::vector<std::string> & vec)
{
	std::sort(vec.begin(), vec.end(), [](std::string & a, std::string & b) { return a.size() > b.size(); }); //We need this to make sure smaller columnNames do not bite chunks off of larger ones
}

ColumnEncoder::colMap ColumnEncoder::ownEncodingMap() const
{
	if(_mode == Mode::canonical)
		throw std::runtime_error("ColumnEncoder: encode-side map requested on a canonical mint, it answers no full-string encode (mds-plan C2)!");

	if(_mode == Mode::full)
		return _encodingMap;

	colMap map;	//view: compose canonical index + own prefix on the fly, nothing is stored (C3)

	for(const auto & nameIndex : mintForward())
		map[nameIndex.first] = _encodePrefix + std::to_string(nameIndex.second) + _encodePostfix;

	return map;
}

ColumnEncoder::colVec ColumnEncoder::ownOriginalNames() const
{
	if(_mode == Mode::canonical)
		throw std::runtime_error("ColumnEncoder: encode-side names requested on a canonical mint, it answers no full-string encode (mds-plan C2)!");

	if(_mode == Mode::full)
		return _originalNames;

	colVec names;

	for(const auto & nameIndex : mintForward())
		names.push_back(nameIndex.first);

	sortVectorBigToSmall(names);
	return names;
}

ColumnEncoder::colVec ColumnEncoder::ownEncodedNames() const
{
	if(_mode == Mode::canonical)
		throw std::runtime_error("ColumnEncoder: encoded names requested on a canonical mint, it answers no full-string encode (mds-plan C2)!");

	if(_mode == Mode::full)
		return _encodedNames;

	colVec names;
	const auto & reverse = mintReverse();
	names.reserve(reverse.size());

	for(size_t i = 0; i < reverse.size(); ++i)
		names.push_back(_encodePrefix + std::to_string(i) + _encodePostfix);

	sortVectorBigToSmall(names);
	return names;
}

ColumnEncoder::colMap ColumnEncoder::encodingMap(const ColumnEncoder * self)
{
	colMap map = self->ownEncodingMap();

	if(_otherEncoders)
		for(const ColumnEncoder * other : *_otherEncoders)
			if(other != self)
				for(const auto & keyVal : other->_encodingMap)
					if(map.count(keyVal.first) == 0)
						map[keyVal.first] = keyVal.second;

	return map;
}

ColumnEncoder::colVec ColumnEncoder::originalNames(const ColumnEncoder * self)
{
	colVec vec = self->ownOriginalNames();

	if(_otherEncoders)
		for(const ColumnEncoder * other : *_otherEncoders)
			if(other != self)
				for(const std::string & name : other->_originalNames)
					vec.push_back(name);

	sortVectorBigToSmall(vec);
	return vec;
}

bool ColumnEncoder::shouldEncode(const std::string & in)
{
	if(_mode == Mode::full)
		return _encodingMap.count(in) > 0;

	return mintForward().count(in) > 0;
}

std::string	ColumnEncoder::replaceAllStrict(const std::string & text, const std::map<std::string, std::string> & map)
{
	if (map.count(text) > 0)
		return map.at(text);
	else
		return text;
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
	return encodeRScript(text, ownEncodingMap(), ownOriginalNames(), columnNamesFound);
}


/*!
 * \brief Replace column names with encoded column names.
 * \param allowedPrefixes Specifies all prefix that column names may have and still be seen as columns names. e.g. "data.<column_name>"
 * \return Original string with column names replaced by encoded column names
 */
std::string ColumnEncoder::encodeRScript(std::string text, std::map<std::string, std::set<std::string>>& prefixedColumnsFound, const std::set<std::string>& allowedPrefixes)
{
	stringvec prefixes(allowedPrefixes.begin(), allowedPrefixes.end());
	
	std::sort(prefixes.begin(), prefixes.end(), [](const std::string & l, const std::string & r){ return l.size() < r.size();  });
	prefixes.insert(prefixes.begin(), ""); //empty first

	const colMap map	= ownEncodingMap();
	const colVec names	= ownOriginalNames();
	
	prefixedColumnsFound.clear();
	
	for(auto& prefix : prefixes) {
		stringset columnNamesFound;
		text = encodeRScript(text, map, names, &columnNamesFound, prefix);
		prefixedColumnsFound.insert({prefix, columnNamesFound});
	}
	return text;
}

/*!
 * \brief Replace column names with encoded column names.
 * \param mandatoryPrefix Specifies a prefix that column names SHOULD have in order to be replaced/encoded. We skip all names matches that lack this prefix. Default ""/empty_string means that all variables will be replaced.
 * \return Original string with column names replaced by encoded column names
 */
std::string ColumnEncoder::encodeRScript(std::string text, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, std::set<std::string> * columnNamesFound, const std::string& mandatoryPrefix)
{
	if(columnNamesFound)
		columnNamesFound->clear();

	static std::regex nonNameChar("[^\\.A-Za-z0-9_]");

	//Some lambas to test if Column names matches are free and not just a substr of some other expression.
	auto testNonNameChar = [&](size_t pos) -> bool
	{
		return pos == 0 || std::regex_match(text.substr(pos - 1, 1),	nonNameChar);
	};
	
	auto testFreePrefix = [&](size_t pos) -> bool
	{
		if(mandatoryPrefix == "") return false;
		try {
			bool match = text.substr(pos - mandatoryPrefix.length(), mandatoryPrefix.length()) == mandatoryPrefix;
			if(match) return testNonNameChar(pos - mandatoryPrefix.length()); //Must still be free before prefix
		}
		catch(std::out_of_range e){}
		return false;
	};
	
	auto testEndFree = [&](size_t pos) -> bool
	{
		bool endIsFree = pos == text.length() || std::regex_match(text.substr(pos, 1),	nonNameChar);

		//Check for "(" as well because maybe someone has a columnname such as rep or if or something weird like that. This might however have some whitespace in between...
		bool keepGoing = true;
		for(size_t bracePos = pos; bracePos < text.size() && endIsFree && keepGoing; bracePos++)
			if(text[bracePos] == '(')
				endIsFree = false;
			else if(text[bracePos] != '\t' && text[bracePos] != ' ' && text[bracePos] != '\n')
				keepGoing = false; //Aka something else than whitespace or a brace and that means that we can replace it!
		return endIsFree;
	};

	//for now we simply replace any found columnname by its encoded variant if found
	for(const std::string & oldCol : names)
	{
		std::string	newCol	= map.at(oldCol);

		std::vector<size_t> foundColPositions = getPositionsColumnNameMatches(text, oldCol);
		std::reverse(foundColPositions.begin(), foundColPositions.end());

		for (size_t foundPos : foundColPositions)
		{
			size_t foundPosEnd = foundPos + oldCol.length();

			bool hasFreePrefix = testFreePrefix(foundPos);
			if(mandatoryPrefix != "" && !hasFreePrefix) // simply skip if the var did not have mandatoryPrefix
				continue;

			//check if it is a "free columnname" aka is there some space or a kind in front of it. We would not want to replace a part of another term (Imagine what happens when you use a columname such as "E" and a filter that includes the term TRUE, it does not end well..)
			bool startIsFree	= hasFreePrefix || testNonNameChar(foundPos);
			bool endIsFree		= testEndFree(foundPosEnd);

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


std::string ColumnEncoder::encodeAll(const std::string & text) const
{
	return replaceAll(text, encodingMap(this), originalNames(this));
}

void ColumnEncoder::encodeJson(Json::Value & json, bool replaceNames, bool replaceStrict) const
{
	replaceAll(json, encodingMap(this), originalNames(this), replaceNames, replaceStrict);
}


void ColumnEncoder::replaceAll(Json::Value & json, const std::map<std::string, std::string> & map, const std::vector<std::string> & names, bool replaceNames, bool replaceStrict)
{
	switch(json.type())
	{
	case Json::arrayValue:
		for(Json::Value & option : json)
			replaceAll(option, map, names, replaceNames, replaceStrict);
		return;

	case Json::objectValue:
	{
		std::map<std::string, std::string> changedMembers;

		for(const std::string & optionName : json.getMemberNames())
		{
			replaceAll(json[optionName], map, names, replaceNames, replaceStrict);

			if(replaceNames)
			{
				std::string replacedName = replaceStrict ? replaceAllStrict(optionName, map) : replaceAll(optionName, map, names);

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
		json = replaceStrict ? replaceAllStrict(json.asString(), map) : replaceAll(json.asString(), map, names);
		return;

	default:
		return;
	}
}

void ColumnEncoder::setCurrentNamesFromOptionsMeta(const Json::Value & options)
{
	colTypeMap namesFound;

	if(!options.isNull() && options.isMember(".meta"))
		collectExtraEncodingsFromMetaJson(options[".meta"], namesFound);

	setCurrentNames(namesFound);
}

void ColumnEncoder::collectExtraEncodingsFromMetaJson(const Json::Value & json, colTypeMap & namesCollected) const
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
			if(json["encodeThis"].isString())
				namesCollected[json["encodeThis"].asString()] = columnType::unknown;
			else if(json["encodeThis"].isArray())
				for(const Json::Value & enc : json["encodeThis"])
					namesCollected[enc.asString()] = columnType::unknown;
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
	return columnEncoder() ? columnEncoder()->ownOriginalNames() : colVec();
}

ColumnEncoder::colVec ColumnEncoder::columnNamesEncoded()
{
	return columnEncoder() ? columnEncoder()->ownEncodedNames() : colVec();
}

void ColumnEncoder::_convertPreloadingDataOption(Json::Value & options, const std::string& optionName, colsPlusTypes& colTypes, ColumnEncoder * encoder)
{
	std::string		optionKey	= options[optionName].isMember("optionKey") ? options[optionName]["optionKey"].asString() : "";
	bool			keepOriginalOption = (!optionKey.empty() && options[optionName].size() > 3); // The option has other members: this must be kept
	Json::Value		typeList	= options[optionName]["types"],
					valueList	= options[optionName]["value"],
					newOption	= keepOriginalOption ? options[optionName] : Json::arrayValue;

	bool useSingleVal = false;

	if(valueList.isString())
	{
		std::string val = valueList.asString();
		valueList = Json::arrayValue;
		valueList.append(val);

		useSingleVal = true; //Otherwise we break things like "splitBy" it seems
	}
	if(typeList.isString())
	{
		std::string type = typeList.asString();
		typeList = Json::arrayValue;
		typeList.append(type);
	}

	if (keepOriginalOption)
		newOption[optionKey] = Json::arrayValue;

	// The valueList can be either;
	// . a list of strings, if it is a list of variables without interaction
	// . a list of array of strings if it is a list of variables with interaction
	// . a list of objects, if the value contains not only the variables but also some extra control values (rowComponent).
	//   In this case, is has an optionKey that gives where the variable names are.
	//	 Also here, there can be interaction, so the optionKey can give either a list of strings or a list of array of strings.

	for(int i=0; i<valueList.size(); i++)
	{
		Json::Value jsonType		= typeList.size() > i ? typeList[i] : Json::nullValue,
					jsonValueOrg	= valueList[i],
					jsonValue		= (optionKey.empty() || keepOriginalOption || !jsonValueOrg.isMember(optionKey)) ? jsonValueOrg : jsonValueOrg[optionKey];

		if (jsonValue.isString())
		{
			std::string type = (jsonType.isString() ? jsonType.asString() : (jsonType.isArray() && jsonType.size() > 0 && jsonType[0].isString()) ? jsonType[0].asString() : "");
			bool hasType = type != "unknown" && columnTypeValidName(type);

			std::string columnName = jsonValue.asString();
			if(!hasType && columnName != "" && encoder->currentNames().count(columnName))
			{
				type = columnTypeToString(encoder->currentNames().at(columnName));
				hasType = type != "unknown";
			}

			std::string columnNameWithType = columnName.empty() ? "" : (columnName + (hasType ? "." + type : ""));

			if (optionKey.empty())
				newOption.append(columnNameWithType);
			else if (keepOriginalOption)
				newOption[optionKey].append(columnNameWithType);
			else
			{
				// Reuse original jsonValue in order to get the other members of the object
				jsonValueOrg[optionKey] = columnNameWithType;
				newOption.append(jsonValueOrg);
			}

			if (!columnNameWithType.empty() && hasType)
				colTypes.insert(std::make_pair(columnNameWithType, columnTypeFromString(type)));
		}
		else if (jsonValue.isArray())
		{
			// Value with interaction: there are several column names.
			Json::Value newColumnNames(Json::arrayValue);
			int colNr = 0;
			for (const Json::Value& jsonColumnName : jsonValue)
			{
				colNr++;
				std::string type = jsonType.isString() ? jsonType.asString() : (jsonType.isArray() && jsonType.size() >= colNr && jsonType[colNr-1].isString() ? jsonType[colNr-1].asString() : "");
				bool hasType = type != "unknown" && columnTypeValidName(type);
				std::string columnName = jsonColumnName.asString();

				if(!hasType && columnName != "" && encoder->currentNames().count(columnName))
				{
					type = columnTypeToString(encoder->currentNames().at(columnName));
					hasType = type != "unknown";
				}

				std::string columnNameWithType = columnName.empty() ? "" : (columnName + (hasType ? "." + type : ""));
				newColumnNames.append(columnNameWithType);

				if (!columnNameWithType.empty() && hasType)
					colTypes.insert(std::make_pair(columnNameWithType, columnTypeFromString(type)));
			}
			if (optionKey.empty())
				newOption.append(newColumnNames);
			else if (keepOriginalOption)
				newOption[optionKey].append(newColumnNames);
			else
			{
				jsonValueOrg[optionKey] = newColumnNames;
				newOption.append(jsonValueOrg);
			}
		}
		else
			newOption.append(jsonValueOrg);
	}

	options[optionName + ".types"] = options[optionName]["types"];
	options[optionName] = !useSingleVal ? newOption : newOption[0];
}

void ColumnEncoder::_addTypeToColumnNamesInOptionsRecursively(Json::Value & options, const Json::Value & meta, int dataSetId, int filterId, bool preloadingData, perDataSetColsPlusTypes & colTypesPerDataSet, const EncoderFor & encoderFor)
{
	if (options.isObject())
	{
		for (const std::string& optionName : options.getMemberNames())
		{
			//The meta tree mirrors the options tree, so the (dataSetId, filterId) stamp recorded on an option's
			//meta node (or on an enclosing node) tells us which namespace this option's terms belong to. Options
			//without meta (or without a dataSetId anywhere up their path) keep belonging to the dataset they
			//inherited: the primary one; without a filterId they stay unqualified (-1) and the resolver picks
			//the dataset-level fallback (mds-plan D1.6/C7).
			const Json::Value & childMeta	= (meta.isObject() && meta.isMember(optionName)) ? meta[optionName] : Json::Value::null;
			const int			childDsId	= _dataSetIdFromMetaNode(childMeta, dataSetId);
			const int			childFlId	= _filterIdFromMetaNode(childMeta, filterId);

			if (options[optionName].isObject() && options[optionName].isMember("value") && options[optionName].isMember("types"))
			{
				if(preloadingData)
					_convertPreloadingDataOption(options, optionName, colTypesPerDataSet[childDsId], _encoderForDataSetId(childDsId, childFlId, encoderFor));
				else //make sure "optionname".types is available for analyses incapable of preloadingData, this should be considered deprecated
				{
					options[optionName + ".types"] = options[optionName]["types"];

					std::string		optionKey			= options[optionName].isMember("optionKey") ? options[optionName]["optionKey"].asString() : "";
					bool			keepOriginalOption	= (!optionKey.empty() && options[optionName].size() > 3); // The option has other members: this must be kept

					if (!keepOriginalOption)
						options[optionName] = options[optionName]["value"];
				}
			}
			else
				_addTypeToColumnNamesInOptionsRecursively(options[optionName], childMeta, childDsId, childFlId, preloadingData, colTypesPerDataSet, encoderFor);
		}
	}
	else if (options.isArray())
	{
		//Meta for an array is either an array (element-wise) or a single object shared by all elements,
		//exactly like _encodeColumnNamesinOptions() below treats it.
		const bool metaIsArray = meta.isArray();

		for (Json::ArrayIndex i = 0; i < options.size(); i++)
		{
			const Json::Value & elementMeta	= metaIsArray && i < meta.size() ? meta[i] : meta;

			_addTypeToColumnNamesInOptionsRecursively(options[i], elementMeta, _dataSetIdFromMetaNode(elementMeta, dataSetId), _filterIdFromMetaNode(elementMeta, filterId), preloadingData, colTypesPerDataSet, encoderFor);
		}
	}
	else if (options.isString())
	{
		const std::string possibleCol = options.asString();

		ColumnEncoder * encoder = _encoderForDataSetId(dataSetId, filterId, encoderFor);

		if(encoder->currentNames().count(possibleCol))
		{
			columnType possType = encoder->currentNames().at(possibleCol);
			if(possType != columnType::unknown)
				colTypesPerDataSet[dataSetId].insert(std::make_pair(possibleCol + "." + columnTypeToString(possType) , possType));
		}

	}
}

ColumnEncoder::colsPlusTypes ColumnEncoder::encodeColumnNamesinOptions(Json::Value & options, bool preloadingData)
{
	perDataSetColsPlusTypes getTheseColsPerDataSet = encodeColumnNamesinOptionsPerDataSet(options, preloadingData, nullptr, 0);

	colsPlusTypes getTheseCols;

	//Without a resolver everything is attributed to the single (primary) dataset, so normally there is just
	//one entry; merging is just to be thorough.
	for (const auto & colsPerDataSet : getTheseColsPerDataSet)
		getTheseCols.insert(colsPerDataSet.second.begin(), colsPerDataSet.second.end());

	return getTheseCols;
}

ColumnEncoder::perDataSetColsPlusTypes ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(Json::Value & options, bool preloadingData, const EncoderFor & encoderFor, int primaryDataSetId)
{
	columnEncoder();
	perDataSetColsPlusTypes getTheseColsPerDataSet;

	//LOGGER << "Options before encoding: " << options.toStyledString() << std::endl;

	_addTypeToColumnNamesInOptionsRecursively(options, options[".meta"], primaryDataSetId, -1, preloadingData, getTheseColsPerDataSet, encoderFor);

	_encodeColumnNamesinOptions(options, options[".meta"], primaryDataSetId, -1, encoderFor);

	//LOGGER << "Options after encoding: " << options.toStyledString() << std::endl;
	return getTheseColsPerDataSet;
}

int ColumnEncoder::_dataSetIdFromMetaNode(const Json::Value & metaNode, int inherited)
{
	if(metaNode.isObject() && metaNode.isMember("dataSetId") && metaNode["dataSetId"].isInt())
		return metaNode["dataSetId"].asInt();

	return inherited;
}

int ColumnEncoder::_filterIdFromMetaNode(const Json::Value & metaNode, int inherited)
{
	if(metaNode.isObject() && metaNode.isMember("filterId") && metaNode["filterId"].isInt())
		return metaNode["filterId"].asInt();

	return inherited;
}

ColumnEncoder * ColumnEncoder::_encoderForDataSetId(int dataSetId, int filterId, const EncoderFor & encoderFor)
{
	if(encoderFor)
		if(ColumnEncoder * encoder = encoderFor(dataSetId, filterId))
			return encoder;

	return columnEncoder();
}

void ColumnEncoder::collectDataSetIdsFromMeta(const Json::Value & meta, std::map<int, int> & dataSetFilterIds)
{
	if(meta.isObject())
	{
		if(meta.isMember("dataSetId") && meta["dataSetId"].isInt())
		{
			const int dataSetId	= meta["dataSetId"].asInt(),
						filterId	= (meta.isMember("filterId") && meta["filterId"].isInt()) ? meta["filterId"].asInt() : -1;

			auto existing = dataSetFilterIds.find(dataSetId);

			if(existing == dataSetFilterIds.end() || (existing->second == -1 && filterId != -1))
				dataSetFilterIds[dataSetId] = filterId;
		}

		for(const std::string & memberName : meta.getMemberNames())
			collectDataSetIdsFromMeta(meta[memberName], dataSetFilterIds);
	}
	else if(meta.isArray())
		for(const Json::Value & element : meta)
			collectDataSetIdsFromMeta(element, dataSetFilterIds);
}

void ColumnEncoder::collectFilterIdsFromMeta(const Json::Value & meta, std::set<int> & filterIds)
{
	if(meta.isObject())
	{
		if(meta.isMember("filterId") && meta["filterId"].isInt())
			filterIds.insert(meta["filterId"].asInt());	//filter ids are globally unique and imply their dataset (Filter::data())

		for(const std::string & memberName : meta.getMemberNames())
			collectFilterIdsFromMeta(meta[memberName], filterIds);
	}
	else if(meta.isArray())
		for(const Json::Value & element : meta)
			collectFilterIdsFromMeta(element, filterIds);
}

void ColumnEncoder::_encodeColumnNamesinOptions(Json::Value & options, Json::Value & meta, int dataSetId, int filterId, const EncoderFor & encoderFor)
{
	if(meta.isNull())
		return;

	//A node can either record its own stamp or inherit it from the node enclosing it (mds-plan D1.6:
	//the filterId is read alongside the dataSetId, so every option encodes against the exact filter
	//its terms were selected through; an unqualified node keeps the inherited -1).
	dataSetId	= _dataSetIdFromMetaNode(meta, dataSetId);
	filterId	= _filterIdFromMetaNode(meta, filterId);

	bool	encodePlease	= meta.isObject() && meta.get("shouldEncode",	false).asBool(),
			isRCode			= meta.isObject() && meta.get("rCode",			false).asBool();

	//Every resolved encoder here is a filter VIEW whose tokens embed the dataset id AND the filter
	//id (mds-plan Phase 1, C1), so a multi-dataset options blob can be encoded slice by slice:
	//every option against the namespace of the filter its terms come from - the very namespace the
	//slice queue hands that filter's data to R in. Without a resolver this is the old behaviour:
	//everything against the process-global current encoder.
	ColumnEncoder * encoder = _encoderForDataSetId(dataSetId, filterId, encoderFor);

	switch(options.type())
	{
	case Json::arrayValue:
		if(encodePlease)
			encoder->encodeJson(options, false, true); //If we already think we have columnNames just change it all

		else if(meta.type() == Json::arrayValue)
			for(int i=0; i<options.size() && i < meta.size(); i++)
				_encodeColumnNamesinOptions(options[i], meta[i], dataSetId, filterId, encoderFor);

		else if(isRCode)
		{
			for(int i=0; i<options.size(); i++)
				if(options[i].isString())
					options[i] = encoder->encodeRScript(options[i].asString());
		}
		else if(meta.type() == Json::objectValue) // The option is an array, and the meta is an object: each option element in the array must be encoded with the same meta
			for(int i=0; i<options.size(); i++)
				_encodeColumnNamesinOptions(options[i], meta, dataSetId, filterId, encoderFor);


		return;

	case Json::objectValue:
		for(const std::string & memberName : options.getMemberNames())
			if(memberName != ".meta" && meta.isMember(memberName))
				_encodeColumnNamesinOptions(options[memberName], meta[memberName], dataSetId, filterId, encoderFor);

			else if(isRCode && options[memberName].isString())
				options[memberName] = encoder->encodeRScript(options[memberName].asString());

			else if(encodePlease)
				encoder->encodeJson(options, false, true); //If we already think we have columnNames just change it all I guess?

		return;

	case Json::stringValue:

			if(isRCode)				options = encoder->encodeRScript(options.asString());
			else if(encodePlease)	options = encoder->encodeAll(options.asString());

		return;

	default:
		return;
	}
}
