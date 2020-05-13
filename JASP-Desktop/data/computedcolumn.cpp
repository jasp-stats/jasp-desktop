#include <regex>
#include "computedcolumn.h"
#include "utils.h"
#include "analysis/analysis.h"

bool ComputedColumn::setRCode(std::string rCode)
{
	if(_rCode != rCode)
	{
		_rCode			= rCode;
        _rCodeStripped	= stringUtils::stripRComments(rCode);

		findDependencies();
		invalidate();
		return true;
	}
	return false;
}

bool ComputedColumn::setConstructorJson(std::string jsonStr)
{
	Json::Value parsed;
	Json::Reader().parse(jsonStr, parsed);

	if(_constructorCode.toStyledString() != parsed.toStyledString())
	{
		_constructorCode = parsed;
		return true;
	}
	return false;
}

bool ComputedColumn::setError(std::string error)
{
	if(_error != error)
	{
		_error = error;
		return true;
	}
	return false;
}

void ComputedColumn::setAnalysis(Analysis *analysis)
{
	_analysis	= analysis;
	_analysisId = _analysis == nullptr ? -1 : _analysis->id();
}

std::vector<std::string> ComputedColumn::_allColumnNames;

void ComputedColumn::setAllColumnNames(std::vector<std::string> names)
{
	std::sort(names.begin(), names.end(), [](std::string & a, std::string & b) { return a.size() > b.size(); }); //Sort on stringlength so that we can check the columns in the code from bigger to small (findUsedColumnNames) so that we can have both something like "Height Ratio"  and "Height"
	_allColumnNames = names;
}

std::set<std::string> ComputedColumn::findUsedColumnNames(std::string searchThis)
{
	if(_codeType == computedType::analysis && _analysis != NULL)
		return _analysis->usedVariables();

	return findUsedColumnNamesStatic(searchThis);

}

std::set<std::string> ComputedColumn::findUsedColumnNamesStatic(std::string searchThis)
{
	//sort of based on rbridge_encodeColumnNamesToBase64
	static std::regex nonNameChar("[^\\.A-Za-z0-9]");
	std::set<std::string> columnsFound;

	size_t foundPos = -1;
	for(const std::string & col : _allColumnNames)
	{
		while((foundPos = searchThis.find(col, foundPos + 1)) != std::string::npos)
		{
			size_t foundPosEnd = foundPos + col.length();
			//First check if it is a "free columnname" aka is there some space or a kind in front of it. We would not want to replace a part of another term (Imagine what happens when you use a columname such as "E" and a filter that includes the term TRUE, it does not end well..)
			bool startIsFree	= foundPos == 0							|| std::regex_match(searchThis.substr(foundPos - 1, 1),	nonNameChar);
			bool endIsFree		= foundPosEnd == searchThis.length()	|| (std::regex_match(searchThis.substr(foundPosEnd, 1),	nonNameChar) && searchThis[foundPosEnd] != '('); //Check for "(" as well because maybe someone has a columnname such as rep or if or something weird like that

			if(startIsFree && endIsFree)
			{
				columnsFound.insert(col);
				searchThis.replace(foundPos, col.length(), ""); // remove the found entry
			}

		}
	}

	return columnsFound;
}

bool ComputedColumn::iShouldBeSentAgain()
{
	if(!_invalidated) return false;

	for(ComputedColumn * col : *_computedColumns)
		if(dependsOn(col->name()) && col->isInvalidated())
			return false;
	return true;
}

void ComputedColumn::invalidate()
{
	_invalidated = true;
}

void ComputedColumn::invalidateDependents()
{
	for(ComputedColumn * col : *_computedColumns)
		if(col->dependsOn(_name))
			col->invalidate();
}

void ComputedColumn::checkForLoopInDepenedencies(std::string code)
{
	std::set<std::string> dependencies	= findUsedColumnNames(code);
	if(dependencies.count(_name) > 0)
		throw std::logic_error("A computed column can not use itself!");

	std::set<std::string> foundNames	= { _name };

	for(auto col : *_computedColumns)
		if(dependencies.count(col->name()) > 0)
			col->_checkForLoopInDepenedencies(foundNames, std::list<std::string>({_name}) );
}

void ComputedColumn::_checkForLoopInDepenedencies(std::set<std::string> foundNames, std::list<std::string> loopList)
{
	if(foundNames.count(_name) > 0)
	{
		std::stringstream loopSS;

		loopSS << "In the definitions of your computed columns the following loop was found:\n";
		for(std::string l : loopList)
			loopSS << l << " depends on ";
		loopSS << _name << "..\nThis is not allowed, so change one of the formulas to break the circle.";

		throw std::logic_error(loopSS.str());
	}

	foundNames.insert(_name);
	std::list<std::string> superLoopList(loopList);
	superLoopList.push_back(_name);

	findDependencies();

	for(auto col : *_computedColumns)
		if(_dependsOnColumns.count(col->name()) > 0)
			col->_checkForLoopInDepenedencies(foundNames, superLoopList);
}

std::string	ComputedColumn::computedTypeToString(computedType type)
{
	switch(type)
	{
	case computedType::constructorCode:	return  "constructorCode";
	case computedType::analysis:		return  "analysis";
	case computedType::rCode:			return  "rCode";
	default:							throw std::runtime_error("Add new computedType types to computedTypeToString!");
	}
}

ComputedColumn::computedType ComputedColumn::computedTypeFromString(std::string type)
{
	if		(type == "constructorCode")	return computedType::constructorCode;
	else if	(type == "analysis")		return computedType::analysis;
	else if	(type == "rCode")			return computedType::rCode;
	else								throw std::runtime_error("Add new computedType types to computedTypeFromString!");
}

Json::Value	ComputedColumn::convertToJson()
{
	Json::Value json(Json::objectValue);

	json["constructorCode"]	= _constructorCode;
	json["invalidated"]		= _invalidated;
	json["analysisId"]		= _analysisId;
	json["codeType"]		= computedTypeToString(_codeType);
	json["rCode"]			= _rCode;
	json["error"]			= _error;
	json["name"]			= _name;

	return json;
}

ComputedColumn::ComputedColumn(std::vector<ComputedColumn*> * allComputedColumns, Json::Value json) //Conversion from JSON!
: _computedColumns(allComputedColumns)
{
	_constructorCode	= json["constructorCode"];
	_invalidated		= json["invalidated"].asBool();
	_analysisId			= json["analysisId"].asInt();
	_codeType			= computedTypeFromString(json["codeType"].asString());
	_rCode				= json["rCode"].asString();
	_error				= json["error"].asString();
	_name				= json["name"].asString();

    _rCodeStripped		= stringUtils::stripRComments(_rCode);
}

void ComputedColumn::findDependencies()
{
	if(_codeType == computedType::analysis && _analysis != NULL)	_dependsOnColumns = _analysis->usedVariables();
	else															_dependsOnColumns = findUsedColumnNames();
}

const std::set<std::string> &  ComputedColumn::dependsOnColumns(bool refresh)
{
	if(refresh)
		findDependencies();
	return _dependsOnColumns;
}

bool ComputedColumn::dependsOn(std::string columnName, bool refresh)
{
	return dependsOnColumns(refresh).count(columnName) > 0;
}
