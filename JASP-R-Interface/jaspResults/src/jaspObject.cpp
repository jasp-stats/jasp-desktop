#define ENUM_DECLARATION_CPP
#include "jaspObject.h"
#include "jaspJson.h"
#include <chrono>

#if defined(_WIN32) || !defined(JASP_R_INTERFACE_LIBRARY)
#include "lib_json/json_value.cpp" //hacky way to get libjson in the code ^^
#include "lib_json/json_reader.cpp"
#include "lib_json/json_writer.cpp"
#endif

jaspObjectType jaspObjectTypeStringToObjectType(std::string type)
{
	try			{ return jaspObjectTypeFromString(type); }
	catch(...)	{ return jaspObjectType::unknown; }
}

std::string stringExtend(std::string & str, size_t len, char kar)
{
	if(str.size() < len)
		str += std::string(len - str.size(), kar);

	return str;
}

std::string stringRemove(std::string str, char kar)
{
	for(size_t removeMe = str.find_first_of(kar); removeMe != std::string::npos; removeMe = str.find_first_of(kar))
		str.erase(removeMe, 1);
	return str;
}

std::vector<std::string> stringSplit(std::string str, char kar)
{
	std::vector<std::string> strs;

	strs.push_back("");
	for(char k : str)
		if(k == kar)
			strs.push_back("");
		else
			strs[strs.size() - 1].push_back(k);

	return strs;
}

void jaspPrint(std::string msg)
{
#ifdef JASP_R_INTERFACE_LIBRARY
	std::cout << msg << std::endl << std::flush;
#else
	Rcpp::Rcout << msg << "\n";
	//Rprintf(msg.c_str());
#endif
}


std::set<jaspObject*> * jaspObject::allocatedObjects = new std::set<jaspObject*>();

jaspObject::~jaspObject()
{
	allocatedObjects->erase(this);

	if(parent != NULL)
		parent->removeChild(this);

	while (children.size() > 0)
	{
		jaspObject * p = *(children.begin());

		removeChild(p);

		delete p;
	}
}

void jaspObject::destroyAllAllocatedObjects()
{
	//std::cout << "destroyAllAllocatedObjects!\n"<<std::flush;
	while(allocatedObjects->size() > 0)
	{
		jaspObject * p = *(allocatedObjects->begin());

		//std::cout << "p == "<<p->objectTitleString()<<"!\n"<<std::flush;

		allocatedObjects->erase(allocatedObjects->begin());
		delete p;
	}
}

void jaspObject::addChild(jaspObject * child)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << title << " adds Child " << child->title << "\n" << std::flush;
#endif

	if(child->parent == this)
		return;

	if(child == this || hasAncestor(child))
		throw std::logic_error("You cannot make someone their own descendant, this isn't back to the future..");

	if(child->parent != NULL)
		child->parent->children.erase(child);

	child->parent = this;

	children.insert(child);
}

void jaspObject::removeChild(jaspObject * child)
{
	if(child->parent != this || child == NULL)
		return;

	children.erase(child);

	child->parent = NULL;
}

void jaspObject::finalized()
{
	//std::cout << "jaspObject::finalized() called on "<<objectTitleString()<<" " << (_finalizedAlready ? "again!" :"") << "\n" << std::flush;
	//std::cout << "this: "<<this<<"\n"<<std::flush;

	if(_finalizedAlready)
		return;

	_finalizedAlready= true;

	if(parent != NULL)
		parent->childFinalized(this);

	finalizedHandler();

	for(auto child : children)
		child->finalized();
}

void jaspObject::childFinalized(jaspObject * child)
{
	finalized();

	childFinalizedHandler(child);
	removeChild(child);
}

void jaspObject::notifyParentOfChanges()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "notifyParentOfChanges()! parent is " << ( parent == NULL ? "NULL" : parent->title) << "\n" << std::flush;
#endif

	if(parent != NULL)
		parent->childrenUpdatedCallback();
}

void jaspObject::childrenUpdatedCallback()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "childrenUpdatedCallback()! parent is " << ( parent == NULL ? "NULL" : parent->title) << "\n" << std::flush;
#endif

	childrenUpdatedCallbackHandler();

	if(parent != NULL)
		parent->childrenUpdatedCallback();
}

std::string jaspObject::toString(std::string prefix)
{
	std::string dataString = dataToString(prefix + "\t");
	return objectTitleString(prefix) + (dataString == "" ? "\n" : ":\n" + dataString);
}

Rcpp::DataFrame jaspObject::convertFactorsToCharacters(Rcpp::DataFrame df)
{

	for(int col=0; col<df.length(); col++)
		if(Rf_isFactor(df[col]))
		{
			Rcpp::IntegerVector		originalColumn	= df[col];

			Rcpp::CharacterVector	factorLevels	= originalColumn.attr("levels");

			std::cout	<< "converting factors to characters for dataframe\n"
						<< "originalColumn: " << originalColumn << "\n"
						<< "factorLevels: " << factorLevels << std::endl;

			Rcpp::CharacterVector	charCol(originalColumn.size());

			for(int i=0; i<originalColumn.size(); i++)
				charCol[i] = Rcpp::as<std::string>(factorLevels[originalColumn[i] - 1]);

			df[col] = charCol;
		}

	return df;
}

Json::Value	jaspObject::constructMetaEntry(std::string type, std::string meta)
{
	Json::Value obj(Json::objectValue);

	obj["name"] = getUniqueNestedName();
	obj["type"] = type;

	if(meta != "")
		obj["meta"] = meta;

	return obj;
}

std::string jaspObject::getUniqueNestedName()
{
	std::string parent_prefix = parent == NULL || parent->getUniqueNestedName() == "" ? "" :  parent->getUniqueNestedName() + "_";

	return parent_prefix + (_name != "" ? _name : "");
}


void jaspObjectFinalizer(jaspObject * obj)
{
	if(obj == NULL)
		return;

#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "JASPobjectFinalizer is run on: " << obj->title << "\n" << std::flush;
#endif

	obj->finalized();
}

Json::Value jaspObject::convertToJSON()
{
	Json::Value obj(Json::objectValue);

	obj["name"]			= _name;
	obj["title"]		= _title;
	obj["type"]			= jaspObjectTypeToString(_type);
	obj["error"]        = _error;
	obj["warning"]		= _warning;
	obj["position"]		= _position;
	obj["warningSet"]	= _warningSet;
	obj["citations"]	= _citations;
	obj["messages"]		= Json::arrayValue;

	for(auto m : _messages)
		obj["messages"].append(m);

	obj["optionMustBe"]	= Json::objectValue;
	for(auto & keyval : _optionMustBe)
		obj["optionMustBe"][keyval.first] = keyval.second;

	obj["optionMustContain"]	= Json::objectValue;
	for(auto & keyval : _optionMustContain)
		obj["optionMustContain"][keyval.first] = keyval.second;

	return obj;
}


void jaspObject::convertFromJSON_SetFields(Json::Value in)
{
	_name		= in.get("name",		"null").asString();
	_title		= in.get("title",		"null").asString();
	_warning	= in.get("warning",		"null").asString();
	_warningSet	= in.get("warningSet",	false).asBool();
	_position	= in.get("position",	JASPOBJECT_DEFAULT_POSITION).asInt();
	_citations	= in.get("citations",	Json::arrayValue);

	_messages.clear();

	for(auto & msg : in.get("messages", Json::nullValue))
		_messages.push_back(msg.asString());

	_optionMustBe.clear();
	Json::Value mustBe(in.get("optionMustBe", Json::objectValue));
	for(auto & mustBeKey : mustBe.getMemberNames())
		_optionMustBe[mustBeKey] = mustBe[mustBeKey];

	_optionMustContain.clear();
	Json::Value mustContain(in.get("optionMustContain", Json::objectValue));
	for(auto & mustContainKey : mustContain.getMemberNames())
		_optionMustContain[mustContainKey] = mustContain[mustContainKey];

}

Json::Value jaspObject::currentOptions = Json::nullValue;

void jaspObject::dependOnOptions(Rcpp::CharacterVector listOptions)
{
	if(currentOptions.isNull()) Rf_error("No options known!");

	for(auto & nameOption : listOptions)
		_optionMustBe[Rcpp::as<std::string>(nameOption)] = currentOptions.get(nameOption, Json::nullValue);
}

void jaspObject::setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis)
{
	_optionMustBe[optionName]	= jaspJson::RObject_to_JsonValue(mustBeThis);
}

void jaspObject::setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis)
{
	_optionMustContain[optionName] = jaspJson::RObject_to_JsonValue(mustContainThis);
}

void jaspObject::copyDependenciesFromJaspObject(jaspObject * other)
{
	for(auto fieldVal : other->_optionMustBe)
		_optionMustBe[fieldVal.first] = fieldVal.second;

	for(auto fieldVal : other->_optionMustContain)
		_optionMustContain[fieldVal.first] = fieldVal.second;
}

bool jaspObject::checkDependencies(Json::Value currentOptions)
{
	if((_optionMustBe.size() + _optionMustContain.size()) == 0)
		return true;

	for(auto & keyval : _optionMustBe)
		if(currentOptions.get(keyval.first, Json::nullValue) != keyval.second)
			return false;

	for(auto & keyval : _optionMustContain)
	{
		bool foundIt = false;

		for(auto & contains : currentOptions.get(keyval.first, Json::arrayValue))
			if(contains == keyval.second)
				foundIt = true;

		if(!foundIt)
			return false;
	}

	checkDependenciesChildren(currentOptions);

	return true;
}

void jaspObject::addCitation(std::string fullCitation)
{
	_citations.append(fullCitation);

	notifyParentOfChanges();
}

Json::Value	jaspObject::dataEntry()
{
	Json::Value baseObject(Json::objectValue);
	if(_citations.size() > 0)
		baseObject["citation"] = _citations;


	return baseObject;

}

int jaspObject::getCurrentTimeMs()
{
	return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
}

