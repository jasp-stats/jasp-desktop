#pragma once
#include <Rcpp.h>
#include <set>
#include <sstream>
#include <queue>
#include "enumutilities.h"
#ifdef JASP_R_INTERFACE_LIBRARY
#include "jsonredirect.h"
#else
#include "lib_json/json.h"
#endif

void JASPprint(std::string msg);

DECLARE_ENUM(jaspObjectType, unknown, container, table, plot, json, list, results, html, state);
jaspObjectType jaspObjectTypeStringToObjectType(std::string type);

//Simple base-class for all JASP-objects, containing things like a title or a warning and stuff like that
class jaspObject
{
public:
						jaspObject()										: _title(""),		_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(std::string title)						: _title(title),	_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(jaspObjectType type, std::string title)	: _title(title),	_type(type)						{ allocatedObjects->insert(this); }
						jaspObject(const jaspObject& that) = delete;
	virtual				~jaspObject();

			std::string objectTitleString()					{ return jaspObjectTypeToString(_type) + "(\"" + _title + "\")"; }
	virtual	std::string dataToString(std::string prefix)	{ return ""; }
			std::string toString(std::string prefix = "");

			std::string	getWarning()						{ return _warning; }
			void		setWarning(std::string warning)		{ _warning = warning; _warningSet = true; }

			void		print()								{ try { JASPprint(toString()); } catch(std::exception e) { JASPprint(std::string("toString failed because of: ") + e.what()); } }
			void		addMessage(std::string msg)			{ _messages.push_back(msg); }
	virtual void		childrenUpdatedCallbackHandler()	{} ///Can be caugt by jaspResults to send changes and stuff like that.

			void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis);
			void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis);
			void		dependOnOptions(Rcpp::CharacterVector listOptions);
			void		copyDependenciesFromJaspObject(jaspObject * other);

			bool		checkDependencies(Json::Value currentOptions); //returns false if no longer valid and destroys children (if applicable) that are no longer valid
	virtual	void		checkDependenciesChildren(Json::Value currentOptions) {}

			std::string	_title;

			jaspObjectType	getType()						{ return _type; }
			bool			shouldBePartOfResultsJson()		{ return _type != jaspObjectType::state && _type != jaspObjectType::json; }

			Json::Value	constructMetaEntry(std::string type, std::string meta = "");

	virtual	Json::Value	metaEntry() { return Json::Value(Json::nullValue); }
	virtual	Json::Value	dataEntry() { return Json::Value(Json::nullValue); }

			///Gives nested name to avoid namingclashes
			std::string getUniqueNestedName();
			void		setName(std::string name) { _name = name; }

			void		childrenUpdatedCallback();
	virtual void		childFinalizedHandler(jaspObject * child) {}
			void		childFinalized(jaspObject * child);
			void		finalized();
	virtual void		finalizedHandler() {}



	template <typename RCPP_CLASS> static std::vector<std::string> extractElementOrColumnNames(RCPP_CLASS rObj)
	{
		Rcpp::RObject colNamesRObject = Rcpp::colnames(rObj), kolnamesRObject = rObj.names();
		Rcpp::CharacterVector colNamesList;
		std::vector<std::string> colNamesVec;

		if(!colNamesRObject.isNULL() || !kolnamesRObject.isNULL())
		{
			colNamesList = !colNamesRObject.isNULL()  ? colNamesRObject : kolnamesRObject;

			for(size_t col=0; col<colNamesList.size(); col++)
				colNamesVec.push_back(Rcpp::as<std::string>(colNamesList[col]));
		}

		return colNamesVec;
	}

	static void destroyAllAllocatedObjects();

	std::set<jaspObject*> & getChildren() { return children; }

	virtual Json::Value convertToJSON();

	static	jaspObject *	convertFromJSON(Json::Value in);
	virtual	void			convertFromJSON_SetFields(Json::Value in);

	Rcpp::DataFrame convertFactorsToCharacters(Rcpp::DataFrame df);


	static Json::Value currentOptions;

protected:
	jaspObjectType				_type;
	std::string					_warning = "";
	bool						_warningSet = false;

	std::vector<std::string>	_messages;
	std::string					_name;

	std::map<std::string, Json::Value> _optionMustBe;
	std::map<std::string, Json::Value> _optionMustContain;

//Should add dependencies somehow here?

//Some basic administration of objecttree:
			bool			hasAncestor(jaspObject * ancestor) { return parent == ancestor || parent == NULL ? false : parent->hasAncestor(ancestor); }
			void			addChild(jaspObject * child);
			void			notifyParentOfChanges(); ///let ancestors know about updates
			void			removeChild(jaspObject * child);



	jaspObject				*parent = NULL;
	std::set<jaspObject*>	children;

	static std::set<jaspObject*> * allocatedObjects;

private:
	bool					_finalizedAlready = false;
};



#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(JASP_TYPE, PROP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
	void set ## PROP_CAPITALIZED_NAME (PROP_TYPE new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME = new ## PROP_CAPITALIZED_NAME; } \
	PROP_TYPE get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }



class jaspObject_Interface
{
public:
	jaspObject_Interface(jaspObject * dataObj) : myJaspObject(dataObj)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "Interface to " << dataObj->objectTitleString() << " is created!\n"<<std::flush;
#endif
	}

	jaspObject_Interface(const jaspObject_Interface* copyMe)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "Interface to " << copyMe->myJaspObject->objectTitleString() << " is copied!\n"<<std::flush;
#endif
		myJaspObject = copyMe->myJaspObject;
	}

	void		print()								{ myJaspObject->print(); }
	void		addMessage(std::string msg)			{ myJaspObject->addMessage(msg); }

	void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis)				{ myJaspObject->setOptionMustBeDependency(optionName, mustBeThis); }
	void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis)	{ myJaspObject->setOptionMustContainDependency(optionName, mustContainThis); }
	void		dependOnOptions(Rcpp::CharacterVector listOptions)									{ myJaspObject->dependOnOptions(listOptions); }
	void		copyDependenciesFromJaspObject(jaspObject_Interface * other)							{ myJaspObject->copyDependenciesFromJaspObject(other->myJaspObject); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspObject, std::string, _title, Title)

	void		setWarning(std::string newWarning)	{ myJaspObject->setWarning(newWarning); }
	std::string getWarning()						{ return myJaspObject->getWarning(); }

	jaspObject * returnMyJaspObject() { return myJaspObject; }

protected:
		jaspObject * myJaspObject = NULL;
};


void jaspObjectFinalizer(jaspObject * obj);
#define JASP_OBJECT_FINALIZER_LAMBDA(JASP_TYPE) //.finalizer( [](JASP_TYPE * obj) { std::cout << "finalizerLambda " #JASP_TYPE " Called\n" << std::flush;  jaspObjectFinalizer(obj); })

#define JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE) create_ ## JASP_TYPE
#define JASP_OBJECT_CREATOR_FUNCTIONNAME_STR(JASP_TYPE) "create_cpp_" #JASP_TYPE
#define JASP_OBJECT_CREATOR(JASP_TYPE) JASP_TYPE ## _Interface * JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE)(std::string title) { return new JASP_TYPE ## _Interface (new JASP_TYPE(title)); }
#define JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(JASP_TYPE) Rcpp::function(JASP_OBJECT_CREATOR_FUNCTIONNAME_STR(JASP_TYPE), &JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE))


RCPP_EXPOSED_CLASS_NODECL(jaspObject_Interface)

