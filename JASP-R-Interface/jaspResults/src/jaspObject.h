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

void jaspPrint(std::string msg);

#define JASPOBJECT_DEFAULT_POSITION 9999

DECLARE_ENUM(jaspObjectType, unknown, container, table, plot, json, list, results, html, state, column);
DECLARE_ENUM(jaspColumnType, unknown, scale, ordinal, nominal, nominalText); //can be merged with columnType from CentralDatasetModel branch later on?

jaspObjectType jaspObjectTypeStringToObjectType(std::string type);

std::string					stringExtend(std::string & str, size_t len, char kar = ' ');
std::string					stringRemove(std::string str,				char kar = ' ');
std::vector<std::string>	stringSplit(std::string str,				char kar = ';');

//Simple base-class for all JASP-objects, containing things like a title or a warning and stuff like that
class jaspObject
{
public:
						jaspObject()										: _title(""),		_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(std::string title)						: _title(title),	_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(jaspObjectType type, std::string title)	: _title(title),	_type(type)						{ allocatedObjects->insert(this); }
						jaspObject(const jaspObject& that) = delete;
	virtual				~jaspObject();

			std::string objectTitleString(std::string prefix)	const { return prefix + jaspObjectTypeToString(_type) + " " + _title; }
	virtual	std::string dataToString(std::string)				const { return ""; }
			std::string toString(std::string prefix = "")		const;

	virtual std::string toHtml() { return ""; }
			std::string htmlTitle() { return "<h2>" + _title + "</h2>"; }

			std::string type() { return jaspObjectTypeToString(_type); }

			bool		getError()								{ return _error; }
	virtual void		setError()								{ _error = true; }
	virtual void		setError(std::string message)			{ _errorMessage = message; _error = true; }
	virtual bool		canShowErrorMessage()			const	{ return false; }

			void		print()									{ try { jaspPrint(toString()); } catch(std::exception e) { jaspPrint(std::string("toString failed because of: ") + e.what()); } }
			void		addMessage(std::string msg)				{ _messages.push_back(msg); }
	virtual void		childrenUpdatedCallbackHandler(bool)	{} ///Can be called by jaspResults to send changes and stuff like that.

			void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis);
			void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis);
			void		dependOnOptions(Rcpp::CharacterVector listOptions);
			void		copyDependenciesFromJaspObject(jaspObject * other);

			bool		checkDependencies(Json::Value currentOptions); //returns false if no longer valid and destroys children (if applicable) that are no longer valid
	virtual	void		checkDependenciesChildren(Json::Value currentOptions) {}

			void		addCitation(std::string fullCitation);

			std::string	_title,
						_info;
			int			_position = JASPOBJECT_DEFAULT_POSITION;

			jaspObjectType	getType()						{ return _type; }
			bool			shouldBePartOfResultsJson()		{ return _type != jaspObjectType::state && _type != jaspObjectType::json; }

			Json::Value		constructMetaEntry(std::string type, std::string meta = "") const;

	//These functions convert the object to a json that can be understood by the resultsviewer
	virtual	Json::Value		metaEntry()															const { return Json::Value(Json::nullValue); }
	virtual	Json::Value		dataEntry(std::string & errorMessage)								const ;

	//These two are meant for jaspContainer and take old results into account and a possible errorMessage
	virtual	Json::Value		metaEntry(jaspObject * oldResult)									const { return metaEntry(); }
	virtual	Json::Value		dataEntry(jaspObject * oldResult, std::string & errorMessage)		const { return dataEntry(errorMessage); }

			Json::Value		dataEntryBase()														const;

	//These functions convert to object and all to a storable json-representation that can be written to disk and loaded again.
	virtual Json::Value		convertToJSON() const;
	static	jaspObject *	convertFromJSON(Json::Value in);
	virtual	void			convertFromJSON_SetFields(Json::Value in);

			///Gives nested name to avoid namingclashes
			std::string getUniqueNestedName() const;
			void		setName(std::string name) { _name = name; }

			void		childrenUpdatedCallback(bool ignoreSendTimer);
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

	Rcpp::DataFrame convertFactorsToCharacters(Rcpp::DataFrame df);

	static Json::Value currentOptions;

	void			notifyParentOfChanges(); ///let ancestors know about updates

	static int getCurrentTimeMs();
	static void setDeveloperMode(bool developerMode);

protected:
	jaspObjectType				_type;
	std::string					_errorMessage = "";
	bool						_error = false;

	std::vector<std::string>	_messages;
	std::set<std::string>		_citations;
	std::string					_name;

	std::set<std::string>							nestedMustBes()			const;
	std::map<std::string, std::set<std::string>>	nestedMustContains()	const;
	std::map<std::string, Json::Value>				_optionMustContain;
	std::map<std::string, Json::Value>				_optionMustBe;


//Should add dependencies somehow here?

//Some basic administration of objecttree:
			bool			hasAncestor(jaspObject * ancestor) { return parent == ancestor || parent == NULL ? false : parent->hasAncestor(ancestor); }
			void			addChild(jaspObject * child);

			void			removeChild(jaspObject * child);


	jaspObject				*parent = NULL;
	std::set<jaspObject*>	children;

	static std::set<jaspObject*> *	allocatedObjects;
	static bool						_developerMode;

private:
	bool					_finalizedAlready = false;
};

#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(JASP_TYPE, PROP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
	void set ## PROP_CAPITALIZED_NAME (PROP_TYPE new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME = new ## PROP_CAPITALIZED_NAME; myJaspObject->notifyParentOfChanges(); } \
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
	std::string	toHtml()							{ return myJaspObject->toHtml(); }
	std::string	type()								{ return myJaspObject->type(); }
	void		printHtml()							{ jaspPrint(myJaspObject->toHtml()); }

	void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis)				{ myJaspObject->setOptionMustBeDependency(optionName, mustBeThis);				}
	void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis)	{ myJaspObject->setOptionMustContainDependency(optionName, mustContainThis);	}
	void		dependOnOptions(Rcpp::CharacterVector listOptions)										{ myJaspObject->dependOnOptions(listOptions);									}
	void		copyDependenciesFromJaspObject(jaspObject_Interface * other)							{ myJaspObject->copyDependenciesFromJaspObject(other->myJaspObject);			}
	void		addCitation(std::string fullCitation)													{ myJaspObject->addCitation(fullCitation);										}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspObject, std::string,	_title,		Title)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspObject, std::string,	_info,		Info)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspObject, int,			_position,	Position)

	void		setError(std::string message)		{ myJaspObject->setError(message); }
	bool		getError()							{ return myJaspObject->getError(); }

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
#define JASP_OBJECT_CREATOR_ARG(JASP_TYPE, EXTRA_ARG) JASP_TYPE ## _Interface * JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE)(std::string title, Rcpp::RObject EXTRA_ARG) { return new JASP_TYPE ## _Interface (new JASP_TYPE(title, EXTRA_ARG)); }


RCPP_EXPOSED_CLASS_NODECL(jaspObject_Interface)

//#define JASP_R_INTERFACE_TIMERS

#ifdef JASP_R_INTERFACE_TIMERS
#define JASP_OBJECT_TIMERBEGIN			static int cumulativeTime = 0;	int startSerialize = getCurrentTimeMs();
#define JASP_OBJECT_TIMEREND(ACTIVITY)	cumulativeTime += getCurrentTimeMs() - startSerialize;	std::cout << jaspObjectTypeToString(getType()) << " spent " << cumulativeTime << "ms " #ACTIVITY "!" << std::endl;
#else
#define JASP_OBJECT_TIMERBEGIN			/* Doin' nothing */
#define JASP_OBJECT_TIMEREND(ACTIVITY)	/* What you didn't start you need not stop */
#endif
