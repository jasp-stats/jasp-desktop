#pragma once
#include "jaspObject.h"
#include "jaspColumn.h"
#include "jaspPlot.h"
#include "jaspTable.h"
#include "jaspState.h"
#include "jaspJson.h"
#include "jaspHtml.h"
#include <map>



class jaspContainer : public jaspObject
{
public:
	jaspContainer(std::string title = "", jaspObjectType type = jaspObjectType::container) : jaspObject(type, title)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "JASPcontainer constructor for title: " << title << std::endl;
#endif
	}

	jaspContainer(const jaspContainer& that) = delete;

	std::string dataToString(std::string prefix = "")						const	override;
	std::string toHtml()															override;

	void			insert(std::string field, Rcpp::RObject value);
	Rcpp::RObject	at(std::string field);

	Json::Value	metaEntry(jaspObject * oldResult)							const	override;
	Json::Value	dataEntry(jaspObject * oldResult, std::string & errorMsg)	const	override;

	std::string getCommonDenominatorMetaType() const;

	int	length() { return _data.size(); }

	void childFinalizedHandler(jaspObject *child)									override;

	static jaspContainer * jaspContainerFromRcppList(Rcpp::List convertThis);

	Json::Value convertToJSON()												const	override;
	void		convertFromJSON_SetFields(Json::Value in)							override;
	void		checkDependenciesChildren(Json::Value currentOptions)				override;

	void		completeChildren();
	void		letChildrenRun();
	void		setError()															override;
	void		setError(std::string message)										override;

	bool		containsNonContainer();
	bool		canShowErrorMessage()										const	override;

	bool		_initiallyCollapsed = false;

	static std::vector<std::string>				convertSortedDataFieldsToStringVector(std::vector<std::pair<double, std::string>> sortvec, bool removeDuplicates = false);
	std::vector<std::pair<double, std::string>> getSortedDataFieldsSortVector()														const;
	std::vector<std::string>					getSortedDataFields()																const;
	std::vector<std::string>					getSortedDataFieldsWithOld(jaspContainer * oldResult)								const;
	jaspObject *								getJaspObjectNewOrOld(std::string fieldName, jaspContainer * oldResult)				const;
	jaspObject *								getJaspObjectFromData(std::string fieldName)										const;
	bool										jaspObjectComesFromOldResults(std::string fieldName, jaspContainer * oldResult)		const;

protected:
	std::map<std::string, jaspObject*>	_data;
	std::map<std::string, int>			_data_order;
	int									_order_increment = 0;

};

class jaspContainer_Interface : public jaspObject_Interface
{
public:
	jaspContainer_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	int length()													{ return ((jaspContainer*)myJaspObject)->length(); }
	Rcpp::RObject	at(std::string field)							{ return ((jaspContainer*)myJaspObject)->at(field); }
	void			insert(std::string field, Rcpp::RObject value)	{ ((jaspContainer*)myJaspObject)->insert(field, value); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspContainer, bool,	_initiallyCollapsed,	InitiallyCollapsed)
};

RCPP_EXPOSED_CLASS_NODECL(jaspContainer_Interface)
