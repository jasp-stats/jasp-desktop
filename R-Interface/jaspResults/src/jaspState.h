#pragma once
#include "jaspObject.h"

class jaspState : public jaspObject
{
public:
	jaspState(std::string title = "") : jaspObject(jaspObjectType::state, title) { initEnvName(); }

	void			setObject(Rcpp::RObject obj);
	Rcpp::RObject	getObject();

	Json::Value		convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;
	std::string		dataToString(std::string prefix)			const	override;
	std::string		_envName;

private:
	void initEnvName();
};



class jaspState_Interface : public jaspObject_Interface
{
public:
	jaspState_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void setObject(Rcpp::RObject obj)	{			((jaspState*)(myJaspObject))->setObject(obj);	}
	Rcpp::RObject getObject()			{ return	((jaspState*)(myJaspObject))->getObject();		}
};

RCPP_EXPOSED_CLASS_NODECL(jaspState_Interface)

