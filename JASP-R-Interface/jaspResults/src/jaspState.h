#pragma once
#include "jaspObject.h"

class jaspState : public jaspObject
{
public:
	jaspState(std::string title = "") : jaspObject(jaspObjectType::state, title) {}
	~jaspState() {}

	void setObject(Rcpp::RObject obj);
	Rcpp::RObject getObject();

	Json::Value convertToJSON() override;
	void		convertFromJSON_SetFields(Json::Value in) override;
	std::string dataToString(std::string prefix) override;


private:
	void initEnvName();

	std::string _envName;
};



class jaspState_Interface : public jaspObject_Interface
{
public:
	jaspState_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void setObject(Rcpp::RObject obj)	{			((jaspState*)(myJaspObject))->setObject(obj);	}
	Rcpp::RObject getObject()			{ return	((jaspState*)(myJaspObject))->getObject();		}
};

RCPP_EXPOSED_CLASS_NODECL(jaspState_Interface)

