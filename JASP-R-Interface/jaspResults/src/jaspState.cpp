#include "jaspState.h"

Json::Value jaspState::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["ObjectSerialized"] = std::string(_stateObjectSerialized.begin(), _stateObjectSerialized.end());

	return obj;
}

void jaspState::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	std::string jsonPlotObjStr = in.get("ObjectSerialized", "").asString();

	_stateObjectSerialized = Rcpp::Vector<RAWSXP>(jsonPlotObjStr.begin(), jsonPlotObjStr.end());
}


void jaspState::setObject(Rcpp::RObject obj)
{
	Rcpp::Function serialize("serialize");

	_stateObjectSerialized = serialize(Rcpp::_["object"] = obj, Rcpp::_["connection"] = R_NilValue, Rcpp::_["ascii"] = true);
}

Rcpp::RObject jaspState::getObject()
{
	if(_stateObjectSerialized.size() == 0)
		return NULL;

	Rcpp::Function unserialize("unserialize");
	return unserialize(_stateObjectSerialized);
}


std::string jaspState::dataToString(std::string prefix)
{
	std::stringstream out;

	out << prefix << "object stored: "	<< ( _stateObjectSerialized.size() == 0 ? "no" : "yes") << "\n";

	return out.str();
}
