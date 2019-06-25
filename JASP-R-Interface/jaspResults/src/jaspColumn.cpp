
#include "jaspColumn.h"

#ifdef JASP_R_INTERFACE_LIBRARY
#include "jasprcpp.h"
#else
bool jaspRCPP_setColumnDataAsScale(			std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
bool jaspRCPP_setColumnDataAsOrdinal(		std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
bool jaspRCPP_setColumnDataAsNominal(		std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
bool jaspRCPP_setColumnDataAsNominalText(	std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
#endif


Json::Value jaspColumn::convertToJSON()
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["columnName"]	= _columnName;
	obj["columnType"]	= jaspColumnTypeToString(_columnType);

	return obj;
}

void jaspColumn::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_columnName = in["columnName"].asString();
	_columnType	= jaspColumnTypeFromString(in["columnType"].asString());
	_changed	= false;
}

std::string jaspColumn::dataToString(std::string prefix)
{
	std::stringstream out;

	out << prefix << "column " << _columnName << " has type " << jaspColumnTypeToString(_columnType) << " and had " << (_changed? "" : "no ") << "changes!\n";

	return out.str();
}

void jaspColumn::setScale(Rcpp::RObject scalarData)
{
	_changed	= jaspRCPP_setColumnDataAsScale(_columnName, scalarData);//		|| _columnType != jaspColumnType::scale;
	_columnType = jaspColumnType::scale;

	if(_changed) notifyParentOfChanges();

}

void jaspColumn::setOrdinal(Rcpp::RObject ordinalData)
{
	_changed	= jaspRCPP_setColumnDataAsOrdinal(_columnName, ordinalData);//	|| _columnType != jaspColumnType::ordinal;
	_columnType = jaspColumnType::ordinal;

	if(_changed) notifyParentOfChanges();
}

void jaspColumn::setNominal(Rcpp::RObject nominalData)
{
	_changed	= jaspRCPP_setColumnDataAsNominal(_columnName, nominalData);//	|| _columnType != jaspColumnType::nominal;
	_columnType = jaspColumnType::nominal;

	if(_changed) notifyParentOfChanges();
}

void jaspColumn::setNominalText(Rcpp::RObject nominalData)
{
	_changed	= jaspRCPP_setColumnDataAsNominalText(_columnName, nominalData);//	|| _columnType != jaspColumnType::text;
	_columnType = jaspColumnType::text;

	if(_changed) notifyParentOfChanges();
}


Json::Value jaspColumn::dataEntry()
{
	Json::Value data(jaspObject::dataEntry());

	data["columnName"]	= _columnName;
	data["columnType"]	= jaspColumnTypeToString(_columnType);
	data["dataChanged"]	= _changed;

	return data;
}
