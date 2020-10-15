
#include "jaspColumn.h"

#ifdef JASP_R_INTERFACE_LIBRARY
#include "jasprcpp.h"
#else
enum ColumnType { ColumnTypeUnknown = 0, ColumnTypeNominal = 1, ColumnTypeNominalText = 2, ColumnTypeOrdinal = 4, ColumnTypeScale = 8 };
bool	jaspRCPP_setColumnDataAsScale(			std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
bool	jaspRCPP_setColumnDataAsOrdinal(		std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
bool	jaspRCPP_setColumnDataAsNominal(		std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
bool	jaspRCPP_setColumnDataAsNominalText(	std::string, Rcpp::RObject) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false; };
#define ENUM_DECLARATION_CPP
#include "columntype.h"
columnType jaspRCPP_getColumnType(std::string columnName) { return columnType::unknown; }
#endif

jaspColumn::jaspColumn(std::string columnName)
	: jaspObject(jaspObjectType::column, "jaspColumn for " + columnName)
	, _columnName(columnName)
{
	switch(jaspRCPP_getColumnType(columnName))
	{
	case columnType::scale:			_columnType = jaspColumnType::scale;		break;
	case columnType::ordinal:		_columnType = jaspColumnType::ordinal;		break;
	case columnType::nominal:		_columnType = jaspColumnType::nominal;		break;
	case columnType::nominalText:	_columnType = jaspColumnType::nominalText;	break;
	default:						_columnType = jaspColumnType::unknown;		break;
	}
}


Json::Value jaspColumn::convertToJSON() const
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
	_dataChanged	= false;
}

std::string jaspColumn::dataToString(std::string prefix) const
{
	std::stringstream out;

	out << prefix << "column " << _columnName << " has type " << jaspColumnTypeToString(_columnType) << " and had " << (_dataChanged? "" : "no ") << "changes!\n";

	return out.str();
}

void jaspColumn::setScale(Rcpp::RObject scalarData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsScale(_columnName, scalarData);
	_typeChanged	= _columnType != jaspColumnType::scale;
	_columnType		= jaspColumnType::scale;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}

void jaspColumn::setOrdinal(Rcpp::RObject ordinalData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsOrdinal(_columnName, ordinalData);
	_typeChanged	= _columnType != jaspColumnType::ordinal;
	_columnType		= jaspColumnType::ordinal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}

void jaspColumn::setNominal(Rcpp::RObject nominalData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsNominal(_columnName, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominal;
	_columnType		= jaspColumnType::nominal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}

void jaspColumn::setNominalText(Rcpp::RObject nominalData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsNominalText(_columnName, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominalText;
	_columnType		= jaspColumnType::nominalText;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}


Json::Value jaspColumn::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

	data["columnName"]	= _columnName;
	data["columnType"]	= jaspColumnTypeToString(_columnType);
	data["dataChanged"]	= _dataChanged;
	data["typeChanged"]	= _typeChanged;

	return data;
}
