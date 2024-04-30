#include "readstatimportcolumn.h"
#include "readstatimportdataset.h"
#include "utils.h"
#include "columnutils.h"
#include "log.h"

using namespace std;

ReadStatImportColumn::ReadStatImportColumn(readstat_variable_t * readstat_var, ReadStatImportDataSet* importDataSet, string name, std::string title, std::string labelsID, columnType columnType)
    : ImportColumn(importDataSet, name, title), _readstatDataSet(importDataSet), _readstatVariable(readstat_var), _labelsID(labelsID), _type(columnType)
{}

ReadStatImportColumn::~ReadStatImportColumn()
{}

size_t ReadStatImportColumn::size() const
{
	return _values.size();
}

const stringvec & ReadStatImportColumn::allValuesAsStrings() const
{
	return _values;
}

void ReadStatImportColumn::addLabel(const std::string & val, const std::string & label)
{
	//Log::log() << "ReadStatImportColumn::addLabel(str '" << val << "', '" << label << "');" <<std::endl;

	_strLabels[val] = label;
}

void ReadStatImportColumn::addMissingValue(const std::string & missingValue)
{
	_missing.insert(missingValue);
}

std::string ReadStatImportColumn::readstatValueToString(const readstat_value_t & value)
{
	readstat_type_t	type	= readstat_value_type(value);

	switch(type)
	{
	case READSTAT_TYPE_STRING:		return										(			readstat_string_value(value)	);
	case READSTAT_TYPE_INT8:		return	std::to_string						(int(		readstat_int8_value(value))		);
	case READSTAT_TYPE_INT16:		return	std::to_string						(int(		readstat_int16_value(value))	);
	case READSTAT_TYPE_INT32:		return	std::to_string						(int(		readstat_int32_value(value))	);
	case READSTAT_TYPE_FLOAT:		return	ColumnUtils::doubleToStringMaxPrec	(			readstat_float_value(value)		);
	case READSTAT_TYPE_DOUBLE:		return	ColumnUtils::doubleToStringMaxPrec	(			readstat_double_value(value)	);
	case READSTAT_TYPE_STRING_REF:	throw	std::runtime_error("File contains string references and we do not support this.");
	}

	return "???";
}

const stringvec &ReadStatImportColumn::labels() const
{
	static stringvec local;
	
	local = _values;
	
	for(size_t i=0; i<_values.size(); i++)
		if(_strLabels.count(_values[i]))
			local[i] = _strLabels.at(_values[i]);
	
	return local;
}

void ReadStatImportColumn::addValue(const readstat_value_t & value)
{
	bool			setMiss	= readstat_value_is_tagged_missing(value) || (_readstatVariable && readstat_value_is_defined_missing(value, _readstatVariable));
	std::string		valStr	= ColumnUtils::doubleToString(EmptyValues::missingValueDouble);

	if(readstat_value_is_tagged_missing(value)) //This is from sas/stata and actual value is NaN but there is a tag. So we use that as a value, this will be converted to NaN later anyway
	{
		valStr = readstat_value_tag(value);
		valStr = "." + valStr; //Apparently this is shown in Stata as ".a" or ".b" (depending on the tag)
	}
	else if(!readstat_value_is_system_missing(value))
		valStr = readstatValueToString(value);

	_values.push_back(valStr);

	if(setMiss)
		addMissingValue(valStr);
}
