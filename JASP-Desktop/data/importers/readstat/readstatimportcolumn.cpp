#include "readstatimportcolumn.h"
#include "utils.h"
#include "log.h"

using namespace std;

ReadStatImportColumn::ReadStatImportColumn(ImportDataSet* importDataSet, string name, std::string labelsID, columnType columnType)
	: ImportColumn(importDataSet, name), _labelsID(labelsID), _type(columnType)
{}

ReadStatImportColumn::~ReadStatImportColumn()
{}

size_t ReadStatImportColumn::size() const
{
	switch(_type)
	{
	default:							return 0;
	case columnType::scale:		return _doubles.size();
	case columnType::ordinal:		[[clang::fallthrough]];
	case columnType::nominal:		return _ints.size();
	case columnType::nominalText:	return _strings.size();
	}
}

std::string ReadStatImportColumn::valueAsString(size_t row) const
{
	if(row >= size())	return Utils::emptyValue;

	switch(_type)
	{
	default:							return Utils::emptyValue;
	case columnType::scale:		return std::to_string(_doubles[row]);
	case columnType::ordinal:		[[clang::fallthrough]];
	case columnType::nominal:		return std::to_string(_ints[row]);
	case columnType::nominalText:	return _strings[row];
	}

}


std::vector<std::string> ReadStatImportColumn::allValuesAsStrings() const
{
	std::vector<std::string> strs;
	strs.reserve(size());

	for(size_t row = 0; row<size(); row++)
		strs.push_back(valueAsString(row));

	return strs;
}


void ReadStatImportColumn::addValue(const string & val)
{
	if(_ints.size() == 0 && _doubles.size() == 0)
		_type = columnType::nominalText; //If we haven't added anything else and the first value is a string then the rest should also be a string from now on

	switch(_type)
	{
	case columnType::unknown:
		_type = columnType::nominalText;
		[[clang::fallthrough]];

	case columnType::nominalText:
		_strings.push_back(val);
		break;

	case columnType::scale:
	{
		double dblVal;
		if(Utils::convertValueToDoubleForImport(val, dblVal))	addValue(dblVal);
		else													addMissingValue();

		break;
	}

	case columnType::ordinal:
	case columnType::nominal:
	{
		int intVal;
		if(Utils::convertValueToIntForImport(val, intVal))	addValue(intVal);
		else												addMissingValue();

		break;
	}
	}
}

void ReadStatImportColumn::addValue(const double & val)
{
	switch(_type)
	{
	case columnType::unknown:
		_type = columnType::scale;
		[[clang::fallthrough]];

	case columnType::scale:
		_doubles.push_back(val);
		break;

	case columnType::nominalText:
		addValue(std::to_string(val));
		break;

	case columnType::ordinal:
	case columnType::nominal:
		addValue(int(val));
		break;
	}
}

void ReadStatImportColumn::addValue(const int & val)
{
	switch(_type)
	{
	case columnType::unknown:
		_type = columnType::ordinal;
		[[clang::fallthrough]];

	case columnType::ordinal:
	case columnType::nominal:
		_ints.push_back(val);
		break;

	case columnType::nominalText:
		addValue(std::to_string(val));
		break;

	case columnType::scale:
		addValue(double(val));
		break;
	}
}

void ReadStatImportColumn::addLabel(const int & val, const std::string & label)
{
	if(!(_type == columnType::ordinal || _type == columnType::nominal))
		Log::log() << "Column type being imported through readstat is not ordinal or nominal but receives an int as value for label " << label << std::endl;
	else
		_intLabels[val] = label;
}

void ReadStatImportColumn::addLabel(const std::string & val, const std::string & label)
{
	if(_ints.size() == 0 && _doubles.size() == 0)
		_type = columnType::nominalText; //If we haven't added anything else and the first value is a string then the rest should also be a string from now on

	if(_type != columnType::nominalText)
		Log::log() << "Column type being imported through readstat is not nominal-text but receives a string as value for label " << label << std::endl;
	else
		_strLabels[val] = label;
}

void ReadStatImportColumn::addMissingValue()
{
	switch(_type)
	{
	case columnType::unknown:												return;
	case columnType::scale:		_doubles.push_back(NAN);				return;
	case columnType::ordinal:		[[clang::fallthrough]];
	case columnType::nominal:		_ints.push_back(INT_MIN);				return;
	case columnType::nominalText:	_strings.push_back(Utils::emptyValue);	return;
	}
}

void ReadStatImportColumn::addValue(const readstat_value_t & value)
{
	readstat_type_t			type = readstat_value_type(value);

	if (!readstat_value_is_system_missing(value))
		switch(type)
		{
		case READSTAT_TYPE_STRING:		addValue(			readstat_string_value(value)	);	return;
		case READSTAT_TYPE_INT8:		addValue(int(		readstat_int8_value(value))		);	return;
		case READSTAT_TYPE_INT16:		addValue(int(		readstat_int16_value(value))	);	return;
		case READSTAT_TYPE_INT32:		addValue(int(		readstat_int32_value(value))	);	return;
		case READSTAT_TYPE_FLOAT:		addValue(double(	readstat_float_value(value))	);	return;
		case READSTAT_TYPE_DOUBLE:		addValue(			readstat_double_value(value)	);	return;
		}

	addMissingValue();
}
