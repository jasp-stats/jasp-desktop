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
	default:						return _leadingMissingValues;
	case columnType::scale:			return _doubles.size();
	case columnType::ordinal:		[[clang::fallthrough]];
	case columnType::nominal:		return _ints.size();
	case columnType::nominalText:	return _strings.size();
	}
}

std::string ReadStatImportColumn::doubleAsString(double dbl)	const
{
	std::stringstream conv; //Use this instead of std::to_string to make sure there are no trailing zeroes
	conv << dbl;
	return conv.str();
}

std::string ReadStatImportColumn::valueAsString(size_t row) const
{
	if(row >= size())	return Utils::emptyValue;

	switch(_type)
	{
	default:						return Utils::emptyValue;
	case columnType::scale:			return doubleAsString(_doubles[row]);
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

void ReadStatImportColumn::setType(columnType newType)
{
	if(_type == newType)
		return;

	//Log::log() << "Changing columntype of '" << _name << "'\tto " << columnTypeToString(newType) << "\tfrom " << columnTypeToString(_type) << std::endl;

	auto conversionFailed = [&](){
		throw std::runtime_error("An attempt was made to change the columntype of '" + _name + "' to "+ columnTypeToString(newType) + " from " + columnTypeToString(_type) + " but the conversion failed...");
	};

	switch(_type)
	{
	case columnType::unknown:
		_type = newType;
		addLeadingMissingValues();
		return;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::scale:
		switch(newType)
		{
		case columnType::unknown:		throw std::runtime_error("Changing the importcolumns type back to unknown from " + columnTypeToString(_type) + " is not allowed!");

		case columnType::ordinal:		[[clang::fallthrough]];
		case columnType::nominal:
			for(double d : _doubles)
				if(isMissingValue(d))			_ints.push_back(missingValueInt());
				else if(d != double(int(d)))	conversionFailed();
				else							_ints.push_back(int(d));
			break;

		case columnType::nominalText:
			for(double d : _doubles)
				_strings.push_back(isMissingValue(d) ? missingValueString() : doubleAsString(d));
			_doubles.clear();
			break;
		}
		break;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::ordinal:			[[clang::fallthrough]];
	case columnType::nominal:
		switch(newType)
		{
		case columnType::unknown:		throw std::runtime_error("Changing the importcolumns type back to unknown from " + columnTypeToString(_type) + " is not allowed!");
		case columnType::ordinal:		[[clang::fallthrough]];
		case columnType::nominal:
			_type = newType; //Ordinal <> Nominal?
			return;

		case columnType::scale:
			for(int i : _ints)
				_doubles.push_back(isMissingValue(i) ? missingValueDouble() : i);
			_ints.clear();
			break;

		case columnType::nominalText:
			for(int i : _ints)
				_strings.push_back(isMissingValue(i) ? missingValueString() : std::to_string(i));
			_ints.clear();
			break;
		}
		break;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::nominalText:
		switch(newType)
		{
		case columnType::unknown:		throw std::runtime_error("Changing the importcolumns type back to unknown from " + columnTypeToString(_type) + " is not allowed!");
		case columnType::scale:
			for(const std::string & str : _strings)
			{
				double dblVal;
				if(isMissingValue(str))										_doubles.push_back(missingValueDouble());
				else if(Utils::convertValueToDoubleForImport(str, dblVal))	_doubles.push_back(dblVal);
				else														conversionFailed();
			}
			break;

		case columnType::ordinal:	[[clang::fallthrough]];
		case columnType::nominal:
			for(const std::string & str : _strings)
			{
				int intVal;
				if(isMissingValue(str))									_ints.push_back(missingValueInt());
				else if(Utils::convertValueToIntForImport(str, intVal))	_ints.push_back(intVal);
				else													conversionFailed();
			}
			break;
		}
		break;
	}

	_type = newType;
}

void ReadStatImportColumn::addLeadingMissingValues()
{
	if(_type == columnType::unknown)
		return;

	if(_leadingMissingValues > 0)
		for(size_t i=0; i< _leadingMissingValues; i++)
			addMissingValue();

	_leadingMissingValues = 0;
}

void ReadStatImportColumn::addValue(const string & val)
{
	if(_type == columnType::unknown || (_ints.size() == 0 && _doubles.size() == 0))
		setType(columnType::nominalText); //If we haven't added anything else and the first value is a string then the rest should also be a string from now on


	switch(_type)
	{
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
	case columnType::ordinal:		[[clang::fallthrough]]; //How does ordinal with doubles make sense? Well, it doesn't!
	case columnType::unknown:
		setType(columnType::scale);
		[[clang::fallthrough]];

	case columnType::scale:
		_doubles.push_back(val);
		break;

	case columnType::nominal:
		setType(columnType::nominalText); //Nominal with float also doesn't work.. Lets make sure any previous ints are converted to string
		[[clang::fallthrough]];

	case columnType::nominalText:
		addValue(doubleAsString(val));
		break;
	}
}

void ReadStatImportColumn::addValue(const int & val)
{
	switch(_type)
	{
	case columnType::unknown:
		setType(columnType::ordinal);
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
	if(_type == columnType::nominalText)
	{
		Log::log() << "Column '" << name() << "' being imported through readstat is of type nominaltext but receives an int (" << val << ") as value for label '" << label << "'. Ignoring it!" << std::endl;
		return;
	}

	if(_type == columnType::unknown)
		setType(columnType::nominal);


	if(_type == columnType::scale)
	{
		Log::log() << "Column '" << name() << "' being imported through readstat was of type scale but receives an int (" << val << ") as value for label '" << label << "'. Converting it's type to ordinal!" << std::endl;
		setType(columnType::ordinal);
	}

	_intLabels[val] = label;
}

void ReadStatImportColumn::addLabel(const std::string & val, const std::string & label)
{
	if(_ints.size() == 0 && _doubles.size() == 0)
		setType(columnType::nominalText); //If we haven't added anything else and the first value is a string then the rest should also be a string from now on

	if(_type != columnType::nominalText)
		setType(columnType::nominalText);

	_strLabels[val] = label;
}

void ReadStatImportColumn::addMissingValue()
{
	switch(_type)
	{
	case columnType::unknown:		_leadingMissingValues++;					return;
	case columnType::scale:			_doubles.push_back(missingValueDouble());	return;
	case columnType::ordinal:		[[clang::fallthrough]];
	case columnType::nominal:		_ints.push_back(missingValueInt());			return;
	case columnType::nominalText:	_strings.push_back(missingValueString());	return;
	}
}

bool ReadStatImportColumn::isMissingValue(std::string s) const
{
	return s == Utils::emptyValue;
}

std::string  ReadStatImportColumn::missingValueString()	const
{
	return Utils::emptyValue;
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
