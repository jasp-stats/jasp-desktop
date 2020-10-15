#include "readstatimportcolumn.h"
#include "utils.h"
#include "log.h"

using namespace std;

ReadStatImportColumn::ReadStatImportColumn(readstat_variable_t * readstat_var, ImportDataSet* importDataSet, string name, std::string labelsID, columnType columnType)
	: ImportColumn(importDataSet, name), _readstatVariable(readstat_var), _labelsID(labelsID), _type(columnType)
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

std::string ReadStatImportColumn::valueAsString(size_t row) const
{
	if(row >= size())	return Utils::emptyValue;

	switch(_type)
	{
	default:						return Utils::emptyValue;
	case columnType::scale:			return Utils::doubleToString(_doubles[row]);
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

bool ReadStatImportColumn::canConvertToType(columnType newType)
{
	if(_type == newType || _type == columnType::unknown)
		return true;

	if(newType == columnType::unknown)
		return false;

	switch(_type)
	{
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::scale:
		switch(newType)
		{
		case columnType::nominalText:	return true;
		case columnType::ordinal:		[[clang::fallthrough]];
		case columnType::nominal:
			for(double d : _doubles)
				if(!isMissingValue(d) && d != double(int(d)))
					return false;
			return true;
		}
		break;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::ordinal:			[[clang::fallthrough]];
	case columnType::nominal:
		switch(newType)
		{
		case columnType::ordinal:		[[clang::fallthrough]];
		case columnType::nominalText:	[[clang::fallthrough]];
		case columnType::nominal:		return true;
		case columnType::scale:			return _intLabels.size() == 0;
		}
		break;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::nominalText:
		switch(newType)
		{

		case columnType::scale:
			if(_strLabels.size() > 0)
				return false;

			for(const std::string & str : _strings)
			{
				double dblVal;
				if(!isMissingValue(str) && !Utils::convertValueToDoubleForImport(str, dblVal))
					return false;
			}

			return true;

		case columnType::ordinal:	[[clang::fallthrough]];
		case columnType::nominal:
		{
			int val;

			for(const std::string & str : _strings)
				if(!isMissingValue(str) && !Utils::convertValueToIntForImport(str, val))
					return false;

			for(const auto & strLabel : _strLabels)
				if(!Utils::convertValueToIntForImport(strLabel.first, val))
					return false;
			return true;
		}
		}
		break;
	}

	return false;
}

void ReadStatImportColumn::setType(columnType newType)
{
	if(_type == newType)
		return;

	Log::log() << "Changing columntype of '" << _name << "'\tto " << columnTypeToString(newType) << "\tfrom " << columnTypeToString(_type) << std::endl;

	auto conversionFailed = [&](const std::string & extraMsg){
		throw std::runtime_error("An attempt was made to change the columntype of '" + _name + "' to "+ columnTypeToString(newType) + " from " + columnTypeToString(_type) + " but the conversion failed.\n" + extraMsg);
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
				else if(d != double(int(d)))	conversionFailed("Double '" + Utils::doubleToString(d) + "' cannot be converted to int.");
				else							_ints.push_back(int(d));

			break;

		case columnType::nominalText:
			for(double d : _doubles)
				_strings.push_back(isMissingValue(d) ? missingValueString() : Utils::doubleToString(d));
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
			if(_intLabels.size() > 0)
				conversionFailed("Because we would have to drop labels.");

			for(int i : _ints)
				_doubles.push_back(isMissingValue(i) ? missingValueDouble() : i);
			_ints.clear();
			break;

		case columnType::nominalText:
			for(int i : _ints)
				_strings.push_back(isMissingValue(i) ? missingValueString() : std::to_string(i));
			_ints.clear();

			for(const auto & intLabel : _intLabels)
				_strLabels[std::to_string(intLabel.first)] = intLabel.second;
			_intLabels.clear();
			break;
		}
		break;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	case columnType::nominalText:
		switch(newType)
		{
		case columnType::unknown:		throw std::runtime_error("Changing the importcolumns type back to unknown from " + columnTypeToString(_type) + " is not allowed!");
		case columnType::scale:
			if(_strLabels.size() > 0)
				conversionFailed("Because we would have to drop some labels.");

			for(const std::string & str : _strings)
			{
				double dblVal;
				if(isMissingValue(str))										_doubles.push_back(missingValueDouble());
				else if(Utils::convertValueToDoubleForImport(str, dblVal))	_doubles.push_back(dblVal);
				else														conversionFailed("String '" + str + "' cannot be converted to double.");
			}
			break;

		case columnType::ordinal:	[[clang::fallthrough]];
		case columnType::nominal:
		{
			int val;
			for(const std::string & str : _strings)
				if(isMissingValue(str))									_ints.push_back(missingValueInt());
				else if(Utils::convertValueToIntForImport(str, val))	_ints.push_back(val);
				else													conversionFailed("String '" + str + "' cannot be converted to int.");

			for(const auto & strLabel : _strLabels)
				if(Utils::convertValueToIntForImport(strLabel.first, val))	_intLabels[val] = strLabel.second;
				else														conversionFailed("String key '" + strLabel.first + "' (for label '" + strLabel.second + "') cannot be converted to int.");
			_strLabels.clear();

			break;
		}
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
		if(Utils::convertValueToDoubleForImport(val, dblVal))
			addValue(dblVal);
		else
		{
			Log::log() << "Column '" << name() << "' being imported through readstat is of type " << _type << " but receives an string (" << val << ") as value. Converting type to nominaltext!" << std::endl;
			setType(columnType::nominalText);
			addValue(val);
			return;
		}
		break;
	}

	case columnType::ordinal:
	case columnType::nominal:
	{
		int intVal;
		if(Utils::convertValueToIntForImport(val, intVal))
			addValue(intVal);
		else
		{
			Log::log() << "Column '" << name() << "' being imported through readstat is of type " << _type << " but receives an string (" << val << ") as value. Converting type to nominaltext!" << std::endl;
			setType(columnType::nominalText);
			addValue(val);
			return;
		}
		break;
	}
	}
}

void ReadStatImportColumn::addValue(const double & val)
{
	switch(_type)
	{
	case columnType::ordinal:
		if(double(int(val)) == val)
		{
			addValue(int(val));
			return;
		}
		//Else if not integer val then:
		[[clang::fallthrough]];

	case columnType::unknown:
		setType(columnType::scale);
		[[clang::fallthrough]];

	case columnType::scale:
		_doubles.push_back(val);
		break;

	case columnType::nominal:
		if(double(int(val)) == val)
		{
			addValue(int(val));
			return;
		}
		//else not integral
		setType(columnType::nominalText);
		[[clang::fallthrough]];

	case columnType::nominalText:
		addValue(Utils::doubleToString(val));
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
	if(_type == columnType::unknown)
		setType(columnType::nominal);

	if(_type == columnType::nominalText)
	{
		Log::log() << "Column '" << name() << "' being imported through readstat is of type nominaltext but receives an int (" << val << ") as value for label '" << label << "'. Converting it to string!" << std::endl;
		addLabel(std::to_string(val), label);
		return;
	}

	if(_type == columnType::scale)
	{
		if(!canConvertToType(columnType::ordinal)) // then there might be some doubles already and then this wont work
		{
			Log::log() << "Column '" << name() << "' being imported through readstat was of type scale but receives an int (" << val << ") as value for label '" << label << "'. Converting it's type to nominal text because there are some doubles which we cannot convert to int!" << std::endl;
			addLabel(std::to_string(val), label);
			return;
		}
		else
		{
			Log::log() << "Column '" << name() << "' being imported through readstat was of type scale but receives an int (" << val << ") as value for label '" << label << "'. Converting it's type to ordinal!" << std::endl;
			setType(columnType::ordinal);
		}
	}

	_intLabels[val] = label;
}

void ReadStatImportColumn::addLabel(const double & val, const std::string & label)
{
	if(_type != columnType::nominalText)
	{
		if(double(int(val)) == val)
		{
			addLabel(int(val), label);
			return;
		}

		Log::log() << "Column '" << name() << "' being imported through readstat was of type " << _type << " but receives a double (" << val << ") as value for label '" << label << "'. Converting it's type to nominalText!" << std::endl;
		setType(columnType::nominalText); //Because we do not support having doubles as values for labels
	}

	addLabel(Utils::doubleToString(val), label);
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

bool ReadStatImportColumn::isMissingValue(std::string s)
{
	return s == Utils::emptyValue;
}

std::string  ReadStatImportColumn::missingValueString()
{
	return Utils::emptyValue;
}

std::string ReadStatImportColumn::readstatValueToString(const readstat_value_t & value)
{
	readstat_type_t	type	= readstat_value_type(value);

	switch(type)
	{
	case READSTAT_TYPE_STRING:		return					(			readstat_string_value(value)	);
	case READSTAT_TYPE_INT8:		return	std::to_string	(int(		readstat_int8_value(value))		);
	case READSTAT_TYPE_INT16:		return	std::to_string	(int(		readstat_int16_value(value))	);
	case READSTAT_TYPE_INT32:		return	std::to_string	(int(		readstat_int32_value(value))	);
	case READSTAT_TYPE_FLOAT:		return	std::to_string	(double(	readstat_float_value(value))	);
	case READSTAT_TYPE_DOUBLE:		return	std::to_string	(			readstat_double_value(value)	);
	case READSTAT_TYPE_STRING_REF:	throw	std::runtime_error("File contains string references and we do not support this.");
	}

	return "???";
}

void ReadStatImportColumn::addValue(const readstat_value_t & value)
{
	readstat_type_t	type	= readstat_value_type(value);
	bool			missing	= _readstatVariable ? readstat_value_is_missing(value, _readstatVariable) : readstat_value_is_system_missing(value);

	if (!missing)
		switch(type)
		{
		case READSTAT_TYPE_STRING:		addValue(			readstat_string_value(value)	);	return;
		case READSTAT_TYPE_INT8:		addValue(int(		readstat_int8_value(value))		);	return;
		case READSTAT_TYPE_INT16:		addValue(int(		readstat_int16_value(value))	);	return;
		case READSTAT_TYPE_INT32:		addValue(int(		readstat_int32_value(value))	);	return;
		case READSTAT_TYPE_FLOAT:		addValue(double(	readstat_float_value(value))	);	return;
		case READSTAT_TYPE_DOUBLE:		addValue(			readstat_double_value(value)	);	return;
		case READSTAT_TYPE_STRING_REF:	throw std::runtime_error("File contains string references and we do not support this.");
		}
	else
	{
		if(!readstat_value_is_system_missing(value))
			Log::log() << "Column '" << _name << "' has non-system missing value: '" << readstatValueToString(value) << "' dropping the value." << std::endl;
		addMissingValue();
	}
}

void ReadStatImportColumn::tryNominalMinusText()
{
	if(_type != columnType::nominalText)
		return;

	if(!canConvertToType(columnType::nominal))
		return;

	Log::log() << "Converting column '" << _name << "' from nominalText to nominal because all values can be converted to int without losing information." << std::endl;

	setType(columnType::nominal);
}
