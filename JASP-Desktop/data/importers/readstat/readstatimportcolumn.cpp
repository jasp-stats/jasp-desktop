#include "ReadStatImportColumn.h"
#include "utils.h"
#include "log.h"

using namespace std;

ReadStatImportColumn::ReadStatImportColumn(ImportDataSet* importDataSet, string name, std::string labelsID, Column::ColumnType columnType)
	: ImportColumn(importDataSet, name), _labelsID(labelsID), _type(columnType)
{}

ReadStatImportColumn::~ReadStatImportColumn()
{}

size_t ReadStatImportColumn::size() const
{
	switch(_type)
	{
	case Column::ColumnTypeUnknown:		return 0;
	case Column::ColumnTypeScale:		return _doubles.size();
	case Column::ColumnTypeOrdinal:		[[clang::fallthrough]];
	case Column::ColumnTypeNominal:		return _ints.size();
	case Column::ColumnTypeNominalText:	return _strings.size();
	}
}

std::string ReadStatImportColumn::valueAsString(size_t row) const
{
	if(row >= size())	return Utils::emptyValue;

	switch(_type)
	{
	case Column::ColumnTypeUnknown:		return Utils::emptyValue;
	case Column::ColumnTypeScale:		return std::to_string(_doubles[row]);
	case Column::ColumnTypeOrdinal:		[[clang::fallthrough]];
	case Column::ColumnTypeNominal:		return std::to_string(_ints[row]);
	case Column::ColumnTypeNominalText:	return _strings[row];
	}

}

bool ReadStatImportColumn::isValueEqual(Column &col, size_t row) const
{
	return row < size() && isStringValueEqual(valueAsString(row), col, row);
}


void ReadStatImportColumn::addValue(const string & val)
{
	if(_ints.size() == 0 && _doubles.size() == 0)
		_type = Column::ColumnTypeNominalText; //If we haven't added anything else and the first value is a string then the rest should also be a string from now on

	switch(_type)
	{
	case Column::ColumnTypeUnknown:
		_type = Column::ColumnTypeNominalText;
		[[clang::fallthrough]];

	case Column::ColumnTypeNominalText:
		_strings.push_back(val);
		break;

	case Column::ColumnTypeScale:
	{
		double dblVal;
		if(convertValueToDouble(val, dblVal))	addValue(dblVal);
		else									addMissingValue();

		break;
	}

	case Column::ColumnTypeOrdinal:
	case Column::ColumnTypeNominal:
	{
		int intVal;
		if(convertValueToInt(val, intVal))	addValue(intVal);
		else								addMissingValue();

		break;
	}
	}
}

void ReadStatImportColumn::addValue(const double & val)
{
	switch(_type)
	{
	case Column::ColumnTypeUnknown:
		_type = Column::ColumnTypeScale;
		[[clang::fallthrough]];

	case Column::ColumnTypeScale:
		_doubles.push_back(val);
		break;

	case Column::ColumnTypeNominalText:
		addValue(std::to_string(val));
		break;

	case Column::ColumnTypeOrdinal:
	case Column::ColumnTypeNominal:
		addValue(int(val));
		break;
	}
}

void ReadStatImportColumn::addValue(const int & val)
{
	switch(_type)
	{
	case Column::ColumnTypeUnknown:
		_type = Column::ColumnTypeOrdinal;
		[[clang::fallthrough]];

	case Column::ColumnTypeOrdinal:
	case Column::ColumnTypeNominal:
		_ints.push_back(val);
		break;

	case Column::ColumnTypeNominalText:
		addValue(std::to_string(val));
		break;

	case Column::ColumnTypeScale:
		addValue(double(val));
		break;
	}
}

void ReadStatImportColumn::addLabel(const int & val, const std::string & label)
{
	if(!(_type == Column::ColumnTypeOrdinal || _type == Column::ColumnTypeNominal))
		Log::log() << "Column type being imported through readstat is not ordinal or nominal but receives an int as value for label " << label << std::endl;
	else
		_intLabels[val] = label;
}

void ReadStatImportColumn::addLabel(const std::string & val, const std::string & label)
{
	if(_ints.size() == 0 && _doubles.size() == 0)
		_type = Column::ColumnTypeNominalText; //If we haven't added anything else and the first value is a string then the rest should also be a string from now on

	if(_type != Column::ColumnTypeNominalText)
		Log::log() << "Column type being imported through readstat is not nominal-text but receives a string as value for label " << label << std::endl;
	else
		_strLabels[val] = label;
}

void ReadStatImportColumn::addMissingValue()
{
	switch(_type)
	{
	case Column::ColumnTypeUnknown:												return;
	case Column::ColumnTypeScale:		_doubles.push_back(NAN);				return;
	case Column::ColumnTypeOrdinal:		[[clang::fallthrough]];
	case Column::ColumnTypeNominal:		_ints.push_back(INT_MIN);				return;
	case Column::ColumnTypeNominalText:	_strings.push_back(Utils::emptyValue);	return;
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
