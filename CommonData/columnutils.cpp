#include "columnutils.h"

#ifndef IGNORE_BOOST
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string/predicate.hpp>
#endif
#include <codecvt>
#include <regex>

using namespace std;
using namespace boost::posix_time;
using namespace boost;


///Internally the following actual missing values are used for scalar, ordinal/nominal and text:  NAN, std::numeric_limits<int>::lowest() and ""
/// However, users might like to see something else hence we allow `emptyValue` to be changed through the settings.
	  string			ColumnUtils::emptyValue					= "";
const stringset			ColumnUtils::_defaultEmptyValues		= {"NaN", "nan", ".", "NA"};
doublevec				ColumnUtils::_currentDoubleEmptyValues	= {};
stringset				ColumnUtils::_currentEmptyValues		= ColumnUtils::_defaultEmptyValues;



void ColumnUtils::setEmptyValues(const stringset &emptyvalues)
{
	JASPTIMER_SCOPE(ColumnUtils::setEmptyValues);
	
	_currentEmptyValues = emptyvalues;
	processEmptyValues();
}

void ColumnUtils::processEmptyValues()
{
	JASPTIMER_SCOPE(ColumnUtils::processEmptyValues);
	
	_currentDoubleEmptyValues.clear();

	for (const std::string & curEmptyVal : _currentEmptyValues)
	{
		double doubleValue;
		if (ColumnUtils::getDoubleValue(curEmptyVal, doubleValue))
			_currentDoubleEmptyValues.push_back(doubleValue);
	}
}

bool ColumnUtils::getIntValue(const string &value, int &intValue)
{
	try
	{
		intValue = boost::lexical_cast<int>(value);
		return true;
	}
	catch (...)	{}

	return false;
}

bool ColumnUtils::isIntValue(const string &value)
{
	try
	{
		boost::lexical_cast<int>(value);
		return true;
	}
	catch (...)	{}

	return false;
}

bool ColumnUtils::getIntValue(const double &value, int &intValue)
{
	JASPTIMER_SCOPE(ColumnUtils::getIntValue);
	
	try
	{
		double intPart;

		if (modf(value, &intPart) == 0.0)
		{
			if (intPart <=  std::numeric_limits<int>::max() && intPart >= std::numeric_limits<int>::lowest())
			{
				intValue = int(intPart);
				return true;
			}
		}
	}
	catch (...) {}

	return false;
}

bool ColumnUtils::getDoubleValue(const string &value, double &doubleValue)
{
	try
	{
		doubleValue = boost::lexical_cast<double>(value);
		return true;
	}
	catch (...) {}

	return false;
}


bool ColumnUtils::isDoubleValue(const string &value)
{
	try
	{
		boost::lexical_cast<double>(value);
		return true;
	}
	catch (...)	{}

	return false;
}


void ColumnUtils::_deEuropeaniseForImport(std::string & value)
{
	int dots	= 0,
		commas	= 0;

	for (const char & k : value)
		if		(k == '.')	dots++;
		else if	(k == ',')	commas++;
	
	if(commas == 1 && dots == 0)
		for (char & k : value)	
			if(k == ',')
			{
				k = '.';
				return;
			}
	
	if (commas > 0)
	{
		std::string uneurope = value;

		if (dots > 0)
		{
			size_t	i = 0,
					j = 0;

			for (;i < value.size(); i++)
				if (value[i] != '.')
				{
					uneurope[j] = value[i];
					j++;
				}

			uneurope.resize(j);
		}

		for (size_t i = 0; i < uneurope.length(); i++)
			if (uneurope[i] == ',')
			{
				uneurope[i] = '.';
				break;
			}

		value = uneurope;
	}
}

bool ColumnUtils::isEmptyValue(const std::string& val)
{
	if (val.empty()) 
		return true;

	return _currentEmptyValues.count(val);
}

bool ColumnUtils::isEmptyValue(const double &val)
{
	JASPTIMER_SCOPE(ColumnUtils::isEmptyValue double);
	
	if (std::isnan(val)) return true;

	return std::find(_currentDoubleEmptyValues.begin(), _currentDoubleEmptyValues.end(), val) != _currentDoubleEmptyValues.end();
}

bool ColumnUtils::convertValueToIntForImport(const std::string &strValue, int &intValue)
{
	JASPTIMER_SCOPE(ColumnUtils::convertValueToIntForImport);
	
	if (!isEmptyValue(strValue))
	{
		if (!ColumnUtils::getIntValue(strValue, intValue))
			return false;
	}
	else
		intValue = std::numeric_limits<int>::lowest();

	return true;
}

bool ColumnUtils::convertValueToDoubleForImport(const std::string & strValue, double & doubleValue)
{
	std::string v = strValue;
	_deEuropeaniseForImport(v);

	if(isEmptyValue(v))
		doubleValue = NAN;
	
	else if (!ColumnUtils::getDoubleValue(v, doubleValue))
		return false;
	
	return true;
}

std::string ColumnUtils::doubleToString(double dbl, int precision)
{
	JASPTIMER_SCOPE(ColumnUtils::doubleToString);
	
	std::stringstream conv; //Use this instead of std::to_string to make sure there are no trailing zeroes (and to get full precision)
	conv << std::setprecision(precision);
	conv << dbl;
	return conv.str();
}

std::string ColumnUtils::doubleToDisplayString(double dbl, bool fancyEmptyValue)
{
	JASPTIMER_SCOPE(ColumnUtils::doubleToDisplayString);

	if (dbl > std::numeric_limits<double>::max())				return "∞";
	else if (dbl < std::numeric_limits<double>::lowest())		return "-∞";
	else if (ColumnUtils::isEmptyValue(dbl))					return fancyEmptyValue ? ColumnUtils::emptyValue : "";
	else														return ColumnUtils::doubleToString(dbl);
}



bool ColumnUtils::convertVecToInt(const std::vector<std::string> &values, std::vector<int> &intValues, std::set<int> &uniqueValues, std::map<int, std::string> &emptyValuesMap)
{
	JASPTIMER_SCOPE(ColumnUtils::convertVecToInt);
	
	emptyValuesMap.clear();
	uniqueValues.clear();
	intValues.clear();
	intValues.reserve(values.size());

	int row = 0;

	for (const std::string &value : values)
	{
		int intValue = std::numeric_limits<int>::lowest();

		if (ColumnUtils::convertValueToIntForImport(value, intValue))
		{
			if (intValue != std::numeric_limits<int>::lowest())	uniqueValues.insert(intValue);
			else if (!value.empty())							emptyValuesMap.insert(make_pair(row, value));

			intValues.push_back(intValue);
		}
		else
		{
			intValues.clear();
			return false;
		}

		row++;
	}

	return true;
}


bool ColumnUtils::convertVecToDouble(const stringvec & values, doublevec & doubleValues, intstrmap & emptyValuesMap)
{
	JASPTIMER_SCOPE(ColumnUtils::convertVecToDouble);
	
	emptyValuesMap.clear();
	doubleValues.clear();
	doubleValues.resize(values.size());

	int row = 0;
	for (const std::string & value : values)
	{
		double doubleValue = static_cast<double>(NAN);

		if (ColumnUtils::convertValueToDoubleForImport(value, doubleValue))
		{
			doubleValues[row] = doubleValue;

			if (std::isnan(doubleValue) && value != ColumnUtils::emptyValue)
				emptyValuesMap.insert(std::make_pair(row, value));
		}
		else
		{
			doubleValues.clear();
			return false;
		}

		row++;
	}

	return true;
}

// hex should be 4 hexadecimals characters
std::string ColumnUtils::_convertEscapedUnicodeToUTF8(std::string hex)
{
	JASPTIMER_SCOPE(ColumnUtils::_convertEscapedUnicodeToUTF8);
			
	std::istringstream iss(hex);

	uint32_t bytes;
#ifdef _WIN32
	static std::wstring_convert<std::codecvt_utf8<unsigned int>, unsigned int> conv;
#else
	static std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
#endif
	// Read the 4 hexadecimals as bytes, and convert these bytes into UTF8.
	if (iss >> std::hex >> bytes) hex = conv.to_bytes(char32_t(bytes));

	return hex;
}

// Replace all <U+FFFF> in str by their UT8 characters.
void ColumnUtils::convertEscapedUnicodeToUTF8(std::string& inputStr)
{
	JASPTIMER_SCOPE(ColumnUtils::convertEscapedUnicodeToUTF8);
	
	static const std::regex unicodeExpression ("<U\\+([0-9a-fA-F]{4})>");

	std::smatch match;
	auto begin	= inputStr.cbegin();

	while (std::regex_search(begin, inputStr.cend(), match, unicodeExpression))
	{
		std::string utf8 = _convertEscapedUnicodeToUTF8(match[1].str()); // match 1 is the first group of the regexp: that is the 4 hexadecimals.
		auto pos = match.position(0); // position of the whole sequence in str.
		inputStr.replace(begin + pos, begin + pos + 8, utf8); // 8 is the number of characters of '<U+FFFF>'
		// str iterators cannot be trusted after replace. They must be recalculated from str.
		begin = inputStr.begin() + pos;
	}
}
