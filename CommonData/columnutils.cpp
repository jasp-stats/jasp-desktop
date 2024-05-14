#include "columnutils.h"
#include "utils.h"
//#include "emptyvalues.h"

#ifndef IGNORE_BOOST
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string/predicate.hpp>
#endif
#include <codecvt>
#include <regex>
#include "emptyvalues.h"
#include "timers.h"


using namespace std;
using namespace boost::posix_time;
using namespace boost;

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
			if (intPart <=  std::numeric_limits<int>::max() && intPart >= EmptyValues::missingValueInteger)
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
	if(value == "∞" || value == "-∞")
	{
		doubleValue = std::numeric_limits<double>::infinity() * (value == "-∞" ? -1 : 1);
		return true;
	}
	
	try
	{
		doubleValue = boost::lexical_cast<double>(deEuropeaniseForImport(value));
		return true;
	}
	catch (...) {}

	return false;
}

doubleset ColumnUtils::getDoubleValues(const stringset & values, bool stripNAN)
{
	doubleset result;
	for (const std::string & val : values)
	{
		double doubleValue;
		if (getDoubleValue(val, doubleValue) && !std::isnan(doubleValue))
			result.insert(doubleValue);
	}

	return result;
}

bool ColumnUtils::isDoubleValue(const string &value)
{
	static double last;
	return getDoubleValue(value, last);
}


bool ColumnUtils::convertVecToInt(const stringvec &values, intvec & intValues, intset & uniqueValues)
{
	JASPTIMER_SCOPE(ColumnUtils::convertVecToInt);
	
	uniqueValues.clear();
	intValues.clear();
	intValues.reserve(values.size());
	
	int row = 0;
	
	for (const std::string &value : values)
	{
		int intValue = EmptyValues::missingValueInteger;
		
		if (ColumnUtils::getIntValue(value, intValue))
		{
			if (intValue != EmptyValues::missingValueInteger)
				uniqueValues.insert(intValue);
			
			intValues.push_back(intValue);
		}
		else
		{
			std::vector<int>().swap(intValues); //this clears intValues and guarentees its memory is released
			return false;
		}
		
		row++;
	}
	
	return true;
}

bool ColumnUtils::convertVecToDouble(const stringvec & values, doublevec & doubleValues)
{
	JASPTIMER_SCOPE(ColumnUtils::convertVecToDouble);
	
	doubleValues.clear();
	doubleValues.resize(values.size());
	
	int row = 0;
	for (const std::string & value : values)
	{
		double doubleValue = static_cast<double>(EmptyValues::missingValueDouble);
		
		if (ColumnUtils::getDoubleValue(value, doubleValue))
			doubleValues[row] = doubleValue;
		else
		{
			std::vector<double>().swap(doubleValues); //this clears doubleValues and guarentees its memory is released
			return false;
		}
		
		row++;
	}
	
	return true;
}

std::string ColumnUtils::deEuropeaniseForImport(std::string value)
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
				return value;
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
	
	return value;
}

std::string ColumnUtils::doubleToStringMaxPrec(double dbl)
{
	constexpr auto max_precision{std::numeric_limits<long double>::digits10 + 1};
	return 	doubleToString(dbl, max_precision);
}

std::string ColumnUtils::doubleToString(double dbl, int precision)
{
	JASPTIMER_SCOPE(ColumnUtils::doubleToString);
	
	if (dbl > std::numeric_limits<double>::max())		return "∞";
	if (dbl < std::numeric_limits<double>::lowest())	return "-∞";
	
	std::stringstream conv; //Use this instead of std::to_string to make sure there are no trailing zeroes (and to get full precision)
	conv << std::setprecision(precision);
	conv << dbl;
	return conv.str();
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
