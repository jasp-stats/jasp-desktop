#ifndef COLUMNUTILS_H
#define COLUMNUTILS_H

#include <string>
#include <locale>
#include "utils.h"
#include <functional>

class ColumnUtils
{
public:
	typedef std::function<std::string(double, int)>		doubleF;
	typedef std::function<bool(std::string, double&)>	toDoubleF;
	typedef std::function<bool(std::string, int&)>		toIntF;

	friend class PreferencesModel;

	static bool					getIntValue(	const std::string	& value, int	& intValue);
	static bool					getIntValue(	const double		& value, int	& intValue);
	static bool					getDoubleValue(	const std::string	& value, double	& doubleValue);
	static doubleset			getDoubleValues(const stringset		& values, bool stripNAN = true);

	static bool					isIntValue(		const std::string	& value);
	static bool					isDoubleValue(	const std::string	& value);

	static void					convertEscapedUnicodeToUTF8(			std::string & inputStr);
	static std::string			deEuropeaniseForImport(					std::string   value);		//Convert a string to a double with a dot for a separator

	static std::string			doubleToString(			double dbl, int precision = 10);
	static std::string			doubleToStringMaxPrec(	double dbl);
	
	static bool					convertVecToInt(	const stringvec & values, intvec	& intValues, intset & uniqueValues);
	static bool					convertVecToDouble(	const stringvec & values, doublevec	& doubleValues);
	
	static void					setAlternativeDoubleToString(	doubleF		newDoubleFunc);
	static void					setExtraStringToNumber(			toDoubleF	newDoubleFunc, toIntF newIntFunc);
	static const std::string &	decimalPoint()							{ return _decimalPoint; }
	static void					setDecimalPoint(const std::string & p)	{ _decimalPoint = p;}
	
	static const std::string &	currentQLocaleId()							{ return _currentQLocaleId; }
	static void					setCurrentQLocaleId(const std::string & p)	{ _currentQLocaleId = p;}
	
private:	
	static std::string			_convertEscapedUnicodeToUTF8(	std::string hex);
	static doubleF				_alternativeDoubleToString;
	static toDoubleF			_extraStringToDouble;
	static toIntF				_extraStringToInt;
	static std::string			_decimalPoint,
								_currentQLocaleId;
};

#endif // COLUMNUTILS_H
