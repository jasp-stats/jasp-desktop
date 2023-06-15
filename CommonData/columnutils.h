#ifndef COLUMNUTILS_H
#define COLUMNUTILS_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include "utils.h"
#include "stringutils.h"

class ColumnUtils
{
public:
	friend class PreferencesModel;

	static       std::string emptyValue;
	static const stringset	& getEmptyValues()			{ return _currentEmptyValues;		}
	static const stringset	& getDefaultEmptyValues()	{ return _defaultEmptyValues;		}
	static const doublevec	& getDoubleEmptyValues()	{ return _currentDoubleEmptyValues;	}
	
	static void setEmptyValues(const stringset & emptyvalues);
	static void processEmptyValues();

	static bool getIntValue(	const std::string	& value, int	& intValue);
	static bool getIntValue(	const double		& value, int	& intValue);
	static bool getDoubleValue(	const std::string	& value, double	& doubleValue);

	static bool isEmptyValue(	const double		& val);
	static bool isEmptyValue(	const std::string	& val);
	static bool isIntValue(		const std::string	& value);
	static bool isDoubleValue(	const std::string	& value);

	static bool	convertValueToIntForImport(		const	std::string & strValue, int &intValue);
	static bool	convertValueToDoubleForImport(	const	std::string & strValue, double &doubleValue);
	static void	convertEscapedUnicodeToUTF8(			std::string & inputStr);

	static bool convertVecToInt(	const stringvec &values, intvec		& intValues,	intset		& uniqueValues, intstrmap & emptyValuesMap);
	static bool convertVecToDouble(	const stringvec &values, doublevec	& doubleValues,								intstrmap	& emptyValuesMap);

	static std::string	doubleToString(			double dbl, int precision = 10);
	static std::string	doubleToDisplayString(	double dbl, bool fancyEmptyValue = true); ///< fancyEmptyValue is the user-settable missing value label, for saving to csv this might be less practical though, so turn it off

private:
	static void			_deEuropeaniseForImport(		std::string & value);
	static std::string _convertEscapedUnicodeToUTF8(	std::string hex);

	static stringset		_currentEmptyValues;
	static const stringset	_defaultEmptyValues;
	static doublevec		_currentDoubleEmptyValues;
};

#endif // COLUMNUTILS_H
