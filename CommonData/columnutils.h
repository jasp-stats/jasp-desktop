#ifndef COLUMNUTILS_H
#define COLUMNUTILS_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include "utils.h"
#include "stringutils.h"
#include "column.h"

class ColumnUtils
{
public:
	friend class PreferencesModel;

	static       std::string emptyValue;
	
	static bool getIntValue(	const std::string	& value, int	& intValue);
	static bool getIntValue(	const double		& value, int	& intValue);
	static bool getDoubleValue(	const std::string	& value, double	& doubleValue);

	static bool isIntValue(		const std::string	& value);
	static bool isDoubleValue(	const std::string	& value);

	static bool	convertValueToIntForImport(		const	std::string & strValue, int		& intValue);
	static bool	convertValueToDoubleForImport(	const	std::string & strValue, double	& doubleValue);
	static void	convertEscapedUnicodeToUTF8(			std::string & inputStr);
	static void	deEuropeaniseForImport(					std::string & value);

	static std::string	doubleToString(			double dbl, int precision = 10);

private:
	static std::string _convertEscapedUnicodeToUTF8(	std::string hex);
};

#endif // COLUMNUTILS_H
