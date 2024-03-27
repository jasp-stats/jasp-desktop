#ifndef COLUMNUTILS_H
#define COLUMNUTILS_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include "utils.h"

class ColumnUtils
{
public:
	friend class PreferencesModel;

	static bool			getIntValue(	const std::string	& value, int	& intValue);
	static bool			getIntValue(	const double		& value, int	& intValue);
	static bool			getDoubleValue(	const std::string	& value, double	& doubleValue);
	static doubleset	getDoubleValues(const stringset & values, bool stripNAN = true);

	static bool			isIntValue(		const std::string	& value);
	static bool			isDoubleValue(	const std::string	& value);

	static void			convertEscapedUnicodeToUTF8(			std::string & inputStr);
	static std::string	deEuropeaniseForImport(					std::string   value);		//Convert a string to a double with a dot for a separator

	static std::string	doubleToString(			double dbl, int precision = 10);
	
	static bool			convertVecToInt(	const stringvec & values, intvec	& intValues, intset & uniqueValues);
	static bool			convertVecToDouble(	const stringvec & values, doublevec	& doubleValues);
private:
	static std::string _convertEscapedUnicodeToUTF8(	std::string hex);
};

#endif // COLUMNUTILS_H
