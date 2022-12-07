#ifndef COLUMNUTILS_H
#define COLUMNUTILS_H

#include <string>
#include <vector>
#include <set>
#include <map>

class ColumnUtils
{
public:
	friend class PreferencesModel;

	static       std::string emptyValue;
	static const std::vector<std::string>	& getEmptyValues()			{ return _currentEmptyValues;		}
	static const std::vector<std::string>	& getDefaultEmptyValues()	{ return _defaultEmptyValues;		}
	static const std::vector<double>		& getDoubleEmptyValues()	{ return _currentDoubleEmptyValues;	}
	static void setEmptyValues(const std::vector<std::string>& emptyvalues);
	static void processEmptyValues();

	static bool getIntValue(const std::string& value, int& intValue);
	static bool getIntValue(const double& value, int& intValue);
	static bool getDoubleValue(const std::string& value, double& doubleValue);

	static bool isEmptyValue(const std::string& val);
	static bool isEmptyValue(const double& val);

	static bool			convertValueToIntForImport(		const std::string &strValue, int &intValue);
	static bool			convertValueToDoubleForImport(	const std::string &strValue, double &doubleValue);
	static void			convertEscapedUnicodeToUTF8(	std::string &inputStr);

	static bool convertVecToInt(const std::vector<std::string> &values, std::vector<int> &intValues, std::set<int> &uniqueValues, std::map<int, std::string> &emptyValuesMap);
	static bool convertVecToDouble(const std::vector<std::string> &values, std::vector<double> &doubleValues, std::map<int, std::string> &emptyValuesMap);

	static std::string	doubleToString(double dbl, int precision = 10);

private:
	static std::string _deEuropeaniseForImport(			const std::string &value);
	static std::string _convertEscapedUnicodeToUTF8(	std::string hex);

	static std::vector<std::string>			_currentEmptyValues;
	static const std::vector<std::string>	_defaultEmptyValues;
	static std::vector<double>				_currentDoubleEmptyValues;
};

#endif // COLUMNUTILS_H
