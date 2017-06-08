#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>

#include "column.h"

class ImportColumn
{
public:
	ImportColumn(std::string name);
	virtual ~ImportColumn();
	virtual size_t size() const = 0;
	virtual bool isValueEqual(Column &col, size_t row) const = 0;


	virtual std::string getName() const;

	static bool convertToInt(const std::vector<std::string> &values, std::vector<int> &intValues, std::set<int> &uniqueValues);
	static bool convertToDouble(const std::vector<std::string> &values, std::vector<double> &doubleValues);

	static bool convertValueToInt(const std::string &strValue, int &intValue);
	static bool convertValueToDouble(const std::string &strValue, double &doubleValue);

	static bool isStringValueEqual(const std::string &value, Column &col, size_t row);

protected:
	std::string _name;

	static std::string _deEuropeanise(const std::string &value);

};

#endif // IMPORTCOLUMN_H
