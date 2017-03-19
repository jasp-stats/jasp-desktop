#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "importcolumn.h"

class CSVImportColumn : public ImportColumn
{
public:
	CSVImportColumn(std::string name);
	virtual ~CSVImportColumn();

	virtual size_t size() const;
	virtual bool isValueEqual(Column &col, size_t row) const;

	void addValue(const std::string &value);
	const std::vector<std::string>& getValues() const;
	bool convertToInt(std::vector<int> &intValues, std::set<int> &uniqueValues) const;
	bool convertToDouble(std::vector<double> &doubleValues) const;

private:
	std::vector<std::string> _data;

	std::string _deEuropeanise(const std::string &value) const;
	bool _convertValueToInt(const std::string &strValue, int &intValue) const;
	bool _convertValueToDouble(const std::string &strValue, double &doubleValue) const;

};

#endif // CSVIMPORTCOLUMN_H
