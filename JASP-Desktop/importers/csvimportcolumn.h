#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "importcolumn.h"

class CSVImportColumn : public ImportColumn
{
public:
	CSVImportColumn(string name);
	virtual ~CSVImportColumn();

	virtual size_t size() const;
	virtual bool isValueEqual(Column &col, size_t row) const;

	void addValue(const string &value);
	const vector<string>& getValues() const;
	bool convertToInt(vector<int> &intValues, set<int> &uniqueValues) const;
	bool convertToDouble(vector<double> &doubleValues) const;

private:
	vector<string> _data;

	string _deEuropeanise(const string &value) const;
	bool _convertValueToInt(const string &strValue, int &intValue) const;
	bool _convertValueToDouble(const string &strValue, double &doubleValue) const;

};

#endif // CSVIMPORTCOLUMN_H
