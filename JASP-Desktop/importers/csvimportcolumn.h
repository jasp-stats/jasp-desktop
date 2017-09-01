#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "importcolumn.h"

class CSVImportColumn : public ImportColumn
{
public:
	CSVImportColumn(ImportDataSet* importDataSet, std::string name);
	virtual ~CSVImportColumn();

	virtual size_t size() const;
	virtual bool isValueEqual(Column &col, size_t row) const;

	void addValue(const std::string &value);
	const std::vector<std::string>& getValues() const;

private:
	std::vector<std::string> _data;

};

#endif // CSVIMPORTCOLUMN_H
