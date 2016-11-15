#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "importcolumn.h"

class CSVImportColumn : public ImportColumn
{
public:
	CSVImportColumn(string name);
	virtual ~CSVImportColumn();

	virtual int size() const;
	virtual bool isValueEqual(int row, string value);

	vector<string> data;
};

#endif // CSVIMPORTCOLUMN_H
