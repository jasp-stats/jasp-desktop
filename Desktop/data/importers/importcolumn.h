#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>
#include "column.h"

class ImportDataSet;

class ImportColumn
{
public:
										ImportColumn(ImportDataSet* importDataSet, std::string name);
	virtual								~ImportColumn();

	virtual size_t						size()									const = 0;
	virtual std::vector<std::string>	allValuesAsStrings()					const = 0;
			std::string					name()									const;
			void						changeName(const std::string & name);

	static bool isStringValueEqual(const std::string &value, Column &col, size_t row);

protected:
	ImportDataSet * _importDataSet;
	std::string		_name;
};

#endif // IMPORTCOLUMN_H
