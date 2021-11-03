#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>
#include "column.h"

class ImportDataSet;


///
/// Base class for all columns during import
/// It has some utility functions and defines the interface that is used to convert all this to the "real" dataset in memory in JASP
class ImportColumn
{
public:
										ImportColumn(ImportDataSet* importDataSet, std::string name);
	virtual								~ImportColumn();

	virtual size_t						size()									const = 0;
	virtual std::vector<std::string>	allValuesAsStrings()					const = 0;
			std::string					name()									const;
			void						changeName(const std::string & name);

	static bool convertVecToInt(	const std::vector<std::string> & values, std::vector<int>		& intValues,	std::set<int> &uniqueValues,	std::map<int, std::string> &emptyValuesMap);
	static bool convertVecToDouble(	const std::vector<std::string> & values, std::vector<double>	& doubleValues,									std::map<int, std::string> &emptyValuesMap);

	static bool isStringValueEqual(const std::string &value, Column &col, size_t row);

protected:
	ImportDataSet * _importDataSet;
	std::string		_name;
};

#endif // IMPORTCOLUMN_H
