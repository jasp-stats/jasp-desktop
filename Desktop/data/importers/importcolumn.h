#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>
#include "columntype.h"

class ImportDataSet;


///
/// Base class for all columns during import
/// It has some utility functions and defines the interface that is used to convert all this to the "real" dataset in memory in JASP
class ImportColumn
{
public:
										ImportColumn(ImportDataSet* importDataSet, std::string name);
	virtual								~ImportColumn();

	virtual			size_t				size()									const = 0;
	virtual const	stringvec		&	allValuesAsStrings()					const = 0;
					std::string			name()									const;
					void				changeName(const std::string & name);

protected:
	ImportDataSet * _importDataSet;
	std::string		_name;
};

#endif // IMPORTCOLUMN_H
