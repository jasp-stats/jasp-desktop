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
										ImportColumn(ImportDataSet* importDataSet, const std::string & name,  const std::string & title = "");
	virtual								~ImportColumn();

	virtual			size_t				size()									const = 0;
	virtual const	stringvec		&	allValuesAsStrings()					const = 0;
	virtual const	stringvec		&	allLabelsAsStrings()					const	{ return allValuesAsStrings(); };
	virtual const	stringset		&	allEmptyValuesAsStrings()				const	{ static stringset a; return a; }
	virtual			columnType			getColumnType()							const	{ return columnType::unknown; }
			const	std::string		&	title()									const;
			const	std::string		&	name()									const;
			void						setName(const std::string & name);
			void						setTitle(const std::string & title);
					
protected:
	ImportDataSet * _importDataSet;
	std::string		_name,
					_title;
};

#endif // IMPORTCOLUMN_H
