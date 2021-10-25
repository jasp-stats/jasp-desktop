#ifndef IMPORTDATASET_H
#define IMPORTDATASET_H

#include "importcolumn.h"

class Importer;

typedef std::vector<ImportColumn *> ImportColumns;

///
/// Base class for all data during import
/// It has some utility functions and defines the interface that is used to convert all this to the "real" dataset in memory in JASP
/// It stores ImportColumn and can be used to iterate over these.
class ImportDataSet
{

public:
					ImportDataSet(Importer* importer);
	virtual			~ImportDataSet();

	virtual void	addColumn(ImportColumn *column);
	virtual size_t	rowCount()							const;
	virtual size_t	columnCount()						const;

	ImportColumn * getColumn(std::string name)			const;
	ImportColumn * getColumn(size_t ind)				const { return _columns[ind]; }

	ImportColumns::iterator			begin();
	ImportColumns::iterator			end();
	ImportColumns::reverse_iterator	rbegin();
	ImportColumns::reverse_iterator	rend();

	void clear();
	void erase(ImportColumns::iterator it);
	void buildDictionary();

protected:
	Importer								*	_importer;
	ImportColumns								_columns;
	std::map<std::string, ImportColumn*>		_nameToColMap;
};

#endif // IMPORTDATASET_H
