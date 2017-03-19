#ifndef IMPORTDATASET_H
#define IMPORTDATASET_H


#include "importcolumn.h"

typedef std::vector<ImportColumn *> ImportColumns;

class ImportDataSet
{

public:
	ImportDataSet();
	virtual ~ImportDataSet();

	virtual void addColumn(ImportColumn *column);

	virtual int rowCount() const;
	virtual int columnCount() const;

	ImportColumn *getColumn(std::string name) const;
	ImportColumns::iterator begin();
	ImportColumns::iterator end();
	ImportColumns::reverse_iterator rbegin();
	ImportColumns::reverse_iterator rend();
	void clear();
	void erase(ImportColumns::iterator it);

	/**
	 * @brief buildDictionary Build the dictiontary/mapping.
	 */
	void buildDictionary();

protected:
	ImportColumns _columns;
	std::map<std::string, ImportColumn*> _nameToColMap;
};

#endif // IMPORTDATASET_H
