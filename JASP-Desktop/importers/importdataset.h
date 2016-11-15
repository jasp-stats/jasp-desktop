#ifndef IMPORTDATASET_H
#define IMPORTDATASET_H


#include "importcolumn.h"

using namespace std;

typedef map<string, ImportColumn *> ImportColumns;

class ImportDataSet
{

public:
	ImportDataSet();
	virtual ~ImportDataSet();

	void addColumn(ImportColumn *column);

	int rowCount() const;
	int columnCount() const;

	ImportColumn *getColumn(string name) const;
	ImportColumns::iterator begin();
	ImportColumns::iterator end();

private:
	ImportColumns _columns;
};

#endif // IMPORTDATASET_H
