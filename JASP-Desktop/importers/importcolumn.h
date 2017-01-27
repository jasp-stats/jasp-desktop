#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>

#include "column.h"

using namespace std;

class ImportColumn
{
public:
	ImportColumn(string name);
	virtual ~ImportColumn();
	virtual size_t size() const = 0;
	virtual bool isValueEqual(Column &col, size_t row) const = 0;

	virtual string getName() const;

protected:
	string _name;
};

#endif // IMPORTCOLUMN_H
