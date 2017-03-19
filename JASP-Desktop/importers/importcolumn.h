#ifndef IMPORTCOLUMN_H
#define IMPORTCOLUMN_H

#include <string>
#include <map>
#include <vector>

#include "column.h"

class ImportColumn
{
public:
	ImportColumn(std::string name);
	virtual ~ImportColumn();
	virtual size_t size() const = 0;
	virtual bool isValueEqual(Column &col, size_t row) const = 0;

	virtual std::string getName() const;

protected:
	std::string _name;
};

#endif // IMPORTCOLUMN_H
