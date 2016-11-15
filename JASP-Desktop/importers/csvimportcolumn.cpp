#include "csvimportcolumn.h"

CSVImportColumn::CSVImportColumn(string name) : ImportColumn(name)
{
}

CSVImportColumn::~CSVImportColumn()
{
}

int CSVImportColumn::size() const
{
	return data.size();
}

bool CSVImportColumn::isValueEqual(int row, string value)
{
	if (row < data.size())
		return data[row] == value;
	else
		return false;
}
