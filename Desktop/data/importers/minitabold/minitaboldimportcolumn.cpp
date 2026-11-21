#include "minitaboldimportcolumn.h"
#include "columnutils.h"
#include "emptyvalues.h"


MinitabOldImportColumn::MinitabOldImportColumn(ImportDataSet * importDataSet, const std::string & name, columnType type)
	: ImportColumn(importDataSet, name), _type(type)
{
}

MinitabOldImportColumn::~MinitabOldImportColumn()
{
}

void MinitabOldImportColumn::addValue(double value)
{
	//Exactly like the ReadStat importer stores a system missing cell: the missing
	//value as a string, and that string registered as an empty value of the column.
	std::string valStr = ColumnUtils::doubleToString(EmptyValues::missingValueDouble);

	if(!std::isnan(value))
		valStr = ColumnUtils::doubleToStringMaxPrec(value);

	_values.push_back(valStr);

	if(std::isnan(value))
		_missing.insert(valStr);
}

void MinitabOldImportColumn::addValue(const std::string & value)
{
	//A blank ("") is a missing cell in Minitab; JASP already treats "" as missing
	//(EmptyValues::isEmptyValue), so it is kept as-is and not registered separately.
	_values.push_back(value);
}
