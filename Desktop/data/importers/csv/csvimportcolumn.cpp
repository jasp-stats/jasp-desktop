#include "csvimportcolumn.h"
#include "columnutils.h"
#include "timers.h"

CSVImportColumn::CSVImportColumn(ImportDataSet* importDataSet, std::string name) : ImportColumn(importDataSet, name)
{
}

CSVImportColumn::CSVImportColumn(ImportDataSet *importDataSet, std::string name, long reserve, ColumnUtils::toDoubleF readNumbersAs) : ImportColumn(importDataSet, name), _readNumbersAs(readNumbersAs)
{
	_data.reserve(reserve);
}

CSVImportColumn::~CSVImportColumn()
{
	JASPTIMER_SCOPE(CSVImportColumn::~CSVImportColumn());
	_data.clear();
}

size_t CSVImportColumn::size() const
{
	return _data.size();
}

std::string CSVImportColumn::valueLookup(size_t row) const
{
	if(_data.size() <= row)
		return "";

	//A number written in the locale of this file is handed on written in the locale of the interface,
	//because that is how the column, its labels and a later sync read it again.
	double number;
	if(_readNumbersAs && _readNumbersAs(_data[row], number))
		return ColumnUtils::doubleToString(number);

	return ColumnUtils::doubleToLocale(_data[row]);
}

void CSVImportColumn::addValue(const std::string &value)
{
	_data.push_back(value);
}

const std::vector<std::string> &CSVImportColumn::getValues() const
{
	return _data;
}
