#include "excelimportcolumn.h"
#include "timers.h"

ExcelImportColumn::ExcelImportColumn(ImportDataSet* importDataSet, std::string name) : ImportColumn(importDataSet, name)
{
}

ExcelImportColumn::ExcelImportColumn(ImportDataSet *importDataSet, std::string name, long reserve) : ImportColumn(importDataSet, name)
{
	_data.reserve(reserve);
}

ExcelImportColumn::~ExcelImportColumn()
{
	JASPTIMER_SCOPE(ExcelImportColumn::~ExcelImportColumn());
	_data.clear();
}

size_t ExcelImportColumn::size() const
{
	return _data.size();
}

void ExcelImportColumn::addValue(const std::string &value)
{
	_data.push_back(value);
}

const std::vector<std::string> &ExcelImportColumn::getValues() const
{
	return _data;
}
