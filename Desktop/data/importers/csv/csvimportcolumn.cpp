#include "csvimportcolumn.h"
#include "columnutils.h"
#include "timers.h"
#include "utilities/qutils.h"
#include <QLocale>
#include <cmath>

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

	if(!_readNumbersAs)
		return ColumnUtils::doubleToLocale(_data[row]);

	//The numbers of this file are written in the locale chosen for it in the csv preview: read them that way (or the way C writes them)
	//but never the way the interface does, so text that locale does not take for a number stays text. They are handed on written the way
	//C writes them, with the precision doubleToLocale has, because the column reads them without the locale of the interface (valuesUseLocale).
	double number;
	if(QColumnUtils::readNumber(_data[row], number, _readNumbersAs) && std::isfinite(number))
		return QLocale::c().toString(number, 'g', 10).toStdString();

	return _data[row];
}

std::string CSVImportColumn::valueLookupAsShown(size_t row) const
{
	if(!_readNumbersAs || _data.size() <= row)
		return valueLookup(row);

	//Read like valueLookup does, but written the way the column shows the number once it is imported
	double number;
	if(QColumnUtils::readNumber(_data[row], number, _readNumbersAs) && std::isfinite(number))
		return ColumnUtils::doubleToString(number);

	return _data[row];
}

void CSVImportColumn::addValue(const std::string &value)
{
	_data.push_back(value);
}

const std::vector<std::string> &CSVImportColumn::getValues() const
{
	return _data;
}
