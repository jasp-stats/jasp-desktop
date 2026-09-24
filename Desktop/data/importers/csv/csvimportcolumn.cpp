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

	double number;
	return _readNumber(_data[row], number) ? QLocale::c().toString(number, 'g', 10).toStdString() : _data[row]; //As many digits as doubleToLocale keeps
}

std::string CSVImportColumn::valueLookupAsShown(size_t row) const
{
	if(_data.size() <= row || !_readNumbersAs)
		return valueLookup(row);

	double number;
	return _readNumber(_data[row], number) ? ColumnUtils::doubleToString(number) : _data[row];
}

bool CSVImportColumn::_readNumber(const std::string & text, double & number) const
{
	//Infinity and NaN go on as the text they were, the column reads those itself
	return QColumnUtils::readNumber(text, number, _readNumbersAs) && std::isfinite(number);
}

void CSVImportColumn::addValue(const std::string &value)
{
	_data.push_back(value);
}

const std::vector<std::string> &CSVImportColumn::getValues() const
{
	return _data;
}
