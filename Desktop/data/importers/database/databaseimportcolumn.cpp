#include "databaseimportcolumn.h"
#include "utilities/qutils.h"
#include "columnutils.h"

DatabaseImportColumn::DatabaseImportColumn(ImportDataSet* importDataSet, std::string name, QMetaType type) 
	: ImportColumn(importDataSet, name), _type(type)
{
}

DatabaseImportColumn::~DatabaseImportColumn()
{
}

size_t DatabaseImportColumn::size() const
{
	return _data.size();
}

const stringvec  DatabaseImportColumn::allValuesAsStrings() const 
{ 
	stringvec out;
	
	out.resize(_data.size());
	
	for(size_t i=0; i<_data.size(); i++)
		out[i] = fq(_data[i].toString());
	
	return  out;
}

std::string DatabaseImportColumn::valueLookup(size_t row) const
{
	if(_data.size() <= row)
		return "";

	const QVariant & value = _data[row];

	//A NULL is missing, but drivers hand a NULL of a numeric column over as a number that is not there, which toString() and toDouble() make 0
	if(value.isNull())
		return "";

	//A number from the database is not text written in some locale, QVariant::toString writes it the way C does
	//while the column reads it in the locale of the interface, so it is handed on written that way (like ExcelImporter does)
	if(value.typeId() == QMetaType::Double || value.typeId() == QMetaType::Float)
		return ColumnUtils::doubleToStringMaxPrec(value.toDouble(), false);

	return fq(value.toString());
}

void DatabaseImportColumn::addValue(const QVariant & value)
{
	_data.push_back(value);
}

const std::vector<QVariant> &DatabaseImportColumn::getValues() const
{
	return _data;
}
