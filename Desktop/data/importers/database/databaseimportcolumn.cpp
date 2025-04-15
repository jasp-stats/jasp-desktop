#include "databaseimportcolumn.h"
#include "utilities/qutils.h"

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
	return fq(_data[row].toString());
}

void DatabaseImportColumn::addValue(const QVariant & value)
{
	_data.push_back(value);
}

const std::vector<QVariant> &DatabaseImportColumn::getValues() const
{
	return _data;
}
