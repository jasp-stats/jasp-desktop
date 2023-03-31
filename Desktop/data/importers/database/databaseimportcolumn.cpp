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

std::vector<std::string> DatabaseImportColumn::allValuesAsStrings() const 
{ 
	stringvec strs;
	strs.reserve(_data.size());
	
	for(const QVariant & v : _data)
		strs.push_back(fq(v.toString()));
	
	return  strs;
}

void DatabaseImportColumn::addValue(const QVariant & value)
{
	_data.push_back(value);
}

const std::vector<QVariant> &DatabaseImportColumn::getValues() const
{
	return _data;
}
