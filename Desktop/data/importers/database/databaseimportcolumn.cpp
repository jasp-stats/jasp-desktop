#include "databaseimportcolumn.h"
#include "qutils.h"

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

const stringvec & DatabaseImportColumn::allValuesAsStrings() const 
{ 
	static stringvec strs;
	strs.resize(_data.size());
	
	for(size_t i=0; i<_data.size(); i++)
		strs[i] = fq(_data[i].toString());
	
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
