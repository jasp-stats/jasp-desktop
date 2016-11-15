#include "importdataset.h"

ImportDataSet::ImportDataSet()
{
}

ImportDataSet::~ImportDataSet()
{
	for (ImportColumns::iterator it = _columns.begin(); it != _columns.end(); ++it)
		delete it->second;
}

void ImportDataSet::addColumn(ImportColumn *column)
{
	_columns.insert(make_pair(column->getName(), column));
}

int ImportDataSet::columnCount() const
{
	return _columns.size();
}

int ImportDataSet::rowCount() const
{
	if (columnCount() == 0)
		return 0;
	else
		return _columns.begin()->second->size();
}

ImportColumn* ImportDataSet::getColumn(string name) const
{
	return _columns.find(name)->second;
}

ImportColumns::iterator ImportDataSet::begin()
{
	return _columns.begin();
}

ImportColumns::iterator ImportDataSet::end()
{
	return _columns.end();
}
