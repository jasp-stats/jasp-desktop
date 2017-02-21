#include "importdataset.h"

using namespace std;

ImportDataSet::ImportDataSet()
{
}

ImportDataSet::~ImportDataSet()
{
	for (ImportColumns::iterator it = _columns.begin(); it != _columns.end(); ++it)
		delete *it;
}

void ImportDataSet::addColumn(ImportColumn *column)
{
	_columns.push_back(column);
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
	{
		ImportColumn* col = *(_columns.begin());
		return col->size();
	}
}

ImportColumn* ImportDataSet::getColumn(string name) const
{
	if (_nameToColMap.empty())
		throw runtime_error("Cannot call ImportDataSet::getColumn() before ImportDataSet::buildDictionary()");
	else
		return _nameToColMap.find(name)->second;
}

ImportColumns::iterator ImportDataSet::begin()
{
	return _columns.begin();
}

ImportColumns::iterator ImportDataSet::end()
{
	return _columns.end();
}

ImportColumns::reverse_iterator ImportDataSet::rbegin()
{
	return _columns.rbegin();
}

ImportColumns::reverse_iterator ImportDataSet::rend()
{
	return _columns.rend();
}

void ImportDataSet::clear()
{
	_columns.clear();
	_nameToColMap.clear();
}

void ImportDataSet::erase(ImportColumns::iterator it)
{
	_columns.erase(it);
}

/**
 * @brief buildDictionary Build the dictiontary/mapping.
 */
void ImportDataSet::buildDictionary()
{
	_nameToColMap.clear();
	for (ImportColumns::iterator colIt = begin(); colIt != end(); ++colIt)
		_nameToColMap.insert(make_pair((*colIt)->getName(), *colIt));
}

