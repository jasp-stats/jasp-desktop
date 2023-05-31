#include "importdataset.h"
#include "timers.h"

using namespace std;

ImportDataSet::ImportDataSet(Importer *importer) : _importer(importer)
{
}

ImportDataSet::~ImportDataSet()
{
	JASPTIMER_SCOPE(ImportDataSet::~ImportDataSet());
	
	for (ImportColumn * col : _columns)
		delete col;
}

void ImportDataSet::addColumn(ImportColumn *column)
{
	_columns.push_back(column);
}

size_t ImportDataSet::columnCount() const
{
	return _columns.size();
}

size_t ImportDataSet::rowCount() const
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
	clearColumns();
	_nameToColMap.clear();
}

void ImportDataSet::erase(ImportColumns::iterator it)
{
	_columns.erase(it);
}

void ImportDataSet::buildDictionary()
{
	_nameToColMap.clear();
	for(ImportColumn * col : *this)
		if(col->name() != "")
			_nameToColMap[col->name()] = col;

	size_t unnamedColumns = 0;

	for(ImportColumn * col : *this)
		if(col->name() == "")
		{
			std::string newName = "";
			while(newName == "" || _nameToColMap.count(newName) > 0)
				newName = "Unnamed Column #" + std::to_string(++unnamedColumns);
			col->changeName(newName);

			_nameToColMap[col->name()] = col;
		}
}

