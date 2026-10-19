#include "qutils.h"
#include "importdataset.h"
#include "timers.h"
#include "appinfo.h"
#include "importer.h"

using namespace std;

ImportDataSet::ImportDataSet(Importer *importer) : QObject(importer), _importer(importer)
{
}

ImportDataSet::~ImportDataSet()
{
}

void ImportDataSet::addColumn(ImportColumn *column)
{
	_columns.push_back(column);
}

size_t ImportDataSet::columnCount() const
{
	return _columns.size();
}

const string & ImportDataSet::description() const
{
	static std::string localCache;
	QString desc = tr("Originally imported into %1 on %2").arg(tq(AppInfo::getShortDesc())).arg(tq(Utils::currentDateTime()));
	localCache = fq(desc);
	
	return localCache;
}

size_t ImportDataSet::rowCount() const
{
	if(_rowsCountedByBuildDictionary != -1)
		return _rowsCountedByBuildDictionary;

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


	//Lets name the unnamed columns the same way csv does
	_rowsCountedByBuildDictionary = -1;

	for(size_t curCol = 0; curCol < _columns.size(); curCol++)
	{
		ImportColumn * col = _columns[curCol];
		
		if(!col->containsAnythingAtAll()) //Ignore very empty columns
		{
			_columns.erase(_columns.begin() + curCol);
			curCol--;
			delete col;
		}
		else if(col->name() == "")
		{
			std::string newName;
			do
				newName = "V" + std::to_string(curCol + 1);
			while(_nameToColMap.count(newName) > 0);
				
			col->setName(newName);

			_nameToColMap[col->name()] = col;

			_rowsCountedByBuildDictionary = std::max(int(col->size()), _rowsCountedByBuildDictionary);
		}
	}
}

