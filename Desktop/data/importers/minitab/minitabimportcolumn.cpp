#include "minitabimportcolumn.h"
#include "timers.h"
#include "columnutils.h"


MwxImportColumn::MwxImportColumn(ImportDataSet* importDataSet, const std::string & name, const stringvec & levels, columnType type)
: ImportColumn(static_cast<ImportDataSet*>(importDataSet), name), _levels(levels), _type(type)
{
}

MwxImportColumn::~MwxImportColumn()
{
}

size_t MwxImportColumn::size() const
{
	return _data.size();
}

std::string MwxImportColumn::valueLookup(size_t row) const

{
	return row < _data.size() ? _data[row] : "";
}

std::string MwxImportColumn::labelLookup(size_t row) const
{
	if (_levels.empty() || row >= _data.size())
		return "";

	int levelIndex;
	
	if (ColumnUtils::getIntValue(_data[row], levelIndex))
	{
		if (levelIndex > 0 && levelIndex <= static_cast<int>(_levels.size()))
			return _levels[levelIndex - 1];
	}

	return "";
}

void MwxImportColumn::addValue(const std::string &value, int index)
{
	if(index == -1 || index == _data.size())
		_data.push_back(value);
	else
	{
		if(_data.size() <= index)
			_data.resize(index+1);

		_data[index] = value;	
	}
}

void MwxImportColumn::reserve(size_t size)
{
	_data.reserve(size);
}
