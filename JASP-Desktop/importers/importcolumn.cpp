#include "importcolumn.h"

ImportColumn::ImportColumn(string name, string longName) : _name(name), _longName(longName)
{
}

ImportColumn::~ImportColumn()
{
}

string ImportColumn::getName() const
{
	return _name;
}

string ImportColumn::getLongName() const
{
	if (!_longName.empty())
		return _longName;
	else
		return _name;
}
