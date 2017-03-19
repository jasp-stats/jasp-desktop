#include "importcolumn.h"

using namespace std;

ImportColumn::ImportColumn(string name)
	: _name(name)
{
}

ImportColumn::~ImportColumn()
{
}

string ImportColumn::getName() const
{
	return _name;
}
