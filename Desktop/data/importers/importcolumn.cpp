#include "importcolumn.h"
#include <cmath>
#include "utils.h"
#include "columnutils.h"
#include "log.h"

ImportColumn::ImportColumn(ImportDataSet* importDataSet, std::string name)
	: _importDataSet(importDataSet), _name(name)
{
}

ImportColumn::~ImportColumn()
{
}


std::string ImportColumn::name() const
{
	return _name;
}

void ImportColumn::changeName(const std::string & name)
{
	Log::log() << "Changing name of column from '" << _name << "' to '" << name << "'\n." << std::endl;

	_name = name;
}
