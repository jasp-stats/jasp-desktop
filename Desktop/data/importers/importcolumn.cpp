#include "importcolumn.h"
#include "log.h"

ImportColumn::ImportColumn(ImportDataSet* importDataSet,  const std::string & name,  const std::string & title)
	: _importDataSet(importDataSet), _name(stringUtils::trimAndRemoveEscapes(name)), _title(stringUtils::trimAndRemoveEscapes(title))
{
}

ImportColumn::~ImportColumn()
{
	JASPTIMER_SCOPE(ImportColumn::~ImportColumn());
}


const std::string & ImportColumn::name() const
{
	return _name;
}



const std::string & ImportColumn::title() const
{
	return _title;
}

void ImportColumn::setName(const std::string & name)
{
	if(!_name.empty())
		Log::log() << "Changing name of column from '" << _name << "' to '" << name << "'\n." << std::endl;

	_name = stringUtils::trimAndRemoveEscapes(name);
}

void ImportColumn::setTitle(const std::string & title)
{
	_title = stringUtils::trimAndRemoveEscapes(title);
}
