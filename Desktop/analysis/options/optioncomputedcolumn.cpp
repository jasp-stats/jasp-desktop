#include "optioncomputedcolumn.h"
#include "data/computedcolumn.h"

void OptionComputedColumn::init(const Json::Value &data)
{
	set(data.get("default", "").asString());
}

Json::Value OptionComputedColumn::asJSON() const
{
	return Json::Value(value());
}

Json::Value OptionComputedColumn::asMetaJSON() const
{
	return defaultMetaEntryContainingColumn();
}

void OptionComputedColumn::setValue(const std::string &newValue)
{
	std::string columnName = newValue;

	if(_value != "")
	{
		if(_value != columnName)	requestComputedColumnDestruction(_value);
		else						return;
	}

	OptionString::setValue(columnName);

	if(columnName != "")
	{
		if(!_computed)
			requestColumnCreation(columnName, _columnType);
		else if(requestComputedColumnCreation(columnName) == NULL)
			OptionString::setValue("");
	}

}

void OptionComputedColumn::clear()
{
	setValue("");
}

Option *OptionComputedColumn::clone() const
{
	return new OptionComputedColumn(_value, _computed, _columnType);
}
