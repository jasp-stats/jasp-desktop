#ifndef OPTIONCOMPUTEDCOLUMN_H
#define OPTIONCOMPUTEDCOLUMN_H

#include "optionstring.h"
#include "common.h"

class ComputedColumn;

class OptionComputedColumn : public OptionString
{
public:
	OptionComputedColumn(std::string columnName = "") : OptionString(columnName) {}

	void					init(const Json::Value &data)			override;
	void					setValue(const std::string &value)		override;
	void					set(const Json::Value& value)			override	{  _value = value.asString(); }
	Json::Value				asJSON()						const	override;
	Option*					clone()							const	override;
	std::set<std::string>	columnsCreated()						override	{ return _value != "" ?  std::set<std::string>({_value}) : std::set<std::string>(); }
};

#endif // OPTIONCOMPUTEDCOLUMN_H
