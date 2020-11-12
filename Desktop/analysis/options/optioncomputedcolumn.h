#ifndef OPTIONCOMPUTEDCOLUMN_H
#define OPTIONCOMPUTEDCOLUMN_H

#include "optionstring.h"
#include "common.h"
#include "column.h"

class ComputedColumn;

class OptionComputedColumn : public OptionString
{
public:
	OptionComputedColumn(std::string columnName = "", bool computed = true, int columnType = int(columnType::scale)) : OptionString(columnName), _computed(computed), _columnType(columnType) 
	{
		setContainsColumn(true);
	}

	void					init(const Json::Value &data)			override;
	void					setValue(const std::string &value)		override;
	void					set(const Json::Value& value)			override	{  _value = value.asString(); }
	void					clear()									override;
	Json::Value				asJSON()						const	override;
	Option*					clone()							const	override;
	std::set<std::string>	columnsCreated()						override	{ return _value != "" && _computed ?  std::set<std::string>({_value}) : std::set<std::string>(); }

private:
	bool _computed;
	int _columnType;
};

#endif // OPTIONCOMPUTEDCOLUMN_H
