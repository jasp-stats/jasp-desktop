#ifndef OPTIONFIELD_H
#define OPTIONFIELD_H

#include <string>

#include "lib_json/json.h"

#include "optionfields.h"
#include "common.h"

class OptionField : public OptionFields
{
public:
	OptionField();
	virtual void set(Json::Value& value) OVERRIDE;
	virtual void setValue(std::vector<std::string> value) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	virtual void setValue(std::string value);
};

#endif // OPTIONFIELD_H
