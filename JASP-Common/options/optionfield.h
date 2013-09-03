#ifndef OPTIONFIELD_H
#define OPTIONFIELD_H

#include <string>

#include "lib_json/json.h"

#include "optionfields.h"

class OptionField : public OptionFields
{
public:
	OptionField(std::string name);
	virtual void set(Json::Value& value);// override;
    virtual void setValue(std::vector<std::string> value);// override;
    virtual Json::Value asJSON() const;// override;
};

#endif // OPTIONFIELD_H
