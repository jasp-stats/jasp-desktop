#ifndef OPTIONINTEGER_H
#define OPTIONINTEGER_H

#include "optioni.h"

class OptionInteger : public OptionI<int>
{
public:
	OptionInteger(std::string name, int value = 0);

	virtual Json::Value asJSON() const;// override;
    virtual void set(Json::Value& value);// override;
};

#endif // OPTIONINTEGER_H
