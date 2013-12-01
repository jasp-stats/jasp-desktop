#ifndef OPTIONINTEGER_H
#define OPTIONINTEGER_H

#include "optioni.h"
#include "common.h"

class OptionInteger : public OptionI<int>
{
public:
	OptionInteger(std::string name, int value = 0);

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
};

#endif // OPTIONINTEGER_H
