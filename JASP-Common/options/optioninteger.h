#ifndef OPTIONINTEGER_H
#define OPTIONINTEGER_H

#include "optioni.h"
#include "common.h"

class OptionInteger : public OptionI<int>
{
public:
	OptionInteger(int value = 0);

	virtual void loadData(const Json::Value &data) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;
};

#endif // OPTIONINTEGER_H
