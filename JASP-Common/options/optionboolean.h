#ifndef OPTIONBOOLEAN_H
#define OPTIONBOOLEAN_H

#include "optioni.h"
#include "common.h"

class OptionBoolean : public OptionI<bool>
{
public:
	OptionBoolean(std::string name, bool defaultValue = false);

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
};

#endif // OPTIONBOOLEAN_H
