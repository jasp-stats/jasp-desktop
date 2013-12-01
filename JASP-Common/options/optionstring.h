#ifndef OPTIONSTRING_H
#define OPTIONSTRING_H

#include "options/optioni.h"
#include "common.h"

class OptionString : public OptionI<std::string>
{
public:
	OptionString(std::string name);

	Json::Value asJSON() const OVERRIDE;
	void set(Json::Value &value) OVERRIDE;
};

#endif // OPTIONSTRING_H
