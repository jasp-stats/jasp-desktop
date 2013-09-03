#ifndef OPTIONSTRING_H
#define OPTIONSTRING_H

#include "options/optioni.h"
#include "string"

class OptionString : public OptionI<std::string>
{
public:
	OptionString(std::string name);

	Json::Value asJSON() const; // override
	void set(Json::Value &value); // override
};

#endif // OPTIONSTRING_H
