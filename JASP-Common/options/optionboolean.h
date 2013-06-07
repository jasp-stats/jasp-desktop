#ifndef OPTIONBOOLEAN_H
#define OPTIONBOOLEAN_H

#include "optioni.h"

class OptionBoolean : public OptionI<bool>
{
public:
	OptionBoolean(std::string name);

    Json::Value asJSON() const;// override;
};

#endif // OPTIONBOOLEAN_H
