#ifndef OPTIONBOOLEAN_H
#define OPTIONBOOLEAN_H

#include "optioni.h"

class OptionBoolean : public OptionI<bool>
{
public:
	OptionBoolean(std::string name);

	virtual Json::Value asJSON() const;// override;
    virtual void set(Json::Value& value);// override;
};

#endif // OPTIONBOOLEAN_H
