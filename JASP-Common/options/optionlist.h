#ifndef OPTIONLIST_H
#define OPTIONLIST_H

#include "optioni.h"

class OptionList : public OptionI<std::string>
{
public:
	OptionList(std::string name, std::string selected);
    virtual Json::Value asJSON() const;// override;
    virtual void set(Json::Value& value);// override;

private:
	std::string _selected;
};

#endif // OPTIONLIST_H
