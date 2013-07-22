#ifndef OPTIONINTEGERARRAY_H
#define OPTIONINTEGERARRAY_H

#include "optioni.h"

class OptionIntegerArray : public OptionI<std::vector<int> >
{
public:
	OptionIntegerArray(std::string name);

	virtual Json::Value asJSON() const;// override;
    virtual void set(Json::Value& value);// override;
};

#endif // OPTIONINTEGERARRAY_H
