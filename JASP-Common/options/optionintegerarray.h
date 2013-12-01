#ifndef OPTIONINTEGERARRAY_H
#define OPTIONINTEGERARRAY_H

#include "optioni.h"
#include "common.h"

class OptionIntegerArray : public OptionI<std::vector<int> >
{
public:
	OptionIntegerArray(std::string name);

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
};

#endif // OPTIONINTEGERARRAY_H
