#ifndef OPTIONINTEGERARRAY_H
#define OPTIONINTEGERARRAY_H

#include "optioni.h"
#include "common.h"

class OptionIntegerArray : public OptionI<std::vector<int> >
{
public:
	OptionIntegerArray();

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;
};

#endif // OPTIONINTEGERARRAY_H
