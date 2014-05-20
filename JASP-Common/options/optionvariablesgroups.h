#ifndef OPTIONVARIABLESGROUPS_H
#define OPTIONVARIABLESGROUPS_H

#include "optioni.h"
#include "common.h"

class OptionVariablesGroups : public OptionI<std::vector<std::vector<std::string> > >
{
public:
	OptionVariablesGroups();

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;
};

#endif // OPTIONVARIABLESGROUPS_H
