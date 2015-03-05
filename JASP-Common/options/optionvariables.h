#ifndef OPTIONVARIABLES_H
#define OPTIONVARIABLES_H

#include "optionterms.h"

class OptionVariables : public OptionTerms
{
public:
	OptionVariables();

	virtual Json::Value asJSON()const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	std::vector<std::string> variables() const;

protected:
	OptionVariables(bool onlyOneTerm);

};

#endif // OPTIONVARIABLES_H
