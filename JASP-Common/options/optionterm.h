#ifndef OPTIONTERM_H
#define OPTIONTERM_H

#include "optionterms.h"

class OptionTerm : public OptionTerms
{
public:
	OptionTerm();

	virtual Json::Value asJSON()const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	virtual void setValue(const std::vector<std::string> &value) OVERRIDE;

	std::vector<std::string> term() const;

};

#endif // OPTIONTERM_H
