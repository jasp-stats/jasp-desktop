#ifndef OPTIONTERM_H
#define OPTIONTERM_H

#include "optionterms.h"

class OptionTerm : public OptionTerms
{
public:
	OptionTerm();

	virtual Json::Value asJSON()const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	std::vector<std::string> term();

};

#endif // OPTIONTERM_H
