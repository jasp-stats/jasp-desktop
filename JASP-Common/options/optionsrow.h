#ifndef OPTIONSROW_H
#define OPTIONSROW_H

#include "options.h"
#include "optionfield.h"

class OptionsRow : public Options
{
public:
	OptionsRow(std::string variable);

	std::string variable() const;
	void setVariable(std::string variable);

	virtual Option *clone() const OVERRIDE;

private:
	OptionField *_field;
};

#endif // OPTIONSROW_H
