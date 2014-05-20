#ifndef OPTIONVARIABLE_H
#define OPTIONVARIABLE_H

#include <string>

#include "lib_json/json.h"

#include "optionvariables.h"
#include "common.h"
#include "term.h"

class OptionVariable : public OptionVariables
{
public:
	OptionVariable();
	virtual void set(Json::Value& value) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	std::string variable() const;
};

#endif // OPTIONVARIABLE_H
