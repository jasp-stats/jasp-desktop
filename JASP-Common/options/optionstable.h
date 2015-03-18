#ifndef OPTIONSTABLE_H
#define OPTIONSTABLE_H

#include "optioni.h"
#include "options.h"

#include "common.h"
#include "option.h"
#include "options.h"

class OptionsTable : public OptionI<std::vector<Options*> >
{
public:
	OptionsTable(Options *rowTemplate);
	OptionsTable();

	virtual void init(const Json::Value &data) OVERRIDE;

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value &value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;
	virtual void setValue(const std::vector<Options *> &value) OVERRIDE;

	Options *rowTemplate() const;

private:
	Options *_template;
};

#endif // OPTIONSTABLE_H
