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

	virtual void loadData(Json::Value data) OVERRIDE;

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value &value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;
	virtual void setValue(std::vector<Options *> value) OVERRIDE;

	Options *rowTemplate() const;
	/*Options *at(int index) const;
	size_t size() const;*/

private:
	Options *_template;
	//void rowChanged();
};

#endif // OPTIONSTABLE_H
