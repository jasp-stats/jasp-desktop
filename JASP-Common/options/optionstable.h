#ifndef OPTIONSTABLE_H
#define OPTIONSTABLE_H

#include "optioni.h"
#include "options.h"

#include "common.h"
#include "option.h"
#include "options.h"

#include "optionsrow.h"

class OptionsTable : public Option
{
public:
	OptionsTable(OptionsRow *rowTemplate);

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value &value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	OptionsRow *rowTemplate();
	OptionsRow *at(int index);
	size_t size();

	OptionsRow *insertAt(std::string name, int index);
	void insertAt(OptionsRow *row, int index);
	OptionsRow *removeAt(int index);
	OptionsRow *remove(std::string name);
	bool contains(std::string name);

private:
	OptionsRow *_template;
	std::vector<OptionsRow *> _rows;
	void rowChanged();
};

#endif // OPTIONSTABLE_H
