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
	OptionsTable(Options *rowTemplate);

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value &value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	Options *rowTemplate();
	Options *at(int index);
	size_t size();

	Options *insertAt(std::string name, int index);
	void insertAt(Options *row, int index);
	Options *removeAt(int index);
	Options *remove(std::string name);
	bool contains(std::string name);

private:
	Options *_template;
	std::vector<Options *> _rows;
	void rowChanged();
};

#endif // OPTIONSTABLE_H
