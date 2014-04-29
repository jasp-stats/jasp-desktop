#ifndef OPTIONSTABLE_H
#define OPTIONSTABLE_H

#include "optioni.h"
#include "options.h"

#include "common.h"
#include "option.h"
#include "options.h"

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

	void insert(int index, Options *row);
	void append(Options *row);
	Options *remove(int index);
	Options *remove(std::string name);
	bool contains(std::string name);

private:
	Options *_template;
	std::vector<Options *> _rows;
	void rowChanged();
};

#endif // OPTIONSTABLE_H
