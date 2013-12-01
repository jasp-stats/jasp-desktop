#ifndef OPTIONLIST_H
#define OPTIONLIST_H

#include "optioni.h"
#include "common.h"

class OptionList : public OptionI<std::string>
{
public:
	OptionList(std::string name, const std::vector<std::string> &options, std::string selected = "");
	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
	void set(int index);
	const std::vector<std::string> options() const;

private:
	std::vector<std::string> _options;
	std::string _selected;
};

#endif // OPTIONLIST_H
