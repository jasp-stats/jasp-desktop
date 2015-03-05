#ifndef OPTIONLIST_H
#define OPTIONLIST_H

#include "optioni.h"
#include "common.h"

class OptionList : public OptionI<std::string>
{
public:
	OptionList(const std::vector<std::string> &options, std::string selected = "");
	OptionList();

	virtual void loadData(const Json::Value &data) OVERRIDE;

	virtual Json::Value asJSON() const OVERRIDE;
	virtual void set(const Json::Value& value) OVERRIDE;
	void set(int index);
	const std::vector<std::string> options() const;
	virtual Option* clone() const OVERRIDE;

private:
	std::vector<std::string> _options;
	std::string _selected;
};

#endif // OPTIONLIST_H
