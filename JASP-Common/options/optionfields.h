#ifndef OPTIONFIELDS_H
#define OPTIONFIELDS_H

#include "optioni.h"
#include "common.h"

#include <QStringList>

class OptionFields : public OptionI<std::vector<std::string> >
{
public:
	OptionFields(std::string name);

	virtual Json::Value asJSON()const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;

	void setRestrictions(int restrictions);

};

#endif // OPTIONFIELDS_H
