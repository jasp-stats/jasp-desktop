#ifndef OPTIONFIELDS_H
#define OPTIONFIELDS_H

#include "optioni.h"
#include "common.h"

#include <QStringList>

class OptionFields : public OptionI<std::vector<std::string> >
{
public:
	OptionFields();

	virtual Json::Value asJSON()const OVERRIDE;
	virtual void set(Json::Value& value) OVERRIDE;
	virtual Option* clone() const OVERRIDE;

};

#endif // OPTIONFIELDS_H
