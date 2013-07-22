#ifndef OPTIONFIELDS_H
#define OPTIONFIELDS_H

#include "optioni.h"

#include <QStringList>

class OptionFields : public OptionI<std::vector<std::string> >
{
public:
	OptionFields(std::string name);

	virtual Json::Value asJSON()const;// override;
    virtual void set(Json::Value& value);// override;

	void setRestrictions(int restrictions);

};

#endif // OPTIONFIELDS_H
