#ifndef OPTIONFIELDS_H
#define OPTIONFIELDS_H

#include "optioni.h"

#include <QStringList>

class OptionFields : public OptionI<std::vector<std::string> >
{
public:
	OptionFields(std::string name);

    Json::Value asJSON()const;// override;

};

#endif // OPTIONFIELDS_H
