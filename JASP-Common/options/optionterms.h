#ifndef OPTIONTERMS_H
#define OPTIONTERMS_H

#include "optioni.h"
#include <list>
#include "common.h"

class OptionTerms : public OptionI<std::vector<std::vector<std::string> > >
{
public:
	OptionTerms();

	virtual void set(const Json::Value& value) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	virtual void setValue(const std::vector<std::vector<std::string> > &value) OVERRIDE;
	virtual void setValue(const std::vector<std::string> &value);
	virtual void setValue(const std::string &value);

	bool onlyOneTerm() const;
	bool onlyOneComponent() const;

protected:
	OptionTerms(bool onlyOneComponent, bool onlyOneTerm = false);

	bool _onlyOneComponent;
	bool _onlyOneTerm;
};

#endif // OPTIONTERMS_H
