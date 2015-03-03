#ifndef OPTIONTERMS_H
#define OPTIONTERMS_H

#include "optioni.h"
#include <list>
#include "common.h"

class OptionTerms : public OptionI<std::vector<std::vector<std::string> > >
{
public:
	OptionTerms();

	virtual void loadData(Json::Value data) OVERRIDE;

	virtual void set(Json::Value& value) OVERRIDE;
	virtual Json::Value asJSON() const OVERRIDE;
	virtual Option* clone() const OVERRIDE;

	void setValue(std::vector<std::vector<std::string> > value) OVERRIDE;
	void setValue(std::vector<std::string> value);
	void setValue(std::string value);

	bool onlyOneTerm() const;
	bool onlyOneComponent() const;

protected:
	OptionTerms(bool onlyOneComponent = false, bool onlyOneTerm = false);

	bool _onlyOneComponent;
	bool _onlyOneTerm;
};

#endif // OPTIONTERMS_H
