#ifndef OPTIONFIELDPAIRS_H
#define OPTIONFIELDPAIRS_H

#include "optioni.h"
#include "common.h"

typedef std::pair<std::string, std::string> FieldPair;
typedef std::vector<FieldPair> FieldPairs;

class OptionFieldPairs : public OptionI<std::vector<std::pair<std::string, std::string> > >
{
public:
	OptionFieldPairs(std::string name);

    virtual Json::Value asJSON() const OVERRIDE;
    virtual void set(Json::Value &value) OVERRIDE;
};

#endif // OPTIONFIELDPAIRS_H
