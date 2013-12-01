#ifndef CONTINGENCYTABLE_H
#define CONTINGENCYTABLE_H

#include "analysis.h"

class ContingencyTables : public Analysis
{
public:
	ContingencyTables(int id);

protected:
	virtual Options* createDefaultOptions() OVERRIDE;
};

#endif // CONTINGENCYTABLE_H
