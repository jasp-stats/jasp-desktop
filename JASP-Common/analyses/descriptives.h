#ifndef DESCRIPTIVES_H
#define DESCRIPTIVES_H

#include "analysis.h"

class Descriptives : public Analysis
{
public:
	Descriptives(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

#endif // DESCRIPTIVES_H

