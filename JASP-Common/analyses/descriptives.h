#ifndef DESCRIPTIVES_H
#define DESCRIPTIVES_H

#include "analysis.h"

class Descriptives : public Analysis
{
public:
	Descriptives(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;
	virtual std::string js() OVERRIDE;

};

#endif // DESCRIPTIVES_H

