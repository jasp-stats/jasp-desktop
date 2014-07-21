#ifndef DESCRIPTIVES_H
#define DESCRIPTIVES_H

#include "analysis.h"

class Descriptives : public Analysis
{
public:
	Descriptives(int id);

protected:

	Options *createOptions() const;

};

#endif // DESCRIPTIVES_H

