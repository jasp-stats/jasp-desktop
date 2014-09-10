#ifndef ANCOVABAYESIAN_H
#define ANCOVABAYESIAN_H

#include "analysis.h"

class AncovaBayesian : public Analysis
{
public:
	AncovaBayesian(int id);

protected:

	Options *createOptions() const;

};

#endif // ANCOVABAYESIAN_H
