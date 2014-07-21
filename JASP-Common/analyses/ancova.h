#ifndef ANCOVA_H
#define ANCOVA_H

#include "analysis.h"

class Ancova : public Analysis
{
public:
	Ancova(int id);

protected:

	Options *createOptions() const;

};

#endif // ANCOVA_H
