#ifndef ANOVABAYESIAN_H
#define ANOVABAYESIAN_H

#include "analysis.h"

class AnovaBayesian : public Analysis
{
public:
	AnovaBayesian(int id);

protected:

	Options *createOptions() const;

};

#endif // ANOVABAYESIAN_H
