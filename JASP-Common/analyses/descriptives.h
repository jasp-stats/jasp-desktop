#ifndef DESCRIPTIVES_H
#define DESCRIPTIVES_H

#include "analysis.h"

#include "../JASP-Common/lib_json/json.h"

namespace analyses
{

class Descriptives : public Analysis
{
public:
	Descriptives(int id);

protected:

	virtual Options *createDefaultOptions() OVERRIDE;

};

}

#endif // DESCRIPTIVES_H

