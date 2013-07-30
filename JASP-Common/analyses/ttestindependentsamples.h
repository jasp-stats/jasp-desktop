#ifndef TTESTINDEPENDENTSAMPLES_H
#define TTESTINDEPENDENTSAMPLES_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class TTestIndependentSamples : public Analysis
{
public:
	TTestIndependentSamples(int id);
    virtual void init();// override;
    virtual void run();// override;

protected:

    virtual Options *createDefaultOptions();// override;

private:

	static std::string _script;
};

}

#endif // TTESTINDEPENDENTSAMPLES_H
