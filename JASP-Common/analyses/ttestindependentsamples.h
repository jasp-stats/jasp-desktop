#ifndef TTESTINDEPENDENTSAMPLES_H
#define TTESTINDEPENDENTSAMPLES_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class TTestIndependentSamples : public Analysis
{
public:
	TTestIndependentSamples(int id);

protected:

    virtual Options *createDefaultOptions();// override;
	virtual std::string rScript(); // override

private:

	static std::string _script;

};

}

#endif // TTESTINDEPENDENTSAMPLES_H
