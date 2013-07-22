#ifndef ONESAMPLETTEST_H
#define ONESAMPLETTEST_H

#include "../JASP-Common/analysis.h"

namespace analyses
{

class TTestOneSample : public Analysis
{
public:
	TTestOneSample(int id);
    virtual void init();// override;
    virtual void run();// override;

protected:

    virtual Options *createDefaultOptions();// override;

private:

	static std::string _script;
};

}

#endif // ONESAMPLETTEST_H
