#ifndef ONESAMPLETTEST_H
#define ONESAMPLETTEST_H

#include "analysistask.h"

class TTestOneSample : public AnalysisTask
{
public:
	TTestOneSample(DataSet *dataSet, Json::Value *options, RInterface *r);
    virtual void init();// override;
    virtual void run();// override;

private:

	vector<string> getMainFields();
};

#endif // ONESAMPLETTEST_H
