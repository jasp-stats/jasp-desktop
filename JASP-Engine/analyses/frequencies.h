#ifndef FREQUENCIES_H
#define FREQUENCIES_H

#include "analysistask.h"

#include <string>
#include <vector>

#include "../JASP-Common/lib_json/json.h"
#include "rinterface.h"

class Frequencies : public AnalysisTask
{
public:
	Frequencies(DataSet *dataSet, Json::Value *options, RInterface *r);
    virtual void init();// override;
    virtual void run();// override;

private:

	Json::Value initTables();
	Json::Value initStats();
	Json::Value initPlots();

	vector<string> getMainFields();
};

#endif // FREQUENCIES_H
