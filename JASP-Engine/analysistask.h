#ifndef ANALYSISTASK_H
#define ANALYSISTASK_H

#include <boost/signals2.hpp>

#include "../JASP-Common/lib_json/json.h"
#include "../JASP-Common/dataset.h"

#include "rinterface.h"

class AnalysisTask
{
public:
	AnalysisTask(DataSet *dataSet, Json::Value *options, RInterface *r);

	Json::Value &results();

	virtual void init() = 0;
	virtual void run() = 0;

	boost::signals2::signal<void()> onUpdate();

protected:
	DataSet *_dataSet;
	Json::Value *_options;
	Json::Value _results;
	RInterface *_r;
};

#endif // ENGINEANALYSIS_H
