#include "analysistask.h"

using namespace Json;

AnalysisTask::AnalysisTask(DataSet *dataSet, Value *options, RInterface *r)
{
	_dataSet = dataSet;
	_options = options;
	_r = r;
}

Value &AnalysisTask::results()
{
	return _results;
}
