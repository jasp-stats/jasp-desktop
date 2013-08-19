#ifndef RINTERFACE_H
#define RINTERFACE_H

#include "../JASP-Common/dataset.h"
#include "../JASP-Common/options.h"

class RInterface
{
public:
	RInterface() { }

	virtual void setDataSet(DataSet *dataSet) = 0;
	virtual Json::Value init(const int id, const string &name, const Json::Value &options) = 0;
	virtual Json::Value run(const int id, const Json::Value &options) = 0;
};

#endif // RINTERFACE_H
