#ifndef RINTERFACE_H
#define RINTERFACE_H

#include "../JASP-Common/dataset.h"
#include "../JASP-Common/options/options.h"
#include "boost/signals2.hpp"

class RInterface
{
public:
	RInterface() { }

	virtual void setDataSet(DataSet *dataSet) = 0;
	virtual Json::Value init(const std::string &name, const Json::Value &options) = 0;
	virtual Json::Value run(const std::string &name, const Json::Value &options, boost::function<int (Json::Value)> callback) = 0;

};

#endif // RINTERFACE_H
