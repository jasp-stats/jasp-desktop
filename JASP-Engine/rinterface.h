#ifndef RINTERFACE_H
#define RINTERFACE_H

#include <RInside.h>
#include <Rcpp.h>

#include "../JASP-Common/dataset.h"
#include "../JASP-Common/options.h"

class RInterface
{
public:
	RInterface();

	void setDataSet(DataSet *dataSet);
	void setOptions(const Json::Value &options);
	Json::Value run(const string &script);

private:
    RInside _rInside;
};

#endif // RINTERFACE_H
