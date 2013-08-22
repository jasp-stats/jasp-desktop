#ifndef RCPPBRIDGE_H
#define RCPPBRIDGE_H

#include <RInside.h>
#include <Rcpp.h>

#include "../JASP-Common/rinterface.h"

class RcppBridge : public RInterface
{
public:
	RcppBridge();

	virtual void setDataSet(DataSet *dataSet); //override;
	virtual Json::Value init(const int id, const std::string &name, const Json::Value &options); // override
	virtual Json::Value run(const int id, const Json::Value &options); // override

private:
	RInside _rInside;

};

#endif // RCPPBRIDGE_H
