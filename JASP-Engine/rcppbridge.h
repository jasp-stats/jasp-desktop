#ifndef RCPPBRIDGE_H
#define RCPPBRIDGE_H

#include <RInside.h>
#include <Rcpp.h>

#include "../JASP-Common/rinterface.h"

class RcppBridge : public RInterface
{
public:
	RcppBridge();

	virtual void setDataSetHeader(DataSet *dataSet); //override;
	virtual void setDataSet(DataSet *dataSet); //override;
	virtual void setOptions(const Json::Value &options) ;//override;
	virtual Json::Value run(const string &script, const string &perform) ;//override;

private:
	RInside _rInside;
};

#endif // RCPPBRIDGE_H
