#ifndef RCPPBRIDGE_H
#define RCPPBRIDGE_H

#include <RInside.h>
#include <Rcpp.h>

#include "../JASP-Common/common.h"
#include "../JASP-Common/rinterface.h"

#include <boost/function.hpp>

class RcppBridge : public RInterface
{
public:
	RcppBridge();

	virtual void setDataSet(DataSet *dataSet) OVERRIDE;
	virtual Json::Value init(const std::string &name, const Json::Value &options) OVERRIDE;
	virtual Json::Value run(const std::string &name, const Json::Value &options, boost::function<int (Json::Value)> callback) OVERRIDE;

private:
	RInside _rInside;
	DataSet *_dataSet;

	static RcppBridge* _staticRef;
	static Rcpp::DataFrame readDataSetStatic();
	static Rcpp::DataFrame readDataSetHeaderStatic();
	static SEXP callbackStatic(SEXP results);

	int callback(SEXP results);
	Rcpp::DataFrame readDataSet();
	Rcpp::DataFrame readDataSetHeader();

	boost::function<int (Json::Value)> _callback;
};

#endif // RCPPBRIDGE_H
