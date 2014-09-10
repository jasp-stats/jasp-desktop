#ifndef RCPPBRIDGE_H
#define RCPPBRIDGE_H

#include <RInside.h>
#include <Rcpp.h>

#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

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

	boost::signals2::signal<void ()> yield;

private:
	RInside _rInside;
	DataSet *_dataSet;

	static RcppBridge* _staticRef;
	static Rcpp::DataFrame readDataSetStatic(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
	static Rcpp::DataFrame readDataSetHeaderStatic(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
	static std::map<std::string, Column::ColumnType> marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns);
	static SEXP callbackStatic(SEXP results);

	int callback(SEXP results);
	Rcpp::DataFrame readDataSet(const std::map<std::string, Column::ColumnType> &columns);
	Rcpp::DataFrame readDataSetHeader(const std::map<std::string, Column::ColumnType> &columns);

	static void makeFactor(Rcpp::IntegerVector &v, const std::vector<std::string> &levels, bool ordinal = false);
	static void makeFactor(Rcpp::IntegerVector &v, const Labels &levels, bool ordinal = false);

	boost::function<int (Json::Value)> _callback;
};

#endif // RCPPBRIDGE_H
