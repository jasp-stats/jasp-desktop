#include "rcppbridge.h"

#include <boost/foreach.hpp>
#include "../JASP-Common/base64.h"

using namespace std;


RcppBridge* RcppBridge::_staticRef;

RcppBridge::RcppBridge()
{
	_staticRef = this;

	_rInside[".read.dataset.native"] = Rcpp::InternalFunction(&RcppBridge::readDataSetStatic);
	_rInside[".read.dataset.header.native"] = Rcpp::InternalFunction(&RcppBridge::readDataSetHeaderStatic);
	_rInside[".callback.native"] = Rcpp::InternalFunction(&RcppBridge::callbackStatic);

	_rInside["jasp.analyses"] = Rcpp::List();
	_rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"RJSONIO\"))");
	_rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"JASP\"))");
}

void RcppBridge::setDataSet(DataSet* dataSet)
{
	_dataSet = dataSet;
}

Json::Value RcppBridge::init(const string &name, const Json::Value &options)
{
	SEXP results;

	_rInside["name"] = name;
	_rInside["options.as.json.string"] = options.toStyledString();
	_rInside.parseEval("init(name=name, options.as.json.string=options.as.json.string)", results);

	string resultsAsString = Rcpp::as<string>(results);

	Json::Reader r;
	Json::Value json;
	r.parse(resultsAsString, json);

	return json;
}

Json::Value RcppBridge::run(const string &name, const Json::Value &options, boost::function<int (Json::Value)> callback)
{
	SEXP results;

	_callback = callback;

	_rInside["name"] = name;
	_rInside["options.as.json.string"] = options.toStyledString();
	_rInside.parseEval("run(name=name, options.as.json.string=options.as.json.string)", results);

	_callback = NULL;

	string resultsAsString = Rcpp::as<string>(results);

	Json::Reader r;
	Json::Value json;
	r.parse(resultsAsString, json);

	return json;
}

Rcpp::DataFrame RcppBridge::readDataSet()
{
	Rcpp::List list(_dataSet->columnCount());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	BOOST_FOREACH(Column &column, _dataSet->columns())
	{
		string columnName = column.name();
		string dot = ".";

		string base64 = Base64::encode(dot, columnName);
		columnNames.push_back(base64);

		if (column.dataType() == Column::DataTypeInt)
		{
			Rcpp::IntegerVector v(column.rowCount());

			if (column.hasLabels())
			{
				int r = 0;
				BOOST_FOREACH(int value, column.AsInts)
					v[r++] = value + 1;

				Rcpp::CharacterVector labels;
				typedef pair<int, string> pair;

				BOOST_FOREACH(pair label, column.labels())
					labels.push_back(label.second);

				v.attr("levels") = labels;
				v.attr("class") = "factor";
			}
			else
			{
				int r = 0;
				BOOST_FOREACH(int value, column.AsInts)
					v[r++] = value;
			}

			list[colNo++] = v;
		}
		else
		{
			Rcpp::NumericVector v(column.rowCount());

			int r = 0;
			BOOST_FOREACH(double value, column.AsDoubles)
			{
				(void)column;
				v[r++] = value;
			}

			list[colNo++] = v;
		}
	}

	list.attr("names") = columnNames;

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame(list);

	/*if (Rf_isNull(excludeNAsListwise))
	{
		wholeDataFrame = dataFrame;
		wholeDataFrameLoaded = true;
	}*/

	return dataFrame;
}

Rcpp::DataFrame RcppBridge::readDataSetHeader()
{
	Rcpp::List list(_dataSet->columnCount());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	BOOST_FOREACH(Column &column, _dataSet->columns())
	{
		string columnName = column.name();
		string dot = ".";

		string base64 = Base64::encode(dot, columnName);
		columnNames.push_back(base64);

		if (column.dataType() == Column::DataTypeInt)
		{
			Rcpp::IntegerVector v(0);

			if (column.hasLabels())
			{
				Rcpp::CharacterVector labels;
				typedef pair<int, string> pair;

				BOOST_FOREACH(pair label, column.labels())
					labels.push_back(label.second);

				v.attr("levels") = labels;
				v.attr("class") = "factor";
			}

			list[colNo++] = v;
		}
		else
		{
			Rcpp::NumericVector v(0);
			list[colNo++] = v;
		}
	}

	list.attr("names") = columnNames;

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame(list);

	return dataFrame;
}

int RcppBridge::callback(SEXP results)
{
	string jsonString = Rcpp::as<string>(results);

	Json::Reader r;
	Json::Value json;
	r.parse(jsonString, json);

	return _callback(json);
}

Rcpp::DataFrame RcppBridge::readDataSetStatic()
{
	return _staticRef->readDataSet();
}

Rcpp::DataFrame RcppBridge::readDataSetHeaderStatic()
{
	return _staticRef->readDataSetHeader();
}

SEXP RcppBridge::callbackStatic(SEXP results)
{
	Rcpp::NumericVector control(1);
	control[0] = _staticRef->callback(results);
	return control;
}


