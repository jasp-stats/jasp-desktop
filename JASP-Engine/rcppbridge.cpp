#include "rcppbridge.h"

#include <boost/foreach.hpp>

using namespace std;


RcppBridge::RcppBridge()
{
	_rInside["jasp.analyses"] = Rcpp::List();
	_rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"RJSONIO\"))");
	_rInside.parseEvalQNT("suppressPackageStartupMessages(library(\"JASP\"))");
}

void RcppBridge::setDataSet(DataSet* dataSet)
{
	Rcpp::List list(dataSet->columnCount());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	BOOST_FOREACH(Column &column, dataSet->columns())
	{
		columnNames.push_back(column.name());

		if (column.dataType() == Column::DataTypeInt)
		{
			Rcpp::IntegerVector v(column.rowCount());

			int r = 0;
			BOOST_FOREACH(int value, column.AsInts)
				v[r++] = value;

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
	_rInside["dataset"] = Rcpp::DataFrame(list);
}

Json::Value RcppBridge::init(const int id, const string &name, const Json::Value &options)
{
	stringstream ss;
	ss << "if (\"";
	ss << id;
	ss << "\" %in% names(jasp.analyses) == FALSE) analysis.env <- (jasp.analyses[[\"";
	ss << id;
	ss << "\"]] <- new.env()) else analysis.env <- jasp.analyses[[\"";
	ss << id;
	ss << "\"]]; analysis.env";

	Rcpp::Environment env = _rInside.parseEval(ss.str());

	ss.str("");

	ss << "evalq(analysis.func <- JASP::";
	ss << name;
	ss << ", analysis.env)";

	_rInside.parseEvalQNT(ss.str());

	SEXP results;

	env["optionsAsString"] = options.toStyledString();

	_rInside.parseEvalQNT("evalq(options <- fromJSON(optionsAsString, asText=TRUE), analysis.env)");
	_rInside.parseEval("evalq(results <- analysis.func(dataset, options, \"init\"), analysis.env)");
	_rInside.parseEval("evalq(toJSON(results), analysis.env)", results);

	string resultsAsString = Rcpp::as<string>(results);

	Json::Reader r;
	Json::Value json;
	r.parse(resultsAsString, json);

	return json;
}

Json::Value RcppBridge::run(const int id, const Json::Value &options)
{
	stringstream ss;

	ss << "if (\"";
	ss << id;
	ss << "\" %in% names(jasp.analyses) == FALSE) analysis.env <- (jasp.analyses[[\"";
	ss << id;
	ss << "\"]] <- new.env()) else analysis.env <- jasp.analyses[[\"";
	ss << id;
	ss << "\"]]; analysis.env";

	Rcpp::Environment env = _rInside.parseEval(ss.str());

	SEXP results;

	env["optionsAsString"] = options.toStyledString();

	_rInside.parseEvalQNT("evalq(options <- fromJSON(optionsAsString, asText=TRUE), analysis.env)");
	_rInside.parseEval("evalq(results <- analysis.func(dataset, options, \"run\"), analysis.env)");
	_rInside.parseEval("evalq(toJSON(results), analysis.env)", results);

	string resultsAsString = Rcpp::as<string>(results);

	Json::Reader r;
	Json::Value json;
	r.parse(resultsAsString, json);

	return json;
}
