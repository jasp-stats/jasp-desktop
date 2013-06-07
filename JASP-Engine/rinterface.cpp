#include "rinterface.h"

#include <boost/foreach.hpp>

using namespace std;

RInterface::RInterface()
{

}

void RInterface::setDataSet(DataSet* dataSet)
{
    Rcpp::List list(dataSet->columnCount());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	BOOST_FOREACH(Column &column, dataSet->columns())
	{
		columnNames.push_back(column.name());

		if (column.columnType() == Column::IntColumnType)
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

				v.attr("labels") = labels;
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

void RInterface::setOptions(const Json::Value &options)
{
    _rInside.parseEvalQNT("library(\"RJSONIO\")");
	_rInside["optionsAsJson"] = options.toStyledString();
	_rInside.parseEvalQNT("options <- fromJSON(optionsAsJson, asText=TRUE)");
    _rInside["results"] = NULL;
}

Json::Value RInterface::run(const string &script)
{
    SEXP results;
	SEXP resultsAsSEXPString;

	_rInside.parseEval(script, results);
	_rInside["results"] = results;
	_rInside.parseEval("toJSON(results)", resultsAsSEXPString);
    string resultsAsString = Rcpp::as<string>(resultsAsSEXPString);

    Json::Reader r;
	Json::Value json;
    r.parse(resultsAsString, json);

	return json;
}
