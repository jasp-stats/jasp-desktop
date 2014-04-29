#include "rcppbridge.h"

#include <boost/foreach.hpp>
#include "../JASP-Common/base64.h"
#include <iomanip>

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

Rcpp::DataFrame RcppBridge::readDataSet(const std::map<std::string, Column::ColumnType> &columns)
{
	Rcpp::List list(columns.size());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	typedef pair<const string, Column::ColumnType> ColumnInfo;

	BOOST_FOREACH(const ColumnInfo &columnInfo, columns)
	{
		(void)columns;
		string dot = ".";

		string columnName = columnInfo.first;

		string base64 = Base64::encode(dot, columnName);
		columnNames.push_back(base64);

		Column &column = _dataSet->columns().get(columnName);
		Column::ColumnType columnType = column.columnType();

		Column::ColumnType requestedType = columnInfo.second;
		if (requestedType == Column::ColumnTypeUnknown)
			requestedType = columnType;

		int rowCount = column.rowCount();
		int rowNo = 0;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				Rcpp::NumericVector v(rowCount);

				BOOST_FOREACH(double value, column.AsDoubles)
				{
					(void)column;
					v[rowNo++] = value;
				}

				list[colNo++] = v;
			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				Rcpp::IntegerVector v(rowCount);

				BOOST_FOREACH(int value, column.AsInts)
				{
					(void)column;
					v[rowNo++] = column.actualFromRaw(value);
				}

				list[colNo++] = v;
			}
			else // columnType == Column::ColumnTypeNominalText
			{
				Rcpp::IntegerVector v(rowCount);

				BOOST_FOREACH(int value, column.AsInts)
				{
					(void)column;
					v[rowNo++] = column.actualFromRaw(value);
				}

				makeFactor(v, column.labels());

				list[colNo++] = v;
			}
		}
		else // if (requestedType != Column::ColumnTypeScale)
		{
			bool ordinal = (requestedType == Column::ColumnTypeOrdinal);

			Rcpp::IntegerVector v(rowCount);

			if (columnType != Column::ColumnTypeScale)
			{
				BOOST_FOREACH(int value, column.AsInts)
				{
					(void)column;
					v[rowNo++] = value + 1;
				}

				makeFactor(v, column.labels(), ordinal);
			}
			else
			{
				// scale to nominal or ordinal (doesn't really make sense, but we have to do something)

				set<int> uniqueValues;

				BOOST_FOREACH(double value, column.AsDoubles)
				{
					(void)column;
					int intValue = (int)(value * 1000);
					uniqueValues.insert(intValue);
				}

				int index = 0;
				map<int, int> valueToIndex;
				vector<string> labels;

				BOOST_FOREACH(int value, uniqueValues)
				{
					(void)uniqueValues;
					valueToIndex[value] = index;

					stringstream ss;
					ss << ((double)value / 1000);
					labels.push_back(ss.str());

					index++;
				}

				BOOST_FOREACH(double value, column.AsDoubles)
				{
					(void)column;

					if (isnan(value))
						v[rowNo++] = INT_MIN;
					else
					{
						v[rowNo++] = valueToIndex[(int)(value * 1000)] + 1;
					}
				}

				makeFactor(v, labels, ordinal);
			}

			list[colNo++] = v;
		}
	}

	list.attr("names") = columnNames;

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame(list);

	return dataFrame;
}

Rcpp::DataFrame RcppBridge::readDataSetHeader(const std::map<string, Column::ColumnType> &columns)
{
	Rcpp::List list(columns.size());
	Rcpp::CharacterVector columnNames;

	int colNo = 0;

	typedef pair<const string, Column::ColumnType> ColumnInfo;

	BOOST_FOREACH(const ColumnInfo &columnInfo, columns)
	{
		(void)columns;
		string dot = ".";

		string columnName = columnInfo.first;

		string base64 = Base64::encode(dot, columnName);
		columnNames.push_back(base64);

		Column &column = _dataSet->columns().get(columnName);
		Column::ColumnType columnType = column.columnType();

		Column::ColumnType requestedType = columnInfo.second;
		if (requestedType == Column::ColumnTypeUnknown)
			requestedType = columnType;

		if (requestedType == Column::ColumnTypeScale)
		{
			if (columnType == Column::ColumnTypeScale)
			{
				list[colNo++] = Rcpp::NumericVector(0);
			}
			else if (columnType == Column::ColumnTypeOrdinal || columnType == Column::ColumnTypeNominal)
			{
				list[colNo++] = Rcpp::IntegerVector(0);
			}
			else
			{
				Rcpp::IntegerVector v(0);
				makeFactor(v, column.labels());
				list[colNo++] = v;
			}
		}
		else
		{
			bool ordinal = (requestedType == Column::ColumnTypeOrdinal);

			Rcpp::IntegerVector v(0);
			makeFactor(v, column.labels(), ordinal);

			list[colNo++] = v;
		}
	}

	list.attr("names") = columnNames;

	Rcpp::DataFrame dataFrame = Rcpp::DataFrame(list);

	return dataFrame;
}

void RcppBridge::makeFactor(Rcpp::IntegerVector &v, const Labels &levels, bool ordinal)
{
	Rcpp::CharacterVector labels;

	for (int i = 0; i < levels.size(); i++)
		labels.push_back(levels.at(i).text());

	v.attr("levels") = labels;

	vector<string> cla55;
	if (ordinal)
		cla55.push_back("ordered");
	cla55.push_back("factor");

	v.attr("class") = cla55;
}

void RcppBridge::makeFactor(Rcpp::IntegerVector &v, const std::vector<string> &levels, bool ordinal)
{
	v.attr("levels") = levels;

	vector<string> cla55;
	if (ordinal)
		cla55.push_back("ordered");
	cla55.push_back("factor");

	v.attr("class") = cla55;
}


int RcppBridge::callback(SEXP results)
{
	string jsonString = Rcpp::as<string>(results);

	Json::Reader r;
	Json::Value json;
	r.parse(jsonString, json);

	return _callback(json);
}

Rcpp::DataFrame RcppBridge::readDataSetStatic(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{	
	map<string, Column::ColumnType> columnsRequested = marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns);
	return _staticRef->readDataSet(columnsRequested);
}

Rcpp::DataFrame RcppBridge::readDataSetHeaderStatic(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	map<string, Column::ColumnType> columnsRequested = marshallSEXPs(columns, columnsAsNumeric, columnsAsOrdinal, columnsAsNominal, allColumns);
	return _staticRef->readDataSetHeader(columnsRequested);
}

std::map<string, Column::ColumnType> RcppBridge::marshallSEXPs(SEXP columns, SEXP columnsAsNumeric, SEXP columnsAsOrdinal, SEXP columnsAsNominal, SEXP allColumns)
{
	map<string, Column::ColumnType> columnsRequested;

	if (Rf_isLogical(allColumns) && Rcpp::as<bool>(allColumns))
	{
		(void)_staticRef;
		BOOST_FOREACH(const Column &column, _staticRef->_dataSet->columns())
			columnsRequested[column.name()] = Column::ColumnTypeUnknown;
	}

	if (Rf_isString(columns))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columns);
		for (int i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeUnknown;
	}

	if (Rf_isString(columnsAsNumeric))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsNumeric);
		for (int i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeScale;
	}

	if (Rf_isString(columnsAsOrdinal))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsOrdinal);
		for (int i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeOrdinal;
	}

	if (Rf_isString(columnsAsNominal))
	{
		vector<string> temp = Rcpp::as<vector<string> >(columnsAsNominal);
		for (int i = 0; i < temp.size(); i++)
			columnsRequested[temp.at(i)] = Column::ColumnTypeNominal;
	}

	return columnsRequested;
}

SEXP RcppBridge::callbackStatic(SEXP results)
{
	Rcpp::NumericVector control(1);
	control[0] = _staticRef->callback(results);
	return control;
}


